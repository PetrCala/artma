#' The external column-mapping hook. Opt-in and off by default: with
#' `data.mapping.external_command` unset, nothing in this module runs and the
#' mapping a run produces is byte-identical to what it produced before.
#'
#' When it is set and recognition declined a required role, artma serializes
#' what it knows about the dataset (column names, per-column summary
#' statistics, and the decline evidence) as JSON, runs the configured command
#' with that payload on stdin, and reads a proposed mapping back from stdout.
#' No raw data leaves the machine by artma's doing: the payload carries names
#' and summaries only. What the configured command does with it is the user's
#' choice.
#'
#' Proposals are never trusted. Each one runs through the same
#' value-plausibility and pair-consistency checks an auto-detected candidate
#' faces, and a proposal that fails is rejected with the failing check named.
#' Any failure of the command itself (nonzero exit, malformed output, timeout)
#' logs a warning and falls back to the ordinary decline; the hook can never
#' abort a run.
#'
#' Payload and response schemas: `contributingGuides/COLUMN_MAPPING_HOOK.md`.

box::use(
  artma / libs / core / utils[opt_or],
  artma / data / role_evidence[
    MIN_ROWS_FOR_EVIDENCE,
    PROVISIONAL_THRESHOLDS,
    coerce_numeric_column,
    profile_role_values,
    score_pair_consistency,
    score_role_evidence
  ]
)

#' Schema tags on the payload artma writes and on the response it reads back.
#' A command can key off them to stay compatible with several artma versions;
#' they change only on an incompatible change to either shape.
EXTERNAL_REQUEST_SCHEMA <- "artma.column_mapping_request/1"
EXTERNAL_RESPONSE_SCHEMA <- "artma.column_mapping_response/1"

#' Seconds the configured command may run before it is killed and the run
#' falls back to the ordinary decline.
DEFAULT_EXTERNAL_TIMEOUT <- 30

#' @title Read the configured external mapper command
#' @description Turns the `data.mapping.external_command` option into an
#'   executable and its arguments. A character vector is taken as already
#'   split; a single string is split on whitespace, honouring quotes. An unset,
#'   `NA`, or empty option means the hook is off.
#' @param command *\[character, optional\]* The configured command. Defaults to
#'   the option value.
#' @return *\[character|NULL\]* Executable followed by its arguments, or `NULL`
#'   when the hook is off.
external_mapper_command <- function(command = opt_or("artma.data.mapping.external_command")) {
  if (is.null(command) || !is.character(command)) {
    return(NULL)
  }
  command <- command[!is.na(command)]
  command <- command[nzchar(trimws(command))]
  if (length(command) == 0) {
    return(NULL)
  }
  if (length(command) > 1) {
    return(trimws(command))
  }
  parts <- tryCatch(
    scan(text = command, what = character(), quiet = TRUE),
    error = function(e) character(0)
  )
  if (length(parts) == 0) NULL else parts
}

#' @title Summarize one column for the external payload
#' @description Names and summary statistics only: how populated the column is,
#'   how many distinct values it holds, and, where the values are numeric-like,
#'   the quantiles and shape shares the role scorers themselves use. Never any
#'   raw values.
#' @param values *\[vector\]* Column values.
#' @return *\[list\]* The summary for one column.
summarize_column_values <- function(values) {
  non_missing <- !is.na(values)
  n_total <- length(values)

  summary <- list(
    class = class(values)[1],
    n = sum(non_missing),
    coverage = if (n_total == 0) 0 else sum(non_missing) / n_total,
    n_distinct = length(unique(values[non_missing])),
    numeric = FALSE
  )

  p <- profile_role_values(values)
  if (is.null(p)) {
    return(summary)
  }

  x <- coerce_numeric_column(values)
  x <- x[!is.na(x) & is.finite(x)]
  q <- unname(stats::quantile(x, c(0, 0.25, 0.5, 0.75, 1), names = FALSE))

  summary$numeric <- TRUE
  summary$quantiles <- list(min = q[1], q25 = q[2], median = q[3], q75 = q[4], max = q[5])
  summary$integer_share <- p$integer_share
  summary$negative_share <- p$negative_share
  summary$zero_share <- p$zero_share
  summary$uniqueness_ratio <- p$uniqueness_ratio
  summary$is_id_like <- p$is_id_like
  summary
}

#' @title Build the payload handed to the external mapper
#' @description The request the configured command reads on stdin: the roles
#'   artma could not map, what it already mapped, a summary of every column,
#'   and the machine-readable decline evidence behind each requested role.
#' @param df *\[data.frame\]* The data frame being mapped.
#' @param mapping *\[list\]* The mapping so far (std_col -> data_col).
#' @param declined *\[list\]* The `declined` attribute of `recognize_columns()`.
#' @param roles *\[character\]* Required roles left unmapped.
#' @return *\[list\]* The request, ready for `jsonlite::toJSON()`.
build_mapping_request <- function(df, mapping, declined, roles) {
  columns <- lapply(names(df), function(col) {
    c(list(name = col), summarize_column_values(df[[col]]))
  })

  list(
    schema = EXTERNAL_REQUEST_SCHEMA,
    dataset = list(n_rows = nrow(df), n_columns = ncol(df)),
    requested_roles = as.character(roles),
    mapped = mapping[vapply(mapping, function(v) is.character(v) && length(v) == 1, logical(1))],
    columns = columns,
    declined = if (is.list(declined)) declined[intersect(roles, names(declined))] else list()
  )
}

#' @title Run the configured command over a payload
#' @description Writes the payload to the command's stdin and reads its stdout.
#'   Every failure mode is a warning and a `NULL`: the caller then behaves as
#'   though the hook were off.
#' @param payload *\[character\]* The JSON request.
#' @param command *\[character\]* Executable followed by its arguments.
#' @param timeout *\[numeric\]* Seconds before the command is killed.
#' @return *\[character|NULL\]* The command's stdout as a single string, or
#'   `NULL` when it failed.
run_external_mapper <- function(payload, command, timeout = DEFAULT_EXTERNAL_TIMEOUT) {
  box::use(artma / libs / core / log[log_warn])

  err_file <- tempfile(fileext = ".err")
  on.exit(unlink(err_file), add = TRUE)

  out <- tryCatch(
    withCallingHandlers(
      system2(
        command = command[1],
        args = command[-1],
        input = payload,
        stdout = TRUE,
        stderr = err_file,
        timeout = timeout
      ),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) {
      log_warn("The external column mapper could not be run: {conditionMessage(e)}.")
      NULL
    }
  )
  if (is.null(out)) {
    return(NULL)
  }

  status <- attr(out, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    detail <- if (file.exists(err_file)) utils::head(readLines(err_file, warn = FALSE), 1L) else character(0)
    reason <- if (identical(as.integer(status), 124L)) {
      sprintf("it timed out after %s seconds", timeout)
    } else {
      sprintf("it exited with status %s", status)
    }
    # Both parts go in as interpolated values: whatever the command wrote to
    # stderr is data, and must never be read as cli markup.
    said <- if (length(detail) == 1 && nzchar(detail)) paste0(" It said: ", detail, ".") else ""
    log_warn("The external column mapper did not produce a mapping: {reason}.{said}")
    return(NULL)
  }

  text <- paste(out, collapse = "")
  if (!nzchar(trimws(text))) {
    log_warn("The external column mapper returned no output; keeping the declined roles unmapped.")
    return(NULL)
  }
  text
}

#' @title Parse the mapping a command proposed
#' @description Accepts either the documented response object (a `mappings`
#'   field) or a bare role-to-column object. A role may name its column
#'   directly or as an object with a `column` field.
#' @param text *\[character\]* The command's stdout.
#' @return *\[character|NULL\]* Named vector of role -> column, or `NULL` when
#'   the output could not be read as a mapping.
parse_mapper_response <- function(text) {
  box::use(artma / libs / core / log[log_warn])

  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) e
  )
  if (inherits(parsed, "error") || !is.list(parsed)) {
    log_warn("The external column mapper returned output that is not valid JSON; keeping the declined roles unmapped.")
    return(NULL)
  }

  proposals <- if (!is.null(parsed$mappings)) parsed$mappings else parsed
  if (!is.list(proposals) || length(proposals) == 0 || is.null(names(proposals))) {
    log_warn("The external column mapper returned JSON without any role-to-column mapping in it.")
    return(NULL)
  }

  columns <- vapply(proposals, function(entry) {
    value <- if (is.list(entry)) entry$column else entry
    if (is.character(value) && length(value) == 1 && !is.na(value) && nzchar(value)) value else NA_character_
  }, character(1))

  columns <- columns[!is.na(columns)]
  if (length(columns) == 0) {
    log_warn("The external column mapper named no usable column for any role.")
    return(NULL)
  }
  columns
}

#' @title Check a proposed mapping before it is used
#' @description The gate every external proposal passes: the column must exist
#'   and be free, its values must not contradict the role
#'   (`check_mapping_plausibility()`), and for the measured roles it must carry
#'   real value evidence and agree with the counterpart column already mapped,
#'   at the same thresholds the confirm-me layer applies to its own candidates.
#' @param df *\[data.frame\]* The data frame being mapped.
#' @param role *\[character\]* The standard column the proposal is for.
#' @param column *\[character\]* The proposed data column.
#' @param mapping *\[list\]* The mapping so far, used to find counterparts and
#'   columns already taken.
#' @return *\[list\]* With `ok` (logical) and `reason` (character, `NA` when
#'   `ok`).
verify_external_proposal <- function(df, role, column, mapping = list()) {
  box::use(artma / data / column_recognition[check_mapping_plausibility])

  reject <- function(reason) list(ok = FALSE, reason = reason)

  if (!column %in% names(df)) {
    return(reject(sprintf("the data has no column named '%s'", column)))
  }
  taken <- setdiff(unlist(mapping, use.names = FALSE), mapping[[role]])
  if (column %in% taken) {
    return(reject(sprintf("'%s' is already mapped to another role", column)))
  }

  plausible <- check_mapping_plausibility(df, role, column)
  if (!isTRUE(plausible$ok)) {
    return(reject(sprintf("value plausibility: %s", plausible$reason)))
  }

  # Below this point the checks need enough rows to judge a distribution. On a
  # tiny frame the plausibility check above is the whole gate, exactly as it is
  # for a mapping supplied through the config.
  if (nrow(df) < MIN_ROWS_FOR_EVIDENCE || !role %in% c("effect", "se")) {
    return(list(ok = TRUE, reason = NA_character_))
  }

  evidence <- score_role_evidence(df[[column]], role)
  if (!is.na(evidence) && evidence < PROVISIONAL_THRESHOLDS$min_evidence) {
    return(reject(sprintf(
      "value evidence: the values of '%s' carry only weak %s evidence (%.2f, %.2f required)",
      column, role, evidence, PROVISIONAL_THRESHOLDS$min_evidence
    )))
  }

  counterpart <- if (identical(role, "effect")) mapping[["se"]] else mapping[["effect"]]
  if (is.null(counterpart) || !counterpart %in% names(df)) {
    return(list(ok = TRUE, reason = NA_character_))
  }
  pair <- if (identical(role, "effect")) {
    score_pair_consistency(df[[column]], df[[counterpart]])
  } else {
    score_pair_consistency(df[[counterpart]], df[[column]])
  }
  if (!is.na(pair) && pair < PROVISIONAL_THRESHOLDS$min_pair) {
    return(reject(sprintf(
      "pair consistency: '%s' does not form a consistent (effect, se) pair with '%s' (%.2f, %.2f required)",
      column, counterpart, pair, PROVISIONAL_THRESHOLDS$min_pair
    )))
  }

  list(ok = TRUE, reason = NA_character_)
}

#' @title Describe a verified proposal the way the confirm-me layer does
#' @description Builds a `provisional`-shaped entry so a verified proposal can
#'   go through exactly the same confirmation prompt as a sub-threshold
#'   candidate recognition found itself.
#' @param df *\[data.frame\]* The data frame being mapped.
#' @param role *\[character\]* The standard column.
#' @param column *\[character\]* The proposed data column.
#' @param mapping *\[list\]* The mapping so far.
#' @return *\[list\]* One provisional entry, tagged `source = "external"`.
describe_external_proposal <- function(df, role, column, mapping = list()) {
  counterpart <- if (identical(role, "effect")) {
    mapping[["se"]]
  } else if (identical(role, "se")) {
    mapping[["effect"]]
  } else {
    NULL
  }
  pair <- if (is.null(counterpart) || !counterpart %in% names(df)) {
    NA_real_
  } else if (identical(role, "effect")) {
    score_pair_consistency(df[[column]], df[[counterpart]])
  } else {
    score_pair_consistency(df[[counterpart]], df[[column]])
  }

  p <- profile_role_values(df[[column]])

  list(
    kind = "unmapped",
    source = "external",
    role = role,
    column = column,
    score = NA_real_,
    evidence = score_role_evidence(df[[column]], role),
    name_score = NA_real_,
    pair_consistency = pair,
    pair_with = if (is.null(counterpart)) NA_character_ else counterpart,
    runner_up = NA_character_,
    margin = NA_real_,
    alternatives = character(0),
    summary = list(
      column = column,
      n = if (is.null(p)) NA_integer_ else p$n,
      coverage = if (is.null(p)) NA_real_ else p$coverage,
      n_distinct = if (is.null(p)) NA_integer_ else p$n_distinct,
      non_integer_share = if (is.null(p)) NA_real_ else p$non_integer_share
    ),
    alternative_summaries = list()
  )
}

#' @title Ask an external command to map the roles recognition declined
#' @description The whole hook in one call: off unless the option is set, one
#'   command run per dataset, every proposal verified before it is returned.
#'   Returns entries shaped like the `provisional` attribute of
#'   `recognize_columns()`, so the caller either confirms them with the user or
#'   applies them directly, exactly as it does for recognition's own
#'   sub-threshold candidates.
#' @param df *\[data.frame\]* The data frame being mapped.
#' @param mapping *\[list\]* The mapping so far (std_col -> data_col).
#' @param roles *\[character\]* Required roles left unmapped.
#' @param declined *\[list, optional\]* The `declined` attribute of the mapping.
#' @param command *\[character, optional\]* The configured command. Defaults to
#'   the `data.mapping.external_command` option.
#' @param timeout *\[numeric, optional\]* Seconds before the command is killed.
#'   Defaults to the `data.mapping.external_timeout` option.
#' @param run_fn *\[function, optional\]* Runner receiving
#'   `(payload, command, timeout)` and returning the command's stdout.
#'   Injectable for testing.
#' @return *\[list\]* Verified proposals keyed by role; empty when the hook is
#'   off, failed, or proposed nothing that verifies.
external_mapping_proposals <- function(
  df,
  mapping,
  roles,
  declined = attr(mapping, "declined"),
  command = external_mapper_command(),
  timeout = getOption("artma.data.mapping.external_timeout", DEFAULT_EXTERNAL_TIMEOUT),
  run_fn = run_external_mapper
) {
  box::use(artma / libs / core / log[log_debug, log_info, log_warn])

  if (!is.data.frame(df) || is.null(command)) {
    return(list())
  }
  roles <- as.character(roles)
  roles <- roles[!is.na(roles) & nzchar(roles)]
  if (length(roles) == 0) {
    return(list())
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    log_warn("The external column mapper needs the {.pkg jsonlite} package to talk to the configured command. Skipping it.")
    return(list())
  }

  request <- build_mapping_request(df, mapping, declined, roles)
  payload <- tryCatch(
    jsonlite::toJSON(request, auto_unbox = TRUE, null = "null", na = "null", digits = 6),
    error = function(e) NULL
  )
  if (is.null(payload)) {
    log_warn("Could not serialize the column-mapping request; skipping the external column mapper.")
    return(list())
  }

  log_info("Asking the configured external column mapper about {.field {roles}}.")
  log_debug("External column mapper command: {.code {paste(command, collapse = ' ')}}.")

  output <- tryCatch(
    run_fn(as.character(payload), command, timeout),
    error = function(e) {
      log_warn("The external column mapper could not be run: {e$message}.")
      NULL
    }
  )
  if (is.null(output) || !is.character(output)) {
    return(list())
  }

  proposals <- parse_mapper_response(paste(output, collapse = ""))
  if (is.null(proposals)) {
    return(list())
  }

  unsolicited <- setdiff(names(proposals), roles)
  if (length(unsolicited) > 0) {
    log_debug("Ignoring external proposals for {.field {unsolicited}}: {?it was/they were} not asked about.")
  }

  accepted <- list()
  working <- mapping
  for (role in intersect(roles, names(proposals))) {
    column <- unname(proposals[[role]])
    verdict <- verify_external_proposal(df, role, column, working)
    if (!isTRUE(verdict$ok)) {
      log_warn("Rejected the external proposal to map {.field {role}} to {.val {column}}: {verdict$reason}.")
      next
    }
    accepted[[role]] <- describe_external_proposal(df, role, column, working)
    working[[role]] <- column
    log_info("The external column mapper proposes {.val {column}} for {.field {role}}; it passed verification.")
  }

  accepted
}

box::export(
  DEFAULT_EXTERNAL_TIMEOUT,
  EXTERNAL_REQUEST_SCHEMA,
  EXTERNAL_RESPONSE_SCHEMA,
  build_mapping_request,
  describe_external_proposal,
  external_mapper_command,
  external_mapping_proposals,
  parse_mapper_response,
  run_external_mapper,
  summarize_column_values,
  verify_external_proposal
)
