#' @title Schema Drift Detection and Reconciliation
#' @description Detects changes between the user's dataset columns and the
#'   unified per-column store (`data.columns`), then guides the user through
#'   resolving those changes before the analysis pipeline runs. All drift is
#'   diffed against, and applied to, the single per-column store.

# -- Helpers --

#' @title Format confidence as percentage string
#' @keywords internal
fmt_pct <- function(score) {
  paste0(round(score * 100), "%")
}

#' @title Read the unified per-column store from options
#' @keywords internal
get_columns_store <- function() {
  store <- getOption("artma.data.columns", list())
  if (!is.list(store)) {
    return(list())
  }
  store
}

#' @title Check that a value is a single usable column name
#' @keywords internal
is_valid_colname <- function(x) {
  !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

#' @title Normalize expected schema columns
#' @keywords internal
normalize_expected_schema_cols <- function(cols) {
  if (is.null(cols)) {
    return(character(0))
  }

  if (is.list(cols)) {
    cols <- unlist(cols, use.names = FALSE)
  }

  cols <- as.character(cols)
  cols <- cols[!is.na(cols)]
  cols <- trimws(cols)
  cols <- cols[nzchar(cols)]

  if (length(cols) == 0L) {
    return(character(0))
  }

  unique(make.names(cols))
}

#' @title Persist expected schema columns
#' @keywords internal
persist_expected_schema_cols <- function(cols) {
  box::use(artma / libs / core / utils[get_verbosity])

  normalized <- normalize_expected_schema_cols(cols)
  options("artma.data.expected_schema_columns" = normalized)

  options_file_name <- getOption("artma.temp.file_name", NULL)
  options_dir <- getOption("artma.temp.dir_name", NULL)
  has_options_file <- !is.null(options_file_name) && !is.null(options_dir)

  if (!has_options_file) {
    return(invisible(normalized))
  }

  tryCatch(
    {
      suppressMessages(
        artma::options.modify(
          options_file_name = options_file_name,
          options_dir = options_dir,
          user_input = list("data.expected_schema_columns" = normalized),
          should_validate = FALSE
        )
      )
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(
          "Could not persist expected schema columns to options file: {e$message}"
        )
      }
    }
  )

  invisible(normalized)
}

#' @title Emit schema reconciliation completion message
#' @keywords internal
emit_reconcile_complete <- function() {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 3) {
    cli::cli_alert_success("Schema reconciliation complete.")
  }
}

# -- Core detection --

#' @title Detect schema drift
#' @description Compares the current dataframe columns against the unified
#'   per-column store to identify renames, removals, and additions.
#' @param raw_df *\[data.frame\]* Raw dataframe with original column names.
#' @param columns_store *\[list\]* The unified per-column store
#'   (from `artma.data.columns`): one record per column, keyed by the standard
#'   name for role columns and by the column's own name for moderators.
#' @return *\[list\]* Drift report with fields: `missing_roles` (named character
#'   vector, names = standard names, values = the stored source columns that
#'   vanished), `missing_moderators`, `added`, and `has_drift`.
#' @keywords internal
detect_schema_drift <- function(raw_df, columns_store) {
  box::use(
    artma / data / utils[get_required_colnames, get_standardized_colnames],
    artma / const[CONST]
  )

  df_cols <- make.names(colnames(raw_df))
  std_names <- get_standardized_colnames()
  required <- get_required_colnames()

  if (!is.list(columns_store)) columns_store <- list()
  store_keys <- names(columns_store)

  entry_is_computed <- function(entry) is.list(entry) && isTRUE(entry[["is_computed"]])

  # --- Role columns (standard names) ---
  # A role's stored source is its record's source_name; required roles with no
  # stored source default to the identity mapping (the standard name itself).
  role_sources <- list()
  for (std in std_names) {
    entry <- columns_store[[std]]
    if (entry_is_computed(entry)) next
    src <- if (is.list(entry)) entry[["source_name"]] else NULL
    if (is_valid_colname(src)) {
      role_sources[[std]] <- src
    } else if (std %in% required) {
      role_sources[[std]] <- std
    }
  }

  role_values <- vapply(role_sources, function(src) make.names(src), character(1))
  missing_role_std <- names(role_values)[!role_values %in% df_cols]
  missing_roles <- vapply(role_sources[missing_role_std], identity, character(1))

  # --- Moderator columns (non-role record keys) ---
  # Computed columns are added by the pipeline, not by the user's data, so they
  # will never be present in the raw df and must not be flagged as missing.
  moderator_keys <- store_keys[
    !store_keys %in% std_names &
      !vapply(columns_store, entry_is_computed, logical(1))
  ]
  moderator_keys_norm <- stats::setNames(make.names(moderator_keys), moderator_keys)
  missing_moderators <- names(moderator_keys_norm)[!moderator_keys_norm %in% df_cols]

  # --- Added columns (in df but not referenced by anything) ---
  # Standard names and computed columns count as referenced so that columns the
  # system already knows about do not appear as "new".
  referenced <- unique(make.names(c(
    unname(role_values),
    store_keys,
    std_names,
    CONST$DATA$COMPUTED_COLNAMES
  )))
  added <- df_cols[!df_cols %in% referenced]

  list(
    missing_roles = missing_roles, # named: std name -> stored source column
    missing_moderators = missing_moderators,
    added = added,
    has_drift = (
      length(missing_roles) > 0 ||
        length(missing_moderators) > 0 ||
        length(added) > 0
    )
  )
}

# -- Rename proposals (shared matching engine) --

#' @title Propose renames via the shared column-matching engine
#' @description For each missing column, finds the best candidate from the
#'   available (unmatched) columns using the recognition engine from
#'   `column_recognition.R`: string similarity plus, when the standard column
#'   is known, the pattern/value-analysis signal.
#' @param missing *\[character\]* Named vector: names identify the record (a
#'   standard name for roles, the column's own name for moderators), values are
#'   the stored column names that are now missing from the dataframe.
#' @param available_cols *\[character\]* Unmatched columns from the raw df.
#' @param raw_df *\[data.frame, optional\]* The raw dataframe, for value analysis.
#' @param roles_known *\[logical\]* Whether the names of `missing` are standard
#'   column names (enables the pattern signal).
#' @return *\[list\]* Named by the record key. Each element:
#'   `list(candidate, score)`.
#' @keywords internal
propose_renames <- function(missing, available_cols, raw_df = NULL, roles_known = FALSE) {
  box::use(
    artma / data / column_recognition[MATCH_THRESHOLDS, score_rename_candidate]
  )

  if (length(missing) == 0 || length(available_cols) == 0) {
    return(stats::setNames(list(), character(0)))
  }

  proposals <- list()

  for (i in seq_along(missing)) {
    record_key <- names(missing)[[i]]
    stored_name <- missing[[i]]
    std_name <- if (roles_known) record_key else NULL

    best_score <- 0
    best_candidate <- NA_character_

    for (cand in available_cols) {
      score <- score_rename_candidate(
        stored_name = stored_name,
        candidate = cand,
        std_name = std_name,
        df = raw_df
      )
      if (score > best_score) {
        best_score <- score
        best_candidate <- cand
      }
    }

    proposals[[record_key]] <- list(
      candidate = if (best_score >= MATCH_THRESHOLDS$rename_suggest) best_candidate else NA_character_,
      score = best_score
    )
  }

  proposals
}

# -- Display --

#' @title Show drift summary
#' @description Prints a unified diff of detected changes to the console.
#' @keywords internal
show_drift_summary <- function(drift, proposals_roles, proposals_moderators, role_sources) {
  cli::cli_rule(left = "artma detected dataset changes")

  # Role (standard) columns
  cli::cli_h3("Standard columns")
  for (std in names(role_sources)) {
    stored <- role_sources[[std]]
    if (std %in% names(drift$missing_roles)) {
      prop <- proposals_roles[[std]]
      if (!is.null(prop) && !is.na(prop$candidate)) {
        cli::cli_alert_warning(
          "{.val {stored}} {cli::symbol$arrow_right} NOT FOUND  (suggested: {.val {prop$candidate}} [{fmt_pct(prop$score)}])"
        )
      } else {
        cli::cli_alert_danger("{.val {stored}} {cli::symbol$arrow_right} NOT FOUND  (no suggestion)")
      }
    } else {
      cli::cli_alert_success("{.val {stored}} {cli::symbol$tick}")
    }
  }

  # Moderator columns
  if (length(drift$missing_moderators) > 0 || length(drift$added) > 0) {
    cli::cli_h3("Moderator columns")
    for (mod in drift$missing_moderators) {
      prop <- proposals_moderators[[mod]]
      if (!is.null(prop) && !is.na(prop$candidate)) {
        cli::cli_alert_warning(
          "{.val {mod}} {cli::symbol$arrow_right} NOT FOUND  (suggested: {.val {prop$candidate}} [{fmt_pct(prop$score)}])"
        )
      } else {
        cli::cli_alert_warning("{.val {mod}} {cli::symbol$arrow_right} NOT FOUND")
      }
    }
    for (col in drift$added) {
      cli::cli_alert_info("{.val {col}}  (new column)")
    }
  }

  cli::cli_rule()
}

# -- Decision collection --

#' @title Auto-resolve decisions
#' @description Applies defaults without prompting (for auto mode).
#' @keywords internal
auto_decisions <- function(drift, proposals_roles, proposals_moderators) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data / column_recognition[MATCH_THRESHOLDS]
  )

  renames <- list()
  drops <- character(0)
  remaps <- list()

  # Role columns: accept high-confidence proposals, abort if unresolvable
  for (std in names(drift$missing_roles)) {
    stored <- drift$missing_roles[[std]]
    prop <- proposals_roles[[std]]

    if (!is.null(prop) && !is.na(prop$candidate) && prop$score >= MATCH_THRESHOLDS$rename_auto) {
      renames[[std]] <- prop$candidate
      if (get_verbosity() >= 3) {
        cli::cli_alert_info(
          "Auto-mapped {.val {stored}} {cli::symbol$arrow_right} {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
        )
      }
    } else {
      candidate_msg <- if (!is.null(prop) && !is.na(prop$candidate)) {
        cli::format_inline(
          "Best candidate {.val {prop$candidate}} has confidence {fmt_pct(prop$score)} (below {fmt_pct(MATCH_THRESHOLDS$rename_auto)})."
        )
      } else {
        "No candidate found."
      }
      cli::cli_abort(c(
        "x" = "Cannot auto-resolve missing required column: {.val {stored}}",
        "i" = candidate_msg,
        "i" = "Use {.code reconcile = \"ask\"} to resolve this interactively."
      ))
    }
  }

  # Moderators: drop missing, remap if high confidence
  for (mod in drift$missing_moderators) {
    prop <- proposals_moderators[[mod]]

    if (!is.null(prop) && !is.na(prop$candidate) && prop$score >= MATCH_THRESHOLDS$rename_auto) {
      remaps[[mod]] <- prop$candidate
      if (get_verbosity() >= 3) {
        cli::cli_alert_info(
          "Auto-remapped moderator {.val {mod}} {cli::symbol$arrow_right} {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
        )
      }
    } else {
      drops <- c(drops, mod)
      if (get_verbosity() >= 3) {
        cli::cli_alert_warning("Dropped missing moderator {.val {mod}} from analysis configuration.")
      }
    }
  }

  list(renames = renames, drops = drops, remaps = remaps)
}

#' @title Ask for reconciliation decisions interactively
#' @description Shows menus for each drift item and collects user choices.
#' @keywords internal
ask_decisions <- function(drift, proposals_roles, proposals_moderators, raw_df) {
  renames <- list()
  drops <- character(0)
  remaps <- list()

  all_df_cols <- make.names(colnames(raw_df))

  # --- Role columns ---
  for (std in names(drift$missing_roles)) {
    stored <- drift$missing_roles[[std]]
    prop <- proposals_roles[[std]]

    has_proposal <- !is.null(prop) && !is.na(prop$candidate)

    if (has_proposal) {
      prompt_text <- cli::format_inline(
        "Required column {.val {stored}} is missing. Suggested rename: {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
      )
      choices <- c(
        cli::format_inline("Accept: use {.val {prop$candidate}}"),
        "Map to a different column",
        "Abort"
      )
    } else {
      prompt_text <- cli::format_inline(
        "Required column {.val {stored}} is missing. No rename suggestion found."
      )
      choices <- c("Map to a different column", "Abort")
    }

    choice <- climenu::select(choices = choices, prompt = prompt_text)

    if (is.null(choice) || grepl("^Abort", choice)) {
      cli::cli_abort("Reconciliation aborted by user.")
    }

    if (has_proposal && grepl("^Accept", choice)) {
      renames[[std]] <- prop$candidate
    } else {
      # Manual mapping via second menu
      available <- setdiff(all_df_cols, unlist(renames))
      manual_choice <- climenu::select(
        choices = available,
        prompt  = cli::format_inline("Select the column to use for {.val {std}}:")
      )
      if (is.null(manual_choice)) {
        cli::cli_abort("Reconciliation aborted by user.")
      }
      renames[[std]] <- manual_choice
    }
  }

  # --- Moderator columns ---
  for (mod in drift$missing_moderators) {
    prop <- proposals_moderators[[mod]]

    has_proposal <- !is.null(prop) && !is.na(prop$candidate)

    if (has_proposal) {
      prompt_text <- cli::format_inline(
        "Moderator {.val {mod}} no longer exists. Suggested rename: {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
      )
      choices <- c(
        "Drop from analysis (default)",
        cli::format_inline("Remap to {.val {prop$candidate}}"),
        "Map to a different column",
        "Abort"
      )
    } else {
      prompt_text <- cli::format_inline(
        "Moderator {.val {mod}} no longer exists in the dataset."
      )
      choices <- c(
        "Drop from analysis (default)",
        "Map to a different column",
        "Abort"
      )
    }

    choice <- climenu::select(choices = choices, prompt = prompt_text)

    if (is.null(choice) || grepl("^Abort", choice)) {
      cli::cli_abort("Reconciliation aborted by user.")
    }

    if (grepl("^Drop", choice)) {
      drops <- c(drops, mod)
    } else if (has_proposal && grepl("^Remap", choice)) {
      remaps[[mod]] <- prop$candidate
    } else {
      # Manual mapping
      available <- setdiff(all_df_cols, unlist(renames))
      manual_choice <- climenu::select(
        choices = available,
        prompt  = cli::format_inline("Select the column to remap {.val {mod}} to:")
      )
      if (is.null(manual_choice)) {
        cli::cli_abort("Reconciliation aborted by user.")
      }
      remaps[[mod]] <- manual_choice
    }
  }

  list(renames = renames, drops = drops, remaps = remaps)
}

# -- Confirmation and summary --

#' @title Show reconciliation outcome summary and ask for confirmation
#' @keywords internal
confirm_decisions <- function(decisions, drift, role_sources) {
  cli::cli_rule(left = "Configuration update summary")

  # Role renames
  for (std in names(decisions$renames)) {
    old_raw <- role_sources[[std]]
    new_raw <- decisions$renames[[std]]
    cli::cli_alert_success("Mapped: {.val {old_raw}} {cli::symbol$arrow_right} {.val {new_raw}}")
  }

  # Moderator drops
  for (mod in decisions$drops) {
    cli::cli_alert_warning("Dropped from analysis: {.val {mod}}")
  }

  # Moderator remaps
  for (old_mod in names(decisions$remaps)) {
    new_mod <- decisions$remaps[[old_mod]]
    cli::cli_alert_success("Remapped moderator: {.val {old_mod}} {cli::symbol$arrow_right} {.val {new_mod}}")
  }

  # Added (informational only)
  if (length(drift$added) > 0) {
    cli::cli_alert_info(
      "New column{?s} detected (will be available in summary stats): {.val {drift$added}}"
    )
  }

  # Unchanged role columns
  unchanged <- setdiff(names(role_sources), names(drift$missing_roles))
  if (length(unchanged) > 0) {
    unchanged_raw <- unlist(role_sources[unchanged], use.names = FALSE)
    cli::cli_alert_success("Unchanged: {.val {unchanged_raw}}")
  }

  cli::cli_rule()

  choice <- climenu::select(
    choices = c("Save changes and continue analysis", "Abort"),
    prompt  = "Apply these changes to your configuration file?"
  )

  if (is.null(choice) || choice == "Abort") {
    cli::cli_abort("Reconciliation aborted by user.")
  }

  invisible(NULL)
}

# -- Application --

#' @title Apply reconciliation decisions
#' @description Applies all decisions to the unified per-column store and
#'   persists it through the single write path.
#' @keywords internal
apply_reconciliation <- function(decisions) {
  box::use(artma / data_config / write[write_unified_columns])

  store <- get_columns_store()

  # 1. Role renames: update the record's source_name
  for (std in names(decisions$renames)) {
    entry <- store[[std]]
    if (!is.list(entry)) entry <- list()
    entry$source_name <- decisions$renames[[std]]
    store[[std]] <- entry
  }

  # 2. Moderator drops: remove the record
  for (mod in decisions$drops) {
    store[[make.names(mod)]] <- NULL
  }

  # 3. Moderator remaps: move the record to the new key
  for (old_mod in names(decisions$remaps)) {
    new_mod <- decisions$remaps[[old_mod]]
    old_key <- make.names(old_mod)
    new_key <- make.names(new_mod)

    entry <- store[[old_key]]
    if (!is.list(entry)) entry <- list()
    entry$var_name <- new_mod
    store[[new_key]] <- entry
    if (!identical(old_key, new_key)) {
      store[[old_key]] <- NULL
    }
  }

  write_unified_columns(store)

  invisible(NULL)
}

# -- Main entry point --

#' @title Reconcile schema drift
#' @description Detects changes between the current dataset columns and the
#'   unified per-column store, then resolves them before the analysis pipeline
#'   runs. Should be called with the raw (un-standardized) dataframe.
#' @param raw_df *\[data.frame\]* Raw dataframe with original column names.
#' @param mode *\[character\]* One of `"ask"`, `"auto"`, or `"strict"`. If `NULL`,
#'   reads from `artma.data.reconcile_mode` option (default: `"ask"`).
#' @return `NULL` invisibly. Side effects: updates options file and in-memory
#'   state if drift is detected and resolved.
#' @keywords internal
reconcile_schema <- function(raw_df, mode = NULL) {
  box::use(
    artma / libs / core / autonomy[should_prompt_user]
  )

  mode <- mode %||% getOption("artma.data.reconcile_mode", "ask")
  current_schema_cols <- unique(make.names(colnames(raw_df)))
  expected_schema_cols <- normalize_expected_schema_cols(
    getOption("artma.data.expected_schema_columns", NA_character_)
  )

  # Initialize baseline schema on first run. Until this baseline exists, drift
  # details are suppressed regardless of reconcile mode.
  if (length(expected_schema_cols) == 0L) {
    persist_expected_schema_cols(current_schema_cols)
    emit_reconcile_complete()
    return(invisible(NULL))
  }

  columns_store <- get_columns_store()

  # Detect drift
  drift <- detect_schema_drift(raw_df, columns_store)

  # "Added" columns should only include columns that are new relative to the
  # stored baseline schema. Baseline columns that are simply not mapped should
  # not be treated as drift on every run.
  drift$added <- setdiff(drift$added, expected_schema_cols)
  drift$has_drift <- (
    length(drift$missing_roles) > 0 ||
      length(drift$missing_moderators) > 0 ||
      length(drift$added) > 0
  )

  if (!drift$has_drift) {
    emit_reconcile_complete()
    return(invisible(NULL))
  }

  # Strict mode: abort with a structured message
  if (mode == "strict") {
    msgs <- c("x" = "Dataset schema does not match the stored configuration.")
    if (length(drift$missing_roles) > 0) {
      msgs <- c(msgs, "i" = cli::format_inline(
        "Missing required column{?s}: {.val {unname(drift$missing_roles)}}"
      ))
    }
    if (length(drift$missing_moderators) > 0) {
      msgs <- c(msgs, "i" = cli::format_inline(
        "Missing moderator{?s}: {.val {drift$missing_moderators}}"
      ))
    }
    if (length(drift$added) > 0) {
      msgs <- c(msgs, "i" = cli::format_inline(
        "New column{?s} not in config: {.val {drift$added}}"
      ))
    }
    msgs <- c(msgs,
      "i" = "Use {.code reconcile = \"ask\"} for interactive resolution.",
      "i" = "Use {.code reconcile = \"auto\"} for automatic resolution."
    )
    cli::cli_abort(msgs)
  }

  # Stored role sources (for display) and "unmatched" columns available for
  # fuzzy matching: everything the store does not already account for.
  box::use(
    artma / data / utils[get_colnames_map, get_required_colnames, get_standardized_colnames]
  )
  role_sources <- as.list(drift$missing_roles)
  matched_role_sources <- character(0)
  full_map <- get_colnames_map()
  # Required roles with no explicit record are tracked as identity mappings
  for (std in setdiff(get_required_colnames(), names(full_map))) {
    full_map[[std]] <- std
  }
  for (std in names(full_map)) {
    if (!std %in% names(drift$missing_roles)) {
      role_sources[[std]] <- full_map[[std]]
      matched_role_sources <- c(matched_role_sources, full_map[[std]])
    }
  }

  moderator_keys <- setdiff(names(columns_store), get_standardized_colnames())
  matched_cols <- make.names(c(matched_role_sources, moderator_keys))
  matched_cols <- setdiff(matched_cols, make.names(unname(drift$missing_roles)))
  available_cols <- setdiff(make.names(colnames(raw_df)), matched_cols)

  # Rename proposals via the shared matching engine
  proposals_roles <- propose_renames(
    drift$missing_roles, available_cols,
    raw_df = raw_df, roles_known = TRUE
  )
  proposals_moderators <- propose_renames(
    stats::setNames(drift$missing_moderators, drift$missing_moderators),
    available_cols,
    raw_df = raw_df
  )

  # Show unified diff
  show_drift_summary(drift, proposals_roles, proposals_moderators, role_sources)

  # Collect decisions
  do_prompt <- (mode == "ask") && should_prompt_user(required_level = "autonomous")

  if (mode == "auto" || !do_prompt) {
    decisions <- auto_decisions(drift, proposals_roles, proposals_moderators)
  } else {
    decisions <- ask_decisions(drift, proposals_roles, proposals_moderators, raw_df)
    confirm_decisions(decisions, drift, role_sources)
  }

  # Apply through the single write path
  apply_reconciliation(decisions)
  persist_expected_schema_cols(current_schema_cols)

  emit_reconcile_complete()

  invisible(NULL)
}

box::export(reconcile_schema, detect_schema_drift)
