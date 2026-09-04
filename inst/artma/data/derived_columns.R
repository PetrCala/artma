#' @title User-defined derived columns
#' @description The data config maps source columns onto roles; it cannot build
#'   new ones. Published meta-analyses routinely need two constructions the
#'   mapping alone cannot express: interactions (`SE x Top journal`) and
#'   indicator columns cut out of a categorical variable (`preferred` and
#'   `discounted` from an `estimate_category` column). Both used to have to be
#'   pre-computed into the input file.
#'
#'   `data.derived` takes named R expressions and evaluates them against the
#'   prepared frame at the end of the compute phase, after the standard
#'   computed columns exist and after winsorization, so an interaction with
#'   `se` uses the same winsorized `se` the rest of the analysis does. The
#'   results then join the normal per-column config (and so can carry `bma`
#'   flags) via the persist phase.

box::use(
  artma / libs / core / validation[assert]
)

#' @title Read the configured derived-column specifications
#' @description Normalizes `artma.data.derived` into a named character vector
#'   of expressions, in the order the user wrote them. An unset option, `NA`,
#'   or an empty list yields an empty vector.
#' @return *\[character\]* Named expressions, possibly empty.
#' @keywords internal
get_derived_specs <- function() {
  empty <- stats::setNames(character(0), character(0))
  specs <- getOption("artma.data.derived", NULL)

  if (is.null(specs)) {
    return(empty)
  }
  # The template default is the `.na` sentinel, which reaches the option store
  # as a bare `NA` rather than an empty list.
  if (!is.list(specs) && length(specs) == 1L && is.na(specs)) {
    return(empty)
  }
  if (is.list(specs)) {
    specs <- specs[!vapply(specs, function(x) is.null(x), logical(1))]
    if (length(specs) == 0) {
      return(empty)
    }
    assert(
      all(vapply(specs, function(x) is.character(x) && length(x) == 1L, logical(1))),
      "Every entry of `data.derived` must be a single R expression given as a string."
    )
    specs <- unlist(specs, use.names = TRUE)
  }

  assert(
    is.character(specs),
    "The `data.derived` option must be a named list (or named character vector) of R expressions."
  )

  specs <- specs[!is.na(specs)]
  if (length(specs) == 0) {
    return(empty)
  }

  names_given <- names(specs)
  assert(
    !is.null(names_given) && all(nzchar(trimws(names_given))),
    "Every `data.derived` entry must be named; the name becomes the new column's name."
  )

  names(specs) <- trimws(names_given)
  specs
}

#' @title Names of the columns `data.derived` adds
#' @description Used by the persist phase to register the derived columns in
#'   the data config alongside the pipeline's own computed columns.
#' @return *\[character\]* The configured derived column names, possibly empty.
derived_column_names <- function() {
  names(get_derived_specs())
}

#' @title Validate a derived column's name
#' @description The name becomes a column of the analysis frame, so it must be
#'   a syntactic name, must not repeat, must not shadow a standard role
#'   (`effect`, `se`, `t_stat`, ...), and must not overwrite a column already
#'   in the frame. Silently replacing `effect` with an expression would be the
#'   worst possible failure mode here, so a collision aborts.
#' @param name *\[character\]* The proposed column name.
#' @param df *\[data.frame\]* The frame the column would be added to.
#' @param taken *\[character\]* Names already claimed by earlier entries.
#' @return `NULL`, invisibly.
#' @keywords internal
validate_derived_name <- function(name, df, taken) {
  box::use(artma / data / utils[get_reserved_colnames])

  if (!identical(make.names(name), name)) {
    cli::cli_abort(c(
      "x" = "{.val {name}} is not a valid column name for a {.field data.derived} entry.",
      "i" = "Use a syntactic R name, e.g. {.val {make.names(name)}}."
    ))
  }

  if (name %in% taken) {
    cli::cli_abort("The {.field data.derived} entry {.val {name}} is defined more than once.")
  }

  if (name %in% get_reserved_colnames()) {
    cli::cli_abort(c(
      "x" = "The {.field data.derived} entry {.val {name}} would shadow a standard column.",
      "i" = "Standard columns are mapped through {.code artma::config_set()}, not derived. Pick another name."
    ))
  }

  if (name %in% colnames(df)) {
    cli::cli_abort(c(
      "x" = "The {.field data.derived} entry {.val {name}} would overwrite an existing column.",
      "i" = "Derived columns are added, never substituted. Pick a name the dataset does not already use."
    ))
  }

  invisible(NULL)
}

#' @title Coerce a derived expression's value into a column
#' @description Accepts an atomic vector of length 1 (recycled) or `nrow(df)`.
#'   Logical values become 0/1 integers, so the natural way of writing an
#'   indicator (`estimate_category == "preferred"`) yields the dummy the
#'   moderator set expects rather than a `TRUE`/`FALSE` column.
#' @param value *\[any\]* The evaluated expression.
#' @param name *\[character\]* The derived column name, for error messages.
#' @param n_rows *\[integer\]* Rows in the frame.
#' @return *\[vector\]* The column values.
#' @keywords internal
coerce_derived_value <- function(value, name, n_rows) {
  if (is.factor(value)) {
    value <- as.character(value)
  }

  if (!is.atomic(value) || is.null(value)) {
    cli::cli_abort(c(
      "x" = "The {.field data.derived} entry {.val {name}} must evaluate to an atomic vector.",
      "i" = "Got {.cls {class(value)}}."
    ))
  }

  if (length(value) == 1L) {
    value <- rep(value, n_rows)
  }

  if (length(value) != n_rows) {
    cli::cli_abort(c(
      "x" = "The {.field data.derived} entry {.val {name}} must evaluate to {n_rows} value{?s}, one per row.",
      "i" = "Got {length(value)}."
    ))
  }

  if (is.logical(value)) {
    value <- as.integer(value)
  }

  value
}

#' @title Reject derived columns that make the moderator set degenerate
#' @description An expression is free to reference `effect`, `se`, `t_stat` or
#'   `precision`, and most such references are legitimate (an interaction of
#'   the standard error with a study characteristic is the whole point). What
#'   is not legitimate is a column that is itself a near-deterministic
#'   encoding of the response: BMA would then regress the effect on a
#'   transform of itself, hiding every genuine moderator. This is the same
#'   check `bma` already applies to helper columns shipped in the data file
#'   (`detect_derived_effect_encodings()`), applied here at the point the
#'   column is created so the run stops with the offending expression in hand.
#'
#'   Entries whose data config says `bma_allow_derived: true` (or `bma: false`)
#'   are exempt, matching the escape hatch documented for file-provided
#'   columns.
#' @param df *\[data.frame\]* The frame with the derived columns already added.
#' @param derived_names *\[character\]* The derived column names.
#' @param specs *\[character\]* The expressions, named by column.
#' @return `NULL`, invisibly.
#' @keywords internal
reject_degenerate_derived <- function(df, derived_names, specs) {
  box::use(artma / variable / bma[flag_derived_bma_candidates])

  if (length(derived_names) == 0) {
    return(invisible(NULL))
  }

  config <- getOption("artma.data.columns", list())
  if (!is.list(config)) config <- list()

  flagged <- flag_derived_bma_candidates(df, derived_names, config = config)
  if (!nrow(flagged)) {
    return(invisible(NULL))
  }

  opted_out <- vapply(flagged$var_name, function(v) {
    identical(config[[make.names(v)]]$bma, FALSE)
  }, logical(1))
  flagged <- flagged[!opted_out, , drop = FALSE]
  if (!nrow(flagged)) {
    return(invisible(NULL))
  }

  details <- sprintf(
    "%s = %s: |cor| = %.3f with %s",
    flagged$var_name, specs[flagged$var_name], flagged$correlation, flagged$target
  )

  cli::cli_abort(c(
    "x" = "{nrow(flagged)} {.field data.derived} column{?s} {?is/are} a derived encoding of the effect or its standard error.",
    stats::setNames(details, rep("*", length(details))),
    "i" = "Regressing the effect on an encoding of itself hides every genuine moderator. Rewrite the expression, or keep it by setting {.code bma_allow_derived: true} (or {.code bma: false}) on {cli::qty(nrow(flagged))}{?its/their} data config entry."
  ))
}

#' @title Apply the user-defined derived columns
#' @description Evaluates every `data.derived` expression against the frame, in
#'   the order written, and appends the results. Each expression sees the
#'   frame's columns plus every derived column defined before it, so
#'   constructions can build on one another. A no-op when the option is unset.
#' @param df *\[data.frame\]* The prepared data frame.
#' @return *\[data.frame\]* The frame with the derived columns appended.
apply_derived_columns <- function(df) {
  box::use(artma / libs / core / log[log_info])

  specs <- get_derived_specs()
  if (length(specs) == 0) {
    return(df)
  }

  enclos <- parent.frame()
  added <- character(0)

  for (name in names(specs)) {
    expr_text <- specs[[name]]
    validate_derived_name(name, df, added)

    parsed <- tryCatch(str2lang(expr_text), error = function(err) NULL)
    if (is.null(parsed)) {
      cli::cli_abort(c(
        "x" = "Could not parse the {.field data.derived} expression for {.val {name}}.",
        "i" = "The expression was {.val {expr_text}}."
      ))
    }

    value <- tryCatch(
      eval(parsed, envir = df, enclos = enclos),
      error = function(err) {
        cli::cli_abort(c(
          "x" = "Failed to evaluate the {.field data.derived} expression for {.val {name}}.",
          "i" = "The expression was {.val {expr_text}}.",
          "i" = conditionMessage(err)
        ))
      }
    )

    df[[name]] <- coerce_derived_value(value, name, nrow(df))
    added <- c(added, name)
  }

  reject_degenerate_derived(df, added, specs)

  log_info("Added {length(added)} derived column{?s}: {.field {added}}.")

  df
}

box::export(
  apply_derived_columns,
  coerce_derived_value,
  derived_column_names,
  get_derived_specs,
  reject_degenerate_derived,
  validate_derived_name
)
