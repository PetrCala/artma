#' @title Model Averaging Table
#' @description
#' Builds and displays a unified model averaging table combining BMA and FMA
#' results. When both methods have run, the table shows BMA (P.Mean, SD, PIP)
#' and FMA (Coef, SE, p-val) columns side by side. When only one method has
#' run, only its columns are shown. Both are read from the methods' `estimates`
#' frames, so the numbers are rounded here and nowhere upstream.
NULL

#' Build a unified model averaging table
#'
#' @description
#' Both inputs are `estimates` frames in the shared schema (see
#' `new_estimates()`), one row per moderator: `term` is the variable, and the
#' displayed numbers come from `estimate`, `std_error`, and, per method,
#' `statistic` (the BMA posterior inclusion probability) or `p_value` (the FMA
#' Wald p-value). Reading the unrounded frames rather than the display tables
#' keeps this table the only place the rounding happens.
#'
#' @param bma_estimates *\[data.frame, optional\]* The BMA estimates frame.
#' @param fma_estimates *\[data.frame, optional\]* The FMA estimates frame.
#' @param round_to *\[integer\]* Number of decimal places for rounding. Defaults to 3.
#' @return *\[data.frame\]* Combined MA table, or NULL if no valid inputs.
#' @export
build_ma_table <- function(bma_estimates = NULL, fma_estimates = NULL, round_to = 3L) {
  box::use(artma / libs / core / validation[validate])

  has_bma <- !is.null(bma_estimates) && is.data.frame(bma_estimates) && nrow(bma_estimates) > 0
  has_fma <- !is.null(fma_estimates) && is.data.frame(fma_estimates) && nrow(fma_estimates) > 0

  if (!has_bma && !has_fma) {
    return(NULL)
  }

  validate(is.numeric(round_to))

  # Normalize intercept naming across methods
  normalize_intercept <- function(names) {
    gsub("^\\(Intercept\\)$", "Intercept", names)
  }

  # Build the union of variable names, preserving order from whichever ran first
  all_vars <- character(0)
  if (has_bma) {
    bma_estimates$term <- normalize_intercept(bma_estimates$term)
    all_vars <- bma_estimates$term
  }
  if (has_fma) {
    fma_estimates$term <- normalize_intercept(fma_estimates$term)
    new_vars <- setdiff(fma_estimates$term, all_vars)
    all_vars <- c(all_vars, new_vars)
  }

  # Move Intercept to the top
  intercept_idx <- which(all_vars == "Intercept")
  if (length(intercept_idx) > 0) {
    all_vars <- c("Intercept", all_vars[-intercept_idx])
  }

  # Initialize result with variable names as row labels
  result <- data.frame(Variable = all_vars, stringsAsFactors = FALSE)

  # Add BMA columns
  if (has_bma) {
    bma_idx <- match(all_vars, bma_estimates$term)
    result[["BMA P.Mean"]] <- round(bma_estimates$estimate[bma_idx], round_to)
    result[["BMA SD"]] <- round(bma_estimates$std_error[bma_idx], round_to)
    result[["BMA PIP"]] <- round(bma_estimates$statistic[bma_idx], round_to)
  }

  # Add FMA columns
  if (has_fma) {
    fma_idx <- match(all_vars, fma_estimates$term)
    result[["FMA Coef"]] <- round(fma_estimates$estimate[fma_idx], round_to)
    result[["FMA SE"]] <- round(fma_estimates$std_error[fma_idx], round_to)
    result[["FMA p-val"]] <- round(fma_estimates$p_value[fma_idx], round_to)
  }

  result
}

#' Display the MA table using cli formatting
#'
#' @param ma_table *\[data.frame\]* The MA table from build_ma_table().
#' @param verbosity *\[integer\]* Current verbosity level.
#' @export
display_ma_table <- function(ma_table, verbosity = 3L) {
  box::use(artma / libs / formatting / results[capture_print_wide])

  if (is.null(ma_table) || nrow(ma_table) == 0) {
    return(invisible(NULL))
  }

  if (verbosity >= 3) {
    cli::cli_h2("Model Averaging Results")
  }

  if (verbosity >= 1) {
    lines <- capture_print_wide(ma_table, row.names = FALSE)
    cli::cli_verbatim(lines)
  }

  invisible(ma_table)
}

box::export(build_ma_table, display_ma_table)
