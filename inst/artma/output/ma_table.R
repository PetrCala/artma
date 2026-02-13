#' @title Model Averaging Table
#' @description
#' Builds and displays a unified model averaging table combining BMA and FMA
#' results. When both methods have run, the table shows BMA (P.Mean, SD, PIP)
#' and FMA (Coef, SE, p-val) columns side by side. When only one method has
#' run, only its columns are shown.
NULL

#' Build a unified model averaging table
#'
#' @param bma_coefficients *\[data.frame, optional\]* BMA coefficients with columns:
#'   variable, pip, post_mean, post_sd.
#' @param fma_coefficients *\[data.frame, optional\]* FMA coefficients with columns:
#'   variable, coefficient, se, p_value.
#' @param round_to *\[integer\]* Number of decimal places for rounding. Defaults to 3.
#' @return *\[data.frame\]* Combined MA table, or NULL if no valid inputs.
#' @export
build_ma_table <- function(bma_coefficients = NULL, fma_coefficients = NULL, round_to = 3L) {
  box::use(artma / libs / core / validation[validate])

  has_bma <- !is.null(bma_coefficients) && is.data.frame(bma_coefficients) && nrow(bma_coefficients) > 0
  has_fma <- !is.null(fma_coefficients) && is.data.frame(fma_coefficients) && nrow(fma_coefficients) > 0

  if (!has_bma && !has_fma) return(NULL)

  validate(is.numeric(round_to))

  # Normalize intercept naming across methods
  normalize_intercept <- function(names) {
    gsub("^\\(Intercept\\)$", "Intercept", names)
  }

  # Build the union of variable names, preserving order from whichever ran first
  all_vars <- character(0)
  if (has_bma) {
    bma_coefficients$variable <- normalize_intercept(bma_coefficients$variable)
    all_vars <- bma_coefficients$variable
  }
  if (has_fma) {
    fma_coefficients$variable <- normalize_intercept(fma_coefficients$variable)
    new_vars <- setdiff(fma_coefficients$variable, all_vars)
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
    bma_idx <- match(all_vars, bma_coefficients$variable)
    result[["BMA P.Mean"]] <- round(bma_coefficients$post_mean[bma_idx], round_to)
    result[["BMA SD"]] <- round(bma_coefficients$post_sd[bma_idx], round_to)
    result[["BMA PIP"]] <- round(bma_coefficients$pip[bma_idx], round_to)
  }

  # Add FMA columns
  if (has_fma) {
    fma_idx <- match(all_vars, fma_coefficients$variable)
    result[["FMA Coef"]] <- round(fma_coefficients$coefficient[fma_idx], round_to)
    result[["FMA SE"]] <- round(fma_coefficients$se[fma_idx], round_to)
    result[["FMA p-val"]] <- round(fma_coefficients$p_value[fma_idx], round_to)
  }

  result
}

#' Display the MA table using cli formatting
#'
#' @param ma_table *\[data.frame\]* The MA table from build_ma_table().
#' @param verbosity *\[integer\]* Current verbosity level.
#' @export
display_ma_table <- function(ma_table, verbosity = 3L) {
  if (is.null(ma_table) || nrow(ma_table) == 0) return(invisible(NULL))

  if (verbosity >= 3) {
    cli::cli_h2("Model Averaging Results")
  }

  if (verbosity >= 1) {
    lines <- utils::capture.output(
      print(ma_table, row.names = FALSE) # nolint: undesirable_function_linter.
    )
    cli::cli_verbatim(lines)
  }

  invisible(ma_table)
}

box::export(build_ma_table, display_ma_table)
