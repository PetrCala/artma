#' @title MAIVE wrapper
#' @description
#' Wrapper for the MAIVE (Meta-Analysis Instrumental Variable Estimator) method
#' from the PetrCala/maive GitHub package. This estimator addresses publication
#' bias and p-hacking in meta-analysis using instrumental variables.
#' @param dat *[data.frame]* Data with columns: bs (estimates), sebs (std errors),
#'   Ns (sample sizes), studyid (study IDs, optional).
#' @param method *[integer]* Meta-analysis method: 1=PET, 2=PEESE, 3=PET-PEESE (default), 4=EK.
#' @param weight *[integer]* Weighting scheme: 0=none (default), 1=weights, 2=adjusted.
#' @param instrument *[integer]* Instrument SEs: 0=no, 1=yes (default).
#' @param studylevel *[integer]* Study-level correlation: 0=none, 1=fixed, 2=cluster (default).
#' @param SE *[integer]* SE estimation: 1=Asymptotic, 2=Pairs cluster boot, 3=Wild boot (default),
#'   4=Wild cluster boot, 5=Pairs boot.
#' @param AR *[integer]* Anderson-Rubin CI: 0=no, 1=yes (default).
#' @param first_stage *[integer]* First stage option (default 0).
#' @return *[list]* MAIVE output with beta, SE, F-test, Hausman test, etc.
maive <- function(dat, method = 3L, weight = 0L, instrument = 1L, studylevel = 2L,
                  SE = 3L, AR = 1L, first_stage = 0L) {
  box::use(
    artma / libs / validation[validate, assert]
  )

  validate(
    is.data.frame(dat),
    is.numeric(method),
    is.numeric(weight),
    is.numeric(instrument),
    is.numeric(studylevel)
  )

  # Check if MAIVE package is available
  if (!requireNamespace("MAIVE", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg MAIVE} is required for MAIVE estimation.",
      "i" = "Install with: devtools::install_github('PetrCala/maive@0.0.2.11')"
    ))
  }

  # Validate data columns
  required_cols <- c("bs", "sebs", "Ns")
  missing_cols <- setdiff(required_cols, colnames(dat))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required columns for MAIVE: {.field {missing_cols}}")
  }

  # Call the MAIVE package function
  result <- tryCatch(
    MAIVE::maive(
      dat = dat,
      method = as.integer(method),
      weight = as.integer(weight),
      instrument = as.integer(instrument),
      studylevel = as.integer(studylevel),
      SE = as.integer(SE),
      AR = as.integer(AR),
      first_stage = as.integer(first_stage)
    ),
    error = function(e) {
      cli::cli_abort(c(
        "MAIVE estimation failed: {e$message}",
        "i" = "Check that your data meets MAIVE requirements"
      ))
    }
  )

  result
}

box::export(maive)
