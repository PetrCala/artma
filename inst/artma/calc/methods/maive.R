MAIVE_MIN_VERSION <- "0.2.4"

#' @title Check whether the installed MAIVE version meets the minimum requirement
#' @param installed_version *[character]* Version string to check, e.g. from `packageVersion("MAIVE")`.
#' @param min_version *[character]* Minimum required version.
#' @return *[logical]* TRUE if `installed_version >= min_version`.
maive_version_ok <- function(installed_version, min_version = MAIVE_MIN_VERSION) {
  package_version(installed_version) >= package_version(min_version)
}

#' @title MAIVE wrapper
#' @description
#' Wrapper for the MAIVE (Meta-Analysis Instrumental Variable Estimator) method
#' from the CRAN package \pkg{MAIVE}. This estimator addresses publication
#' bias and p-hacking in meta-analysis using instrumental variables.
#' @param dat *[data.frame]* Data with columns: bs (estimates), sebs (std errors),
#'   Ns (sample sizes), studyid (study IDs, optional).
#' @param method *[integer]* Meta-analysis method: 1=PET, 2=PEESE, 3=PET-PEESE (default), 4=EK.
#' @param weight *[integer]* Weighting scheme: 0=none (default), 1=weights, 2=adjusted.
#' @param instrument *[integer]* Instrument SEs: 0=no, 1=yes (default).
#' @param studylevel *[integer]* Study-level correlation: 0=none, 1=fixed, 2=cluster (default).
#' @param SE *[integer]* SE estimation: 1=Asymptotic (default), 2=Pairs cluster boot, 3=Wild boot,
#'   4=Wild cluster boot, 5=Pairs boot.
#' @param AR *[integer]* Anderson-Rubin CI: 0=no (default), 1=yes.
#' @param first_stage *[integer]* First stage option (default 0).
#' @param seed *[integer]* RNG seed for bootstrap SE modes (SE = 2..5), so
#'   repeated runs on the same data reproduce identical results (default 123).
#' @return *[list]* MAIVE output with beta, SE, F-test, Hausman test, etc.
maive <- function(dat, method = 3L, weight = 0L, instrument = 1L, studylevel = 2L,
                  SE = 1L, AR = 0L, first_stage = 0L, seed = 123L) {
  box::use(
    artma / libs / core / validation[validate, assert]
  )

  validate(
    is.data.frame(dat),
    is.numeric(method),
    is.numeric(weight),
    is.numeric(instrument),
    is.numeric(studylevel),
    is.numeric(seed)
  )

  # Check if MAIVE package is available
  if (!requireNamespace("MAIVE", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg MAIVE} is required for MAIVE estimation.",
      "i" = "Install with: install.packages('MAIVE')"
    ))
  }

  installed_version <- as.character(utils::packageVersion("MAIVE"))
  if (!maive_version_ok(installed_version)) {
    cli::cli_abort(c(
      "Package {.pkg MAIVE} {MAIVE_MIN_VERSION} or higher is required for MAIVE estimation.",
      "i" = "Installed version: {installed_version}. Upgrade with: install.packages('MAIVE')"
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
      first_stage = as.integer(first_stage),
      seed = as.integer(seed)
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

box::export(maive, maive_version_ok)
