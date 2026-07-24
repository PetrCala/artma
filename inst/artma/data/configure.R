#' @title Resolve the missing-value handling strategy
#' @description Configure-phase decision that fixes `artma.data.na_handling`
#'   before the cached compute phase runs. When the option is already set it is
#'   a no-op. Otherwise, if the (already cleaned) data frame has missing values
#'   in optional columns, it prompts the user interactively (and offers to save
#'   the choice) or falls back to the deterministic `"stop"` strategy in a
#'   non-interactive session. This is intentionally separate from
#'   `handle_missing_values()` (the pure compute-phase step) so that no prompt
#'   or option write ever happens inside the cached pipeline.
#' @param df *\[data.frame\]* The cleaned data frame used to detect optional
#'   missing values (as produced by `clean_data()`).
#' @return `NULL`, invisibly.
#' @keywords internal
resolve_na_handling <- function(df) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data / na_handling[detect_missing_values],
    artma / options / prompts[prompt_na_handling],
    artma / interactive / save_preference[prompt_save_preference]
  )

  na_handling_option <- getOption("artma.data.na_handling", default = NA)

  # Strategy already configured: nothing to decide.
  if (!is.na(na_handling_option)) {
    return(invisible(NULL))
  }

  na_summary <- detect_missing_values(df)

  # Without optional missing values there is no decision to make; the compute
  # phase falls back to the deterministic "stop" default when it runs.
  if (!na_summary$has_optional_na) {
    return(invisible(NULL))
  }

  if (interactive()) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Missing values detected and no handling strategy configured")
    }

    # Prompt the user for their preference and record it for this session.
    selected_strategy <- prompt_na_handling()
    options(artma.data.na_handling = selected_strategy)

    # Offer to persist the preference to the options file.
    prompt_save_preference(
      option_path = "data.na_handling",
      value = selected_strategy,
      description = "missing value handling strategy",
      respect_autonomy = FALSE
    )
  } else {
    # Non-interactive mode: default to "stop"
    if (get_verbosity() >= 2) {
      cli::cli_warn("Running in non-interactive mode with missing values. Defaulting to 'stop' strategy.")
    }
    options(artma.data.na_handling = "stop")
  }

  invisible(NULL)
}

#' @title Resolve the zero-standard-error handling strategy
#' @description Configure-phase decision that fixes `artma.calc.se_zero_handling`
#'   before the cached compute phase runs. When the option is already set it is
#'   a no-op. Otherwise, if the (already cleaned) data frame has rows with a
#'   zero standard error, it prompts the user interactively when the autonomy
#'   level warrants it (offering the strict `"stop"` choice among others), or
#'   falls back to the friendlier `"remove"` strategy. This is intentionally
#'   separate from `enforce_correct_values()` (the pure compute-phase step) so
#'   that no prompt or option write ever happens inside the cached pipeline.
#' @param df *\[data.frame\]* The cleaned data frame used to detect zero
#'   standard errors (as produced by `clean_data()`).
#' @return `NULL`, invisibly.
#' @keywords internal
resolve_se_zero_handling <- function(df) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / autonomy[should_prompt_user],
    artma / options / prompts[prompt_se_zero_handling],
    artma / interactive / save_preference[prompt_save_preference]
  )

  se_zero_handling_option <- getOption("artma.calc.se_zero_handling", default = NA)

  # Strategy already configured: nothing to decide.
  if (!is.na(se_zero_handling_option)) {
    return(invisible(NULL))
  }

  if (is.null(df[["se"]])) {
    return(invisible(NULL))
  }

  zero_se_rows <- which(suppressWarnings(as.numeric(df$se)) == 0)

  # Without zero standard errors there is no decision to make; the compute
  # phase falls back to the deterministic "remove" default when it runs.
  if (length(zero_se_rows) == 0) {
    return(invisible(NULL))
  }

  if (should_prompt_user(required_level = "autonomous")) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Zero standard errors detected and no handling strategy configured")
    }

    # Prompt the user for their preference and record it for this session.
    selected_strategy <- prompt_se_zero_handling()
    options(artma.calc.se_zero_handling = selected_strategy)

    # Offer to persist the preference to the options file.
    prompt_save_preference(
      option_path = "calc.se_zero_handling",
      value = selected_strategy,
      description = "zero standard error handling strategy",
      respect_autonomy = FALSE
    )
  } else {
    if (get_verbosity() >= 2) {
      cli::cli_warn(c(
        "!" = "{length(zero_se_rows)} row{?s} with zero standard errors detected.",
        "i" = "Defaulting to the {.val remove} strategy. Set {.field artma.calc.se_zero_handling} to {.val stop} for stricter validation."
      ))
    }
    options(artma.calc.se_zero_handling = "remove")
  }

  invisible(NULL)
}

box::export(
  resolve_na_handling,
  resolve_se_zero_handling
)
