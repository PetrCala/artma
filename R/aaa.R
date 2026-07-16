# nolint start: box_usage_linter.

#' @title Runtime Setup
#' @description
#' A function user as a wrapper for runtime functions invocation to ensure crucial fucntionality, such as imports, etc., all work as expected.
#'
#' @param FUN [function] The function to be called after the setup.
#' @param options_file_name *\[character\]* Name of the options file to use, including the suffix.
#' @param options_dir *\[character, optional\]* Path to the directory that contains user options. Defaults to the directory specified in PATHS.
#' @keywords internal
runtime_setup <- function(
    FUN,
    options_file_name = NULL,
    options_dir = NULL) {
  if (is.null(options_file_name) && !interactive()) {
    if (getOption("artma.verbose", 3) >= 2) {
      cli::cli_alert_warning("Running in non-interactive mode without providing an options file name. Please provide an options file name or run in interactive mode.")
    }
    return(invisible())
  }

  # Loading itself is pure, so it will neither migrate nor repair an outdated
  # file. In interactive mode, detect an outdated file up front and offer to fix
  # it before loading, so the user is not silently running on defaults.
  if (interactive() && !is.null(options_file_name)) {
    offer_options_fix(options_file_name = options_file_name, options_dir = options_dir)
  }

  runtime_options <- options.load(
    options_file_name = options_file_name,
    options_dir = options_dir,
    should_validate = TRUE,
    should_add_temp_options = TRUE # Load to the options() namespace
  )

  withr::local_options(runtime_options)

  FUN()
}

#' @title Offer to fix an outdated options file
#' @description Validate the given options file and, if it has problems, offer the
#'   interactive user a chance to repair it via [options.fix()] before it is
#'   loaded. Best effort: any failure here is swallowed so that loading (which is
#'   pure and applies defaults) can still proceed.
#' @param options_file_name *\[character\]* Name of the options file, including the suffix.
#' @param options_dir *\[character, optional\]* Path to the directory that contains user options.
#' @keywords internal
offer_options_fix <- function(options_file_name, options_dir = NULL) {
  tryCatch(
    {
      errors <- withr::with_options(
        list("artma.verbose" = 1),
        suppressMessages(options.validate(
          options_file_name = options_file_name,
          options_dir = options_dir,
          failure_action = "return_errors_quiet"
        ))
      )

      if (length(errors) == 0) {
        return(invisible())
      }

      cli::cli_alert_warning(
        "Your options file {.file {options_file_name}} is outdated ({length(errors)} problem{?s})."
      )
      choice <- climenu::select(
        choices = c("Fix now (recommended)", "Continue with defaults"),
        prompt = "How would you like to handle this?"
      )
      if (choice == "Fix now (recommended)") {
        options.fix(
          options_file_name = options_file_name,
          options_dir = options_dir,
          force_default_overwrites = TRUE
        )
      }
    },
    error = function(e) invisible()
  )

  invisible()
}

# nolint end: box_usage_linter.
