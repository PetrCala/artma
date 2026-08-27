# nolint start: box_usage_linter.

#' @title Runtime Setup
#' @description
#' A function user as a wrapper for runtime functions invocation to ensure crucial fucntionality, such as imports, etc., all work as expected.
#'
#' @param FUN [function] The function to be called after the setup.
#' @param options_file_name *\[character\]* Name of the options file to use, including the suffix.
#' @param options_dir *\[character, optional\]* Path to the directory that contains user options. Defaults to the directory specified in PATHS.
#' @param allow_unbound *\[logical, optional\]* Whether an interactive call
#'   without an options file may proceed on the template defaults instead of
#'   prompting for a file. The session hub sets this: it offers the choice of
#'   options file as a menu item rather than as a gate in front of the menu.
#'   Defaults to FALSE, which keeps the prompt.
#' @keywords internal
runtime_setup <- function(
  FUN,
  options_file_name = NULL,
  options_dir = NULL,
  allow_unbound = FALSE
) {
  if (is.null(options_file_name) && !interactive()) {
    if (getOption("artma.verbose", 3) >= 2) {
      cli::cli_alert_warning("Running in non-interactive mode without providing an options file name. Please provide an options file name or run in interactive mode.")
    }
    return(invisible())
  }

  # An unbound session runs on the template defaults with no file behind it,
  # which is what the hub needs to open its menu before the user has chosen
  # one. Nothing else changes: the values live in the same option namespace,
  # and loading a file later overwrites them.
  runtime_options <- if (is.null(options_file_name) && isTRUE(allow_unbound)) {
    unbound_runtime_options(options_dir = options_dir)
  } else {
    # Loading itself is pure, so it will neither migrate nor repair an outdated
    # file. Migrate legacy dual-store files (data.colnames + data.config) to the
    # unified data.columns store up front, then, in interactive mode, detect an
    # outdated file and offer to fix it before loading, so the user is not
    # silently running on defaults.
    if (!is.null(options_file_name)) {
      box::use(artma / options / migrate[migrate_legacy_options])
      migrate_legacy_options(options_file_name = options_file_name, options_dir = options_dir)
      if (interactive()) {
        offer_options_fix(options_file_name = options_file_name, options_dir = options_dir)
      }
    }

    options_load(
      options_file_name = options_file_name,
      options_dir = options_dir,
      should_validate = TRUE,
      should_add_temp_options = TRUE # Load to the options() namespace
    )
  }

  withr::local_options(runtime_options)

  # The CLI (cli_run) carries its flag-derived options overlay in a dedicated
  # session option so it survives the file load above and still wins. Re-apply
  # it here, after the file values are in place. No-op for every other caller.
  cli_overrides <- getOption("artma.temp.cli_overrides", NULL)
  if (is.list(cli_overrides) && length(cli_overrides) > 0) {
    withr::local_options(cli_overrides)
  }

  FUN()
}

#' @title Session options for a session with no options file
#' @description The template defaults, prefixed for the `options()` namespace,
#'   plus the temporary options that name the file behind the session: the
#'   directory, and an explicitly empty file name. Every `getOption()` in the
#'   package therefore reads the same value it would read under a freshly
#'   created options file, and the empty file name is what marks the session
#'   as unbound.
#' @param options_dir *\[character, optional\]* Path to the directory that
#'   contains user options.
#' @param template_path *\[character, optional\]* Path to the options template.
#' @return *\[list\]* Options to apply for the session.
#' @keywords internal
unbound_runtime_options <- function(options_dir = NULL, template_path = NULL) {
  box::use(
    artma / const[CONST],
    artma / options / files[resolve_options_dir, resolve_template_path],
    artma / options / template[get_template_defaults]
  )

  options_dir <- resolve_options_dir(options_dir, must_exist = FALSE)
  runtime_options <- get_template_defaults(
    template_path = resolve_template_path(template_path),
    prefix = CONST$PACKAGE_NAME
  )

  # Single-bracket assignment of a NULL list: `[[<-` would drop the entry
  # instead of carrying an explicit NULL through to `options()`.
  runtime_options["artma.temp.file_name"] <- list(NULL)
  runtime_options[["artma.temp.dir_name"]] <- options_dir
  runtime_options
}

#' @title Offer to fix an outdated options file
#' @description Validate the given options file and, if it has problems, offer the
#'   interactive user a chance to repair it via [options_fix()] before it is
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
        suppressMessages(options_validate(
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
      box::use(artma / interactive / input[ask_select])
      choice <- ask_select(
        question = "How would you like to handle this?",
        choices = c("Fix now (recommended)" = "fix", "Continue with defaults" = "continue")
      )
      if (identical(choice, "fix")) {
        options_fix(
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
