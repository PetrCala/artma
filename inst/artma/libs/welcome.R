#' @title Show welcome message for first-time users
#' @description Displays an elegant welcome message introducing artma to new users.
#' @keywords internal
show_welcome_message <- function() {
  box::use(artma / const[CONST])

  # Decorative separator
  cli::cli_rule()

  # Welcome title with styling
  cli::cli_h1(cli::col_blue(cli::style_bold("Welcome to {.pkg {CONST$PACKAGE_NAME}}")))

  # Introduction text
  cli::cli_par()
  cli::cli_text(
    "{cli::symbol$star} {.strong artma} (Automatic Replication Tools for Meta-Analysis) helps you ",
    "perform comprehensive meta-analyses with ease. Whether you're analyzing effect sizes, ",
    "detecting publication bias, or running Bayesian models, artma provides a unified interface ",
    "for all your meta-analysis needs."
  )
  cli::cli_par()

  cli::cli_text(
    "You'll be guided through a simple workflow: setting up your data, choosing analysis methods, ",
    "and reviewing results. Everything is designed to be intuitive and reproducible."
  )
  cli::cli_par()

  # Prompt for readiness
  cli::cli_text(cli::style_bold("Are you ready to get started?"))
  cli::cli_par()

  # Menu for user response
  choices <- c(
    "Yes, let's get started!",
    "Show me more information"
  )

  choice <- climenu::menu(
    choices = choices,
    prompt = "Select an option:"
  )

  if (is.null(choice)) {
    # User cancelled - return early (will still be marked as shown by caller)
    return(invisible())
  }

  if (grepl("more information", choice, ignore.case = TRUE)) {
    cli::cli_par()
    cli::cli_inform(c(
      "i" = "For detailed information, see:",
      " " = "  {.code vignette('getting-started', package='artma')} - Quick start guide",
      " " = "  {.code vignette('options-files', package='artma')} - Options files guide",
      " " = "  {.code ?artma} - Function documentation"
    ))
    cli::cli_par()
    cli::cli_text("Press Enter to continue...")
    readline()
  }

  cli::cli_par()
  cli::cli_rule()
  cli::cli_par()

  invisible()
}

#' @title Check if user is a first-time user
#' @description Determines if this is the user's first time using artma by checking
#'   for options files, a welcome flag file, and session option.
#' @param options_dir *\[character, optional\]* Directory containing options files.
#'   If `NULL`, uses the default options directory.
#' @return *\[logical\]* `TRUE` if first-time user, `FALSE` otherwise.
#' @keywords internal
is_first_time_user <- function(options_dir = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / options / files[resolve_options_dir]
  )

  # Method 3: Check R option (session-based, fastest check)
  if (isTRUE(getOption("artma.welcome.shown", FALSE))) {
    return(FALSE)
  }

  # Resolve options directory
  resolved_dir <- tryCatch(
    resolve_options_dir(options_dir, must_exist = FALSE),
    error = function(e) PATHS$DIR_USR_CONFIG
  )

  # Method 2: Check flag file (persistent across sessions)
  flag_file <- file.path(resolved_dir, ".welcome_shown")
  if (file.exists(flag_file)) {
    # Also set the R option for this session
    options(artma.welcome.shown = TRUE)
    return(FALSE)
  }

  # Method 1: Check for options files (primary method, user's suggestion)
  # Use fully qualified name since options.list is exported
  existing_options <- tryCatch(
    artma::options.list(options_dir = options_dir),
    error = function(e) character(0)
  )

  if (length(existing_options) > 0L) {
    # User has options files, not a first-time user
    # Mark as shown to avoid future checks
    mark_welcome_as_shown(options_dir)
    return(FALSE)
  }

  # No options files, no flag file, and option not set - first-time user
  TRUE
}

#' @title Mark welcome message as shown
#' @description Creates a flag file and sets R option to indicate the welcome
#'   message has been shown, preventing it from appearing again.
#' @param options_dir *\[character, optional\]* Directory for the flag file.
#'   If `NULL`, uses the default options directory.
#' @keywords internal
mark_welcome_as_shown <- function(options_dir = NULL) {
  box::use(
    artma / paths[PATHS],
    artma / options / files[resolve_options_dir],
    artma / libs / file_utils[ensure_folder_existence]
  )

  # Set R option for current session
  options(artma.welcome.shown = TRUE)

  # Create flag file for persistence across sessions
  resolved_dir <- tryCatch(
    resolve_options_dir(options_dir, must_exist = FALSE),
    error = function(e) PATHS$DIR_USR_CONFIG
  )

  # Ensure directory exists
  ensure_folder_existence(resolved_dir)

  flag_file <- file.path(resolved_dir, ".welcome_shown")
  # Create empty flag file
  file.create(flag_file)

  invisible()
}
