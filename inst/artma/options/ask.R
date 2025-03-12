#' @title Ask for options file name
#' @description Ask the user to input a name of an options file. Clean the user's input and return it as a string.
#' @param should_clean [logical, optional] Whether to clean the input string. Defaults to TRUE
#' @param prompt [character, optional] The prompt to use. Defaults to a generic prompt.
#' @return [character] The options file name.
ask_for_options_file_name <- function(should_clean = TRUE, prompt = NULL) {
  if (!interactive()) {
    rlang::abort("You must provide the options file name explicitly in non-interactive R sessions.")
  }
  box::use(artma / options / utils[parse_options_file_name])

  prompt <- prompt %||% "Please provide the name for your options file, including the .yaml suffix: "
  options_file_name <- readline(prompt = prompt)

  if (should_clean) {
    options_file_name <- parse_options_file_name(options_file_name)
  }

  return(options_file_name)
}


#' @title Ask for an existing options file name
#' @description Prompt the user to select from an existing list of user options files. Return the name of the selected file as a character.
#' @param options_dir [character, optional] Name of the directory to list the files from. Defaults to NULL.
#' @param prompt [character, optional] The prompt to use when asking for the user options file name. Defaults to NULL.
#' @return [character] Name of the selected file.
ask_for_existing_options_file_name <- function(options_dir = NULL, prompt = NULL) { # nolint: object_length_linter.
  if (!interactive()) {
    rlang::abort("You must provide the options file name explicitly in non-interactive R sessions.")
  }
  box::use(
    artma / libs / utils[is_empty],
    artma / options / utils[list_user_options_files],
  )

  prompt <- prompt %||% "Please select the user options file name you would like to use."

  user_options_file_names <- list_user_options_files(options_dir = options_dir)
  if (length(user_options_file_names) == 0) {
    stop("No existing user options files were found. Aborting...")
  }

  selected_file_name <- utils::select.list(
    title = prompt,
    choices = user_options_file_names
  )
  if (is_empty(selected_file_name)) {
    stop("No user options file was selected. Aborting...")
  }
  return(selected_file_name)
}

#' @title Ask for options to modify
#' @description Prompt the user to input the names and values of the options they wish to modify. Return a list of the modified options.
#' @return [list] A list of the modified options.
ask_for_options_to_modify <- function() {
  cli::cli_h1("Modify Options")
  cli::cli_text("Please provide the names and values of the options you wish to modify.")

  cli::cli_h3("Instructions")
  cli::cli_ul(c(
    "The names should be {.emph separated by dots} and {.strong NOT} prepended by the package name prefix. {.strong Example}: {cli::col_magenta('logging.log_file_name')}",
    "{.strong DO NOT} use quotes for option names.",
    "The values should usually be provided {.emph without quotes}. Use quotes only if the value is a string that contains spaces or special characters.",
    "Press {.kbd Enter} to finish."
  ))
  cat("\n")

  options_list <- list()

  get_option_name <- function() {
    readline(cli::format_inline("Please enter an option name (or press {.kbd Enter} to finish): "))
  }

  get_option_value <- function(option_name, max_retries = 3) {
    retries <- 0
    option_value <- ""

    while (option_value == "" && retries < max_retries) {
      prompt_text <- if (retries == 0) {
        cli::format_inline("Please provide the value for {cli::col_magenta(option_name)}: ")
      } else {
        cli::format_inline("{cli::col_red(cli::symbol$cross)} Value cannot be empty. Please provide a value for {cli::col_magenta(option_name)}:")
      }
      option_value <- readline(prompt_text)
      retries <- retries + 1
    }

    if (option_value == "") {
      cli::cli_alert_danger("Failed to set the value for option {cli::col_magenta(option_name)}.")
      cat("\n")
      return(NULL)
    }

    option_value <- stringr::str_trim(option_value)

    # Possibly add more validation here in the future.

    option_value
  }

  print_options_to_apply <- function() {
    if (length(options_list) > 0) {
      cli::cli_h3("Applying the following options:")
      for (opt_name in names(options_list)) {
        opt_str <- glue::glue("{cli::col_magenta(opt_name)}: {cli::col_green(options_list[[opt_name]])}")
        cli::cli_text(opt_str)
      }
    } else {
      cli::cli_alert_info("No options provided. Keeping the existing options.")
    }
    cat("\n")
  }

  repeat {
    option_name <- get_option_name()
    if (option_name == "") {
      print_options_to_apply()
      break
    }


    if (option_name %in% names(options_list)) {
      cli::cli_alert_danger("Option already exists: {cli::col_magenta(option_name)}. Choose a different name.")
      cat("\n")
      next
    }

    option_value <- get_option_value(option_name)
    if (is.null(option_value)) next

    options_list[[option_name]] <- option_value
    opt_str <- glue::glue("{cli::col_magenta(option_name)}: {cli::col_green(option_value)}") # nolint: unused_declared_object_linter.
    cli::cli_alert_success("Option added: {.emph {opt_str}}")
    cat("\n")
  }

  options_list
}

box::export(
  ask_for_options_file_name,
  ask_for_options_to_modify,
  ask_for_existing_options_file_name
)
