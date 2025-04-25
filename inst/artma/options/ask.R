# nolint start: object_length_linter.

#' @title Ask for options file name
#' @description Ask the user to input a name of an options file. Clean the user's input and return it as a string.
#' @param should_clean *\[logical, optional\]* Whether to clean the input string. Defaults to TRUE
#' @param prompt *\[character, optional\]* The prompt to use. Defaults to a generic prompt.
#' `character` The options file name.
ask_for_options_file_name <- function(should_clean = TRUE, prompt = NULL) {
  if (!interactive()) {
    cli::cli_abort("You must provide the options file name explicitly in non-interactive R sessions.")
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
#' @param options_dir *\[character, optional\]* Name of the directory to list the files from. Defaults to `NULL`.
#' @param prompt *\[character, optional\]* The prompt to use when asking for the user options file name. Defaults to `NULL`.
#' @param multiple *\[logical, optional\]* Whether to allow the selction of multiple values. Defaults to FALSE.
#' `character` Name of the selected file.
ask_for_existing_options_file_name <- function(
    options_dir = NULL,
    prompt = NULL,
    multiple = FALSE) {
  if (!interactive()) {
    cli::cli_abort("You must provide the options file name explicitly in non-interactive R sessions.")
  }

  box::use(
    artma[options.list],
    artma / libs / string[pluralize]
  )

  file_str <- if (isTRUE(multiple)) pluralize("name") else "name" # nolint: unused_declared_object_linter.

  prompt <- prompt %||% glue::glue("Please select the user options file {file_str} you would like to use.")

  user_options_file_names <- options.list(options_dir = options_dir) # nolint: box_usage_linter.
  if (length(user_options_file_names) == 0) {
    cli::cli_abort("No existing user options files were found. Aborting…")
  }

  selected_file_name <- utils::select.list(
    title = prompt,
    choices = user_options_file_names,
    multiple = multiple
  )
  if (rlang::is_empty(selected_file_name)) {
    cli::cli_abort("No user options file was selected. Aborting…")
  }
  return(selected_file_name)
}

#' @title Ask for an option value
#' @description Prompt the user to input a value for an option. Validate the input and return the value.
#' @param option_name *\[character\]* The name of the option.
#' @param option_type *\[character, optional\]* The type of the option. Defaults to `NULL`.
#' @param allow_na *\[logical, optional\]* Whether to allow NA values. Defaults to FALSE.
#' @param max_retries *\[integer, optional\]* The maximum number of retries to ask for the option value. Defaults to 3.
#' `any` The value of the option.
ask_for_option_value <- function(
    option_name,
    option_type = NULL,
    allow_na = FALSE,
    max_retries = 3) {
  box::use(
    artma / const[CONST],
    artma / options / utils[validate_option_value],
    artma / libs / string[trim_quotes]
  )

  retries <- 0
  option_value <- ""

  while (option_value == "" && retries < max_retries) {
    prompt_text <- if (retries == 0) {
      cli::format_inline("Please provide the value for {CONST$STYLES$OPTIONS$NAME(option_name)}: ")
    } else {
      cli::format_inline("{cli::col_red(cli::symbol$cross)} Value cannot be empty. Please provide a value for {CONST$STYLES$OPTIONS$NAME(option_name)}:")
    }
    option_value <- readline(prompt_text)
    retries <- retries + 1
  }

  if (option_value == "") {
    cli::cli_alert_danger("Failed to set the value for option {CONST$STYLES$OPTIONS$NAME(option_name)}.")
    cli::cat_line()
    return(NULL)
  }

  if (is.character(option_value)) {
    option_value <- stringr::str_trim(option_value, side = "both")
    option_value <- trim_quotes(option_value)
  }

  if (!is.null(option_type)) {
    err_msg <- validate_option_value(option_value, option_type, option_name, allow_na)
    if (!is.null(err_msg)) {
      cli::cli_alert_danger(err_msg)
      cli::cat_line()
      return(NULL)
    }
  }

  option_value
}

#' @title Ask for options to modify
#' @description Prompt the user to input the names and values of the options they wish to modify. Return a list of the modified options.
#' `list` A list of the modified options.
ask_for_options_to_modify <- function() {
  box::use(artma / const[CONST])

  cli::cli_h1("Modify Options")
  cli::cli_text("Please provide the names and values of the options you wish to modify.")

  cli::cli_h3("Instructions")
  cli::cli_ul(c(
    "The names should be {.emph separated by dots} and {.strong NOT} prepended by the package name prefix. {.strong Example}: {CONST$STYLES$OPTIONS$NAME('data.source_path')}",
    "{.strong DO NOT} use quotes for option names.",
    "The values should usually be provided {.emph without quotes}. Use quotes only if the value is a string that contains spaces or special characters.",
    "Press {.kbd Enter} to finish."
  ))
  cli::cat_line()

  options_list <- list()

  get_option_name <- function() {
    readline(cli::format_inline("Please enter an option name (or press {.kbd Enter} to finish): "))
  }

  print_options_to_apply <- function() {
    if (length(options_list) > 0) {
      cli::cli_h3("Applying the following options:")
      cli::cli_ul()
      for (opt_name in names(options_list)) {
        opt_str <- glue::glue("{CONST$STYLES$OPTIONS$NAME(opt_name)}: {CONST$STYLES$OPTIONS$VALUE(options_list[[opt_name]])}")
        cli::cli_li(opt_str)
      }
    } else {
      cli::cli_alert_info("No options provided. Keeping the existing options.")
    }
    cli::cat_line()
  }

  repeat {
    option_name <- get_option_name()
    if (option_name == "") {
      print_options_to_apply()
      break
    }

    if (option_name %in% names(options_list)) {
      cli::cli_alert_danger("Option already exists: {CONST$STYLES$OPTIONS$NAME(option_name)}. Choose a different name.")
      cli::cat_line()
      next
    }

    # if NA/NULL is passed, the validation correctly fails down the line, so we allow NA values
    option_value <- ask_for_option_value(option_name, allow_na = TRUE)
    if (is.null(option_value)) next

    options_list[[option_name]] <- option_value
    opt_str <- glue::glue("{CONST$STYLES$OPTIONS$NAME(option_name)}: {CONST$STYLES$OPTIONS$VALUE(option_value)}") # nolint: unused_declared_object_linter.
    cli::cli_alert_success("Option added: {.emph {opt_str}}")
    cli::cat_line()
  }

  options_list
}

box::export(
  ask_for_option_value,
  ask_for_options_file_name,
  ask_for_options_to_modify,
  ask_for_existing_options_file_name
)

# nolint end: object_length_linter.
