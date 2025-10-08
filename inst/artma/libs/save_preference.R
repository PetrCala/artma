#' @title Save Preference Prompt
#' @description Reusable function to prompt users whether they want to save a
#'   configuration choice to their options file or use it only for the current session.

#' @title Prompt user to save preference to options file
#' @description Ask the user if they want to save a configuration choice
#'   (e.g., selected variables, handling strategy) to their options file
#'   or use it only for the current session.
#' @param option_path *\[character\]* The option path (e.g., "data.na_handling")
#' @param value *\[any\]* The value to save
#' @param description *\[character, optional\]* Short description of what's being saved
#'   (e.g., "missing value handling strategy", "selected variables")
#' @return *\[logical\]* TRUE if successfully saved, FALSE otherwise
#' @export
prompt_save_preference <- function(option_path, value, description = NULL) {
  box::use(
    artma / libs / utils[get_verbosity],
    artma / libs / validation[validate]
  )

  validate(
    is.character(option_path),
    length(option_path) == 1
  )

  # Only prompt in interactive mode
  if (!interactive()) {
    if (get_verbosity() >= 4) {
      cli::cli_inform("Non-interactive mode: preference not saved to file")
    }
    return(FALSE)
  }

  # Create description text
  desc_text <- if (!is.null(description)) {
    description
  } else {
    "this configuration"
  }

  cli::cat_line()

  # Ask if they want to save
  save_choices <- c(
    "Yes, save to options file" = "yes",
    "No, use only for this session" = "no"
  )

  selected <- climenu::select(
    choices = names(save_choices),
    prompt = sprintf("Do you want to save %s to your options file?", desc_text),
    selected = 1 # Default to "yes"
  )

  if (rlang::is_empty(selected)) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("No selection made. Using for current session only.")
    }
    return(FALSE)
  }

  should_save <- save_choices[selected] == "yes"

  if (!should_save) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Using for current session only.")
    }
    return(FALSE)
  }

  # Attempt to save to options file
  saved <- save_to_options_file(option_path, value)

  if (saved && get_verbosity() >= 3) {
    cli::cli_alert_success("Preference saved to options file")
  }

  cli::cat_line()

  saved
}


#' @title Save value to options file
#' @description Internal function to save a value to the user's options file.
#' @param option_path *\[character\]* The option path (e.g., "data.na_handling")
#' @param value *\[any\]* The value to save
#' @return *\[logical\]* TRUE if successfully saved, FALSE otherwise
#' @keywords internal
save_to_options_file <- function(option_path, value) {
  box::use(artma / libs / utils[get_verbosity])

  tryCatch(
    {
      box::use(artma / options / files[read_options_file, write_options_file])

      options_file <- getOption("artma.temp.file_name")
      if (is.null(options_file)) {
        if (get_verbosity() >= 2) {
          cli::cli_warn("No options file found. Preference not saved.")
        }
        return(FALSE)
      }

      # Read current options
      current_opts <- read_options_file(options_file)

      # Parse option path and update nested structure
      path_parts <- strsplit(option_path, "\\.")[[1]]

      # Navigate to the correct level and update
      if (length(path_parts) == 1) {
        current_opts[[path_parts[1]]] <- value
      } else if (length(path_parts) == 2) {
        if (is.null(current_opts[[path_parts[1]]])) {
          current_opts[[path_parts[1]]] <- list()
        }
        current_opts[[path_parts[1]]][[path_parts[2]]] <- value
      } else if (length(path_parts) == 3) {
        if (is.null(current_opts[[path_parts[1]]])) {
          current_opts[[path_parts[1]]] <- list()
        }
        if (is.null(current_opts[[path_parts[1]]][[path_parts[2]]])) {
          current_opts[[path_parts[1]]][[path_parts[2]]] <- list()
        }
        current_opts[[path_parts[1]]][[path_parts[2]]][[path_parts[3]]] <- value
      }

      # Write back to file
      write_options_file(options_file, current_opts)

      return(TRUE)
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_warn("Could not save preference to options file: {e$message}")
      }
      return(FALSE)
    }
  )
}


#' @title Save variable list to options file
#' @description Save a list of selected variables to the data configuration
#'   in the options file. This is a specialized version for saving variable selections.
#' @param var_names *\[character\]* Vector of variable names
#' @param var_configs *\[list\]* Named list of variable configurations (optional)
#' @param description *\[character, optional\]* Description of the variables
#' @return *\[logical\]* TRUE if successfully saved, FALSE otherwise
#' @export
prompt_save_variable_selection <- function(var_names, var_configs = NULL, description = NULL) {
  box::use(
    artma / libs / utils[get_verbosity],
    artma / libs / validation[validate]
  )

  validate(is.character(var_names))

  if (length(var_names) == 0) {
    if (get_verbosity() >= 4) {
      cli::cli_inform("No variables to save")
    }
    return(FALSE)
  }

  # Only prompt in interactive mode
  if (!interactive()) {
    if (get_verbosity() >= 4) {
      cli::cli_inform("Non-interactive mode: variable selection not saved to file")
    }
    return(FALSE)
  }

  # Create description text
  desc_text <- if (!is.null(description)) {
    sprintf("selected %s (%d variable%s)", description, length(var_names), ifelse(length(var_names) == 1, "", "s"))
  } else {
    sprintf("selected variables (%d)", length(var_names))
  }

  cli::cat_line()

  # Ask if they want to save
  save_choices <- c(
    "Yes, save to options file" = "yes",
    "No, use only for this session" = "no"
  )

  selected <- climenu::select(
    choices = names(save_choices),
    prompt = sprintf("Do you want to save %s to your options file?", desc_text),
    selected = 1 # Default to "yes"
  )

  if (rlang::is_empty(selected)) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("No selection made. Using for current session only.")
    }
    return(FALSE)
  }

  should_save <- save_choices[selected] == "yes"

  if (!should_save) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Using for current session only.")
    }
    return(FALSE)
  }

  # Attempt to save to options file
  saved <- save_variables_to_config(var_names, var_configs)

  if (saved && get_verbosity() >= 3) {
    cli::cli_alert_success("Variable selection saved to options file")
  }

  cli::cat_line()

  saved
}


#' @title Save variables to data config in options file
#' @description Internal function to update the data configuration with variable selections.
#' @param var_names *\[character\]* Vector of variable names
#' @param var_configs *\[list\]* Named list of variable configurations (optional)
#' @return *\[logical\]* TRUE if successfully saved, FALSE otherwise
#' @keywords internal
save_variables_to_config <- function(var_names, var_configs = NULL) {
  box::use(artma / libs / utils[get_verbosity])

  tryCatch(
    {
      box::use(
        artma / options / files[read_options_file, write_options_file],
        artma / data_config / read[get_data_config]
      )

      options_file <- getOption("artma.temp.file_name")
      if (is.null(options_file)) {
        if (get_verbosity() >= 2) {
          cli::cli_warn("No options file found. Variable selection not saved.")
        }
        return(FALSE)
      }

      # Read current options
      current_opts <- read_options_file(options_file)

      # Get current config or initialize if missing
      config <- if (!is.null(current_opts$data$config)) {
        current_opts$data$config
      } else {
        get_data_config()
      }

      # Update config with variable selections
      for (var_name in var_names) {
        config_key <- make.names(var_name)

        if (!config_key %in% names(config)) {
          if (get_verbosity() >= 3) {
            cli::cli_warn("Variable {var_name} not found in config. Skipping.")
          }
          next
        }

        # If we have detailed configurations, apply them
        if (!is.null(var_configs) && var_name %in% names(var_configs)) {
          var_conf <- var_configs[[var_name]]

          # Update split method and value if provided
          if (!is.null(var_conf$split_method)) {
            if (var_conf$split_method == "equal") {
              config[[config_key]]$equal <- var_conf$split_value
              config[[config_key]]$gltl <- NA_character_
            } else if (var_conf$split_method == "gltl") {
              config[[config_key]]$gltl <- var_conf$split_value
              config[[config_key]]$equal <- NA_character_
            }
          }

          # Set effect_sum_stats flag if not already set
          if (is.null(config[[config_key]]$effect_sum_stats)) {
            config[[config_key]]$effect_sum_stats <- TRUE
          }
        }
      }

      # Update options with modified config
      current_opts$data$config <- config

      # Write back to file
      write_options_file(options_file, current_opts)

      return(TRUE)
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_warn("Could not save variable selection: {e$message}")
      }
      return(FALSE)
    }
  )
}


box::export(
  prompt_save_preference,
  prompt_save_variable_selection,
  save_to_options_file,
  save_variables_to_config
)
