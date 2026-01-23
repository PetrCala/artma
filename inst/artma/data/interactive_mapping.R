#' @title Format mapping for display
#' @description Helper function to format column mapping for user display
#' @param mapping *\[list\]* The column mapping (std_col -> data_col)
#' @param required_cols *\[character\]* Required column names
#' @param all_std_cols *\[character\]* All standard column names (required + optional)
#' @return *\[list\]* List with 'required' and 'optional' mappings
format_mapping_display <- function(mapping, required_cols, all_std_cols = NULL) {
  if (is.null(all_std_cols)) {
    all_std_cols <- names(mapping)
  }

  required_mapping <- mapping[names(mapping) %in% required_cols]
  optional_mapping <- mapping[names(mapping) %in% setdiff(all_std_cols, required_cols)]

  list(
    required = required_mapping,
    optional = optional_mapping
  )
}


#' @title Present detected column mapping to user
#' @description Show detected columns to user and get their confirmation choice
#' @param auto_mapping *\[list\]* Automatically detected column mapping
#' @param df *\[data.frame\]* The data frame
#' @param required_cols *\[character\]* Required column names
#' @param all_std_cols *\[character\]* All standard column names
#' @return *\[character\]* User's choice: "accept", "modify", or "skip_optional"
present_detected_mapping <- function(
    auto_mapping,
    df,
    required_cols,
    all_std_cols = NULL) {
  box::use(
    climenu[menu],
    artma / libs / utils[get_verbosity]
  )

  if (length(auto_mapping) == 0) {
    return("modify")
  }

  if (is.null(all_std_cols)) {
    box::use(artma / data / column_recognition[get_column_patterns])
    patterns <- get_column_patterns()
    all_std_cols <- names(patterns)
  }

  # Format mapping for display
  formatted <- format_mapping_display(auto_mapping, required_cols, all_std_cols)

  if (get_verbosity() >= 3) {
    cli::cli_h2("Detected Column Mapping")
    cli::cli_inform("We have detected the following columns in your data:")
    cli::cli_par()

    # Show required columns
    if (length(formatted$required) > 0) {
      cli::cli_inform("{.strong Required columns:}")
      for (std_col in names(formatted$required)) {
        cli::cli_inform("  {cli::symbol$bullet} {.field {std_col}} {cli::symbol$arrow_right} {.val {formatted$required[[std_col]]}}")
      }
    }

    # Show optional columns
    if (length(formatted$optional) > 0) {
      cli::cli_inform("{.strong Optional columns:}")
      for (std_col in names(formatted$optional)) {
        cli::cli_inform("  {cli::symbol$bullet} {.field {std_col}} {cli::symbol$arrow_right} {.val {formatted$optional[[std_col]]}}")
      }
    }

    cli::cli_par()
  }

  # Present choices to user
  choices <- c(
    "Accept all detected columns",
    "Modify mappings",
    "Skip optional columns (keep only required)"
  )

  choice_idx <- menu(
    choices = choices,
    title = "What would you like to do?"
  )

  if (choice_idx == 0) {
    cli::cli_abort("Column mapping cancelled by user")
  }

  choice <- choices[choice_idx]

  if (grepl("Accept", choice, fixed = TRUE)) {
    return("accept")
  } else if (grepl("Modify", choice, fixed = TRUE)) {
    return("modify")
  } else if (grepl("Skip", choice, fixed = TRUE)) {
    return("skip_optional")
  }

  # Default to modify
  "modify"
}


#' @title Interactive column mapping with climenu
#' @description Allow users to interactively map columns using climenu
#' @param df *\[data.frame\]* The data frame
#' @param auto_mapping *\[list\]* Automatically recognized mapping
#' @param required_only *\[logical\]* If TRUE, only ask for required columns
#' @param show_detected_first *\[logical\]* If TRUE, show detected columns first for confirmation
#' @return *\[list\]* User-confirmed column mapping
interactive_column_mapping <- function(df, auto_mapping = list(), required_only = TRUE, show_detected_first = FALSE) {
  box::use(
    climenu[menu],
    artma / libs / validation[validate],
    artma / libs / utils[get_verbosity],
    artma / data / column_recognition[
      get_required_column_names,
      get_column_patterns
    ]
  )

  validate(is.data.frame(df), is.list(auto_mapping))

  # Determine which columns to ask about
  patterns <- get_column_patterns()
  required_cols <- get_required_column_names()
  all_std_cols <- names(patterns)

  cols_to_map <- if (required_only) {
    required_cols
  } else {
    all_std_cols
  }

  # Track missing required columns
  missing_required <- setdiff(required_cols, names(auto_mapping))

  # If show_detected_first is TRUE and we have detected columns, present them first
  if (show_detected_first && length(auto_mapping) > 0) {
    user_choice <- present_detected_mapping(
      auto_mapping = auto_mapping,
      df = df,
      required_cols = required_cols,
      all_std_cols = all_std_cols
    )

    if (user_choice == "accept") {
      # User accepted all detected columns
      if (get_verbosity() >= 3) {
        cli::cli_alert_success("Accepted all detected column mappings")
      }
      # Still need to check if required columns are missing
      if (length(missing_required) == 0) {
        return(auto_mapping)
      }
      # Fall through to prompt for missing required columns
      mapping <- auto_mapping
    } else if (user_choice == "skip_optional") {
      # Remove optional columns, keep only required
      mapping <- auto_mapping[names(auto_mapping) %in% required_cols]
      if (get_verbosity() >= 3) {
        cli::cli_alert_info("Skipped optional columns, keeping only required mappings")
      }
      # Fall through to check for missing required
    } else {
      # User chose to modify - continue with interactive mapping
      mapping <- auto_mapping
    }
  } else {
    # Not showing detected first, use auto_mapping as starting point
    mapping <- auto_mapping
  }

  # If all required columns are present and user accepted, return early
  if (length(missing_required) == 0 && required_only && !show_detected_first) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_success("All required columns automatically recognized")
    }
    return(mapping)
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Interactive column mapping")
    if (length(mapping) > 0) {
      cli::cli_inform("Current mappings: {.field {paste(names(mapping), collapse = ', ')}}")
    }
    if (length(missing_required) > 0) {
      cli::cli_inform("Missing required: {.field {paste(missing_required, collapse = ', ')}}")
    }
  }

  available_cols <- names(df)

  # If user chose to modify, allow editing existing mappings first
  if (show_detected_first && length(mapping) > 0) {
    cli::cli_h2("Modify Column Mappings")
    cli::cli_inform("You can modify any of the detected mappings or skip to keep them as-is.")

    # Ask if user wants to modify existing mappings
    modify_choices <- c(
      names(mapping),
      "--- Keep all current mappings ---",
      "--- Modify all mappings ---"
    )

    modify_idx <- menu(
      choices = modify_choices,
      title = "Select a mapping to modify, or keep/modify all"
    )

    if (modify_idx == 0) {
      cli::cli_abort("Column mapping cancelled by user")
    }

    selected_modify <- modify_choices[modify_idx]

    if (grepl("Keep all", selected_modify, fixed = TRUE)) {
      # Keep all mappings, just continue to missing required
    } else if (grepl("Modify all", selected_modify, fixed = TRUE)) {
      # Clear all mappings and re-map everything
      mapping <- list()
      missing_required <- required_cols
    } else {
      # Modify specific column
      std_col_to_modify <- selected_modify
      pattern_def <- patterns[[std_col_to_modify]]

      cli::cli_h2("Modify mapping: {.field {std_col_to_modify}}")
      cli::cli_inform("Current mapping: {.val {mapping[[std_col_to_modify]]}}")
      cli::cli_inform("Examples: {.val {pattern_def$keywords[1:min(3, length(pattern_def$keywords))]}}")

      # Add current mapping back to available if it was removed
      current_mapped <- mapping[[std_col_to_modify]]
      if (!current_mapped %in% available_cols) {
        available_cols <- c(available_cols, current_mapped)
      }

      choices <- c(
        available_cols,
        "--- Keep current mapping ---",
        "--- Remove this mapping ---"
      )

      choice_idx <- menu(
        choices = choices,
        title = sprintf("Select new column for '%s'", std_col_to_modify)
      )

      if (choice_idx == 0) {
        cli::cli_abort("Column mapping cancelled by user")
      }

      selected <- choices[choice_idx]

      if (grepl("Keep current", selected, fixed = TRUE)) {
        # Keep as-is, do nothing
      } else if (grepl("Remove", selected, fixed = TRUE)) {
        # Remove this mapping
        mapping <- mapping[names(mapping) != std_col_to_modify]
        if (std_col_to_modify %in% required_cols) {
          missing_required <- unique(c(missing_required, std_col_to_modify))
        }
      } else {
        # Update mapping
        mapping[[std_col_to_modify]] <- selected
        available_cols <- setdiff(available_cols, selected)
      }
    }
  }

  # Ask for each missing required column
  for (std_col in missing_required) {
    pattern_def <- patterns[[std_col]]

    cli::cli_h2("Map column: {.field {std_col}}")
    cli::cli_inform("Examples: {.val {pattern_def$keywords[1:min(3, length(pattern_def$keywords))]}}")

    # Add "Skip (column not present)" and "None of these" options
    choices <- c(
      available_cols,
      "--- Skip (column not present) ---",
      "--- None of these ---"
    )

    choice_idx <- menu(
      choices = choices,
      title = sprintf("Select the column for '%s' (required)", std_col)
    )

    if (choice_idx == 0) {
      # User cancelled
      cli::cli_abort("Column mapping cancelled by user")
    }

    selected <- choices[choice_idx]

    if (grepl("^---.*Skip.*---$", selected)) {
      if (std_col %in% required_cols) {
        cli::cli_alert_warning("Skipping required column {.field {std_col}}. This may cause errors later.")
      }
      next
    } else if (grepl("^---.*None.*---$", selected)) {
      # Ask user to type column name manually
      typed_col <- readline(sprintf("Enter the exact column name for '%s': ", std_col))
      typed_col <- trimws(typed_col)

      if (nchar(typed_col) == 0) {
        cli::cli_alert_warning("No column name provided, skipping {.field {std_col}}")
        next
      }

      if (!typed_col %in% available_cols) {
        cli::cli_alert_danger("Column {.val {typed_col}} not found in data frame. Skipping.")
        next
      }

      selected <- typed_col
    }

    mapping[[std_col]] <- selected

    # Remove from available columns to avoid duplicate mapping
    available_cols <- setdiff(available_cols, selected)
  }

  # Optionally ask for optional columns
  if (!required_only) {
    optional_cols <- setdiff(cols_to_map, c(names(mapping), missing_required))

    if (length(optional_cols) > 0 && length(available_cols) > 0) {
      cli::cli_h2("Optional columns")
      cli::cli_inform("Would you like to map optional columns? (y/n)")

      response <- tolower(trimws(readline("Map optional columns? [y/N]: ")))

      if (response %in% c("y", "yes")) {
        for (std_col in optional_cols) {
          pattern_def <- patterns[[std_col]]

          choices <- c(
            available_cols,
            "--- Skip ---"
          )

          choice_idx <- menu(
            choices = choices,
            title = sprintf("Select the column for '%s' (optional)", std_col)
          )

          if (choice_idx == 0 || grepl("^---.*Skip.*---$", choices[choice_idx])) {
            next
          }

          selected <- choices[choice_idx]
          mapping[[std_col]] <- selected
          available_cols <- setdiff(available_cols, selected)
        }
      }
    }
  }

  mapping
}


#' @title Prompt user to confirm or modify column mapping
#' @description Show the mapping and allow user to confirm or modify
#' @param mapping *\[list\]* The column mapping
#' @param required_cols *\[character\]* Required column names
#' @return *\[list\]* Confirmed mapping
confirm_column_mapping <- function(mapping, required_cols) {
  box::use(artma / libs / utils[get_verbosity])

  if (get_verbosity() >= 3) {
    cli::cli_h2("Column Mapping Summary")

    for (std_col in names(mapping)) {
      is_req <- if (std_col %in% required_cols) " (required)" else " (optional)"
      cli::cli_inform("{.field {std_col}}{is_req} -> {.val {mapping[[std_col]]}}")
    }

    missing <- setdiff(required_cols, names(mapping))
    if (length(missing) > 0) {
      cli::cli_alert_warning("Missing required columns: {.field {paste(missing, collapse = ', ')}}")
    }
  }

  mapping
}


#' @title Save column mapping to options
#' @description Save the confirmed mapping to the artma options
#' @param mapping *\[list\]* The column mapping (std_col -> data_col)
#' @param options_file_name *\[character, optional\]* Options file name
#' @return *\[invisible\]* NULL
save_column_mapping_to_options <- function(mapping, options_file_name = NULL) {
  box::use(artma / libs / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Saving column mapping to options")
  }

  # Convert mapping to options format (artma.data.colnames.*)
  user_input <- list()
  for (std_col in names(mapping)) {
    opt_key <- paste0("data.colnames.", std_col)
    user_input[[opt_key]] <- mapping[[std_col]]
  }

  # Update options
  if (!is.null(options_file_name)) {
    artma::options.modify(
      user_input = user_input,
      options_file_name = options_file_name
    )
  } else {
    # Set in current session
    for (key in names(user_input)) {
      opt_name <- paste0("artma.", key)
      options_list <- list()
      options_list[[opt_name]] <- user_input[[key]]
      do.call(options, options_list)
    }
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_success("Column mapping saved")
  }

  invisible(NULL)
}


#' @title Full interactive column mapping workflow
#' @description Complete workflow: recognize, interact, confirm, save
#' @param df *\[data.frame\]* The data frame
#' @param auto_mapping *\[list, optional\]* Pre-computed auto mapping
#' @param options_file_name *\[character, optional\]* Options file to save mapping
#' @param min_confidence *\[numeric\]* Minimum confidence for auto-recognition
#' @param force_interactive *\[logical\]* Force interactive mapping even if all columns recognized
#' @return *\[list\]* Final column mapping
column_mapping_workflow <- function(
    df,
    auto_mapping = NULL,
    options_file_name = NULL,
    min_confidence = 0.7,
    force_interactive = FALSE) {
  box::use(
    artma / data / column_recognition[
      recognize_columns,
      check_mapping_completeness,
      get_required_column_names
    ],
    artma / libs / utils[get_verbosity]
  )

  # Auto-recognize if not provided
  if (is.null(auto_mapping)) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Automatically recognizing columns...")
    }
    auto_mapping <- recognize_columns(df, min_confidence = min_confidence)
  }

  # Check completeness
  completeness <- check_mapping_completeness(auto_mapping)
  required_cols <- get_required_column_names()

  # Always present detected columns if any were found
  has_detected <- length(auto_mapping) > 0

  if (has_detected) {
    # Present detected columns and get user confirmation
    mapping <- interactive_column_mapping(
      df = df,
      auto_mapping = auto_mapping,
      required_only = TRUE,
      show_detected_first = TRUE
    )
  } else if (force_interactive || !completeness$complete) {
    # No detected columns, need to prompt for missing
    mapping <- interactive_column_mapping(
      df = df,
      auto_mapping = auto_mapping,
      required_only = TRUE,
      show_detected_first = FALSE
    )
  } else {
    # Edge case: no detected columns but all required are somehow present
    # (shouldn't happen, but handle gracefully)
    mapping <- auto_mapping
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("No columns detected, but all required columns are present")
    }
  }

  # Confirm mapping
  mapping <- confirm_column_mapping(mapping, required_cols)

  # Check if we have all required columns
  final_check <- check_mapping_completeness(mapping)
  if (!final_check$complete) {
    cli::cli_abort(c(
      "x" = "Column mapping incomplete",
      "i" = "Missing required columns: {.field {paste(final_check$missing, collapse = ', ')}}"
    ))
  }

  # Save to options if requested
  if (!is.null(options_file_name)) {
    save_column_mapping_to_options(mapping, options_file_name)
  }

  mapping
}


box::export(
  interactive_column_mapping,
  confirm_column_mapping,
  save_column_mapping_to_options,
  column_mapping_workflow,
  present_detected_mapping,
  format_mapping_display
)
