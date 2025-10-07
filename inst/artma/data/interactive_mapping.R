#' @title Interactive column mapping with climenu
#' @description Allow users to interactively map columns using climenu
#' @param df *\[data.frame\]* The data frame
#' @param auto_mapping *\[list\]* Automatically recognized mapping
#' @param required_only *\[logical\]* If TRUE, only ask for required columns
#' @return *\[list\]* User-confirmed column mapping
interactive_column_mapping <- function(df, auto_mapping = list(), required_only = TRUE) {
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

  cols_to_map <- if (required_only) {
    required_cols
  } else {
    names(patterns)
  }

  # Track missing required columns
  missing_required <- setdiff(required_cols, names(auto_mapping))

  # If all required columns are recognized and user doesn't want to override, skip
  if (length(missing_required) == 0 && required_only) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_success("All required columns automatically recognized")
    }
    return(auto_mapping)
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Interactive column mapping")
    if (length(auto_mapping) > 0) {
      cli::cli_inform("Auto-recognized: {.field {paste(names(auto_mapping), collapse = ', ')}}")
    }
    if (length(missing_required) > 0) {
      cli::cli_inform("Missing required: {.field {paste(missing_required, collapse = ', ')}}")
    }
  }

  mapping <- auto_mapping
  available_cols <- names(df)

  # Ask for each missing required column
  for (std_col in missing_required) {
    pattern_def <- patterns[[std_col]]
    help_text <- if (is.null(pattern_def$help)) std_col else pattern_def$help

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

  # Decide if we need interactive mapping
  needs_interactive <- force_interactive || !completeness$complete

  if (needs_interactive) {
    mapping <- interactive_column_mapping(
      df = df,
      auto_mapping = auto_mapping,
      required_only = TRUE
    )
  } else {
    mapping <- auto_mapping
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
  column_mapping_workflow
)
