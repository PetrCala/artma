#' @title Preprocess column mapping before options parsing
#' @description If data.source_path is provided in user_input, read the file and auto-detect column mappings.
#' This runs BEFORE the main options prompting, ensuring data detection happens first.
#' Confirmed mappings are stored as role records in the unified per-column store
#' (`data.columns`), each carrying a `source_name` field.
#' @param user_input [list] User-supplied values
#' @param options_def [list] Flattened options definitions from template
#' @return [list] Updated user_input with auto-detected column mappings
#' @keywords internal
preprocess_column_mapping <- function(user_input, options_def) {
  box::use(artma / libs / core / utils[get_verbosity])

  # Check if data source path is provided
  data_source_path <- user_input[["data.source_path"]]

  # If no data source, skip
  if (is.null(data_source_path) || !nzchar(data_source_path) || is.na(data_source_path)) {
    return(user_input)
  }

  # If the unified column store is already supplied, respect it as-is
  existing_columns <- user_input[["data.columns"]]
  if (is.list(existing_columns) && length(existing_columns) > 0) {
    return(user_input)
  }

  # Expand path
  data_source_path <- path.expand(data_source_path)

  # Check if file exists
  if (!file.exists(data_source_path)) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("Data source path {.path {data_source_path}} not found. Skipping column auto-detection.")
    }
    return(user_input)
  }

  # Check config setup mode
  config_setup <- user_input[["data.config_setup"]]
  if (!is.null(config_setup) && config_setup == "manual") {
    # User wants manual configuration, skip auto-detection
    return(user_input)
  }

  if (get_verbosity() >= 3) {
    cli::cli_h2("Auto-detecting column mappings")
    cli::cli_alert_info("Reading data from {.path {data_source_path}}")
  }

  # Read and recognize columns with user confirmation
  tryCatch(
    {
      box::use(
        artma / data / read[read_file],
        artma / data / column_recognition[recognize_columns],
        artma / data / interactive_mapping[interactive_column_mapping],
        artma / libs / core / utils[get_verbosity]
      )

      # Read through the same dispatch and normalization as read_data, without
      # standardizing column names yet.
      df <- read_file(data_source_path)

      # Recognize columns via the shared matching engine
      auto_mapping <- recognize_columns(df)

      # Present detected columns to user for confirmation
      # This will show detected columns and allow user to accept, modify, or skip optional
      if (length(auto_mapping) > 0) {
        mapping <- interactive_column_mapping(
          df = df,
          auto_mapping = auto_mapping,
          required_only = TRUE,
          show_detected_first = TRUE
        )
      } else {
        # No columns detected, will prompt later during options creation
        mapping <- list()
      }

      # Convert the confirmed mapping into unified role records.
      # Skip any NULL, NA, or empty string values to prevent validation errors.
      # Identity mappings (a column already carrying the standard name) are not
      # stored: the sparse store only holds genuine renames.
      records <- list()
      for (std_col in names(mapping)) {
        val <- mapping[[std_col]]
        if (is.null(val) || (length(val) == 1 && is.na(val)) || !nzchar(trimws(val))) {
          if (get_verbosity() >= 2) {
            cli::cli_alert_warning(
              "Skipping invalid mapping for {.field {std_col}}: value is NULL, NA, or empty"
            )
          }
          next
        }
        if (identical(trimws(val), std_col)) next
        records[[std_col]] <- list(source_name = trimws(val))
      }

      if (length(records) > 0) {
        user_input[["data.columns"]] <- records
      }
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("Failed to auto-detect columns: {e$message}")
        cli::cli_alert_info("You will be prompted for column mappings")
      }
    }
  )

  user_input
}


box::export(preprocess_column_mapping)
