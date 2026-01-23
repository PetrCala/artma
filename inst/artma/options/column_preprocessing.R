#' @title Preprocess column mapping before options parsing
#' @description If data.source_path is provided in user_input, read the file and auto-detect column mappings.
#' This runs BEFORE the main options prompting, ensuring data detection happens first.
#' @param user_input [list] User-supplied values
#' @param options_def [list] Flattened options definitions from template
#' @return [list] Updated user_input with auto-detected column mappings
#' @keywords internal
preprocess_column_mapping <- function(user_input, options_def) {
  box::use(artma / libs / utils[get_verbosity])

  # Check if data source path is provided
  data_source_path <- user_input[["data.source_path"]]

  # If no data source or if column names are already fully specified, skip
  if (is.null(data_source_path) || !nzchar(data_source_path) || is.na(data_source_path)) {
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

  # Get required column names from options_def
  colname_opts <- options_def[vapply(options_def, function(opt) {
    startsWith(opt$name, "data.colnames.") && !isTRUE(opt$allow_na)
  }, logical(1))]

  # Check if all required column names are already in user_input
  required_colname_keys <- vapply(colname_opts, `[[`, character(1), "name")
  provided_colnames <- required_colname_keys[required_colname_keys %in% names(user_input)]

  if (length(provided_colnames) == length(required_colname_keys)) {
    # All required columns already specified
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
        artma / data / utils[determine_df_type, raise_invalid_data_type_error],
        artma / data / smart_detection[smart_read_csv, validate_df_structure],
        artma / data / column_recognition[recognize_columns],
        artma / data / interactive_mapping[interactive_column_mapping],
        artma / const[CONST],
        artma / libs / utils[get_verbosity]
      )

      # Read the data based on file type (same logic as read_data but without standardization)
      df_type <- determine_df_type(data_source_path, should_validate = TRUE)

      # Helper function to read Excel files (inline version of read_excel_file)
      read_excel_inline <- function(path) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          cli::cli_abort("Package {.pkg readxl} is required to read Excel files. Install with: install.packages('readxl')")
        }
        # Read all columns as text first, then we'll handle type detection if needed
        suppressWarnings(
          readxl::read_excel(
            path,
            col_types = "text",
            na = CONST$DATA$NA_STRINGS,
            trim_ws = TRUE
          )
        )
      }

      df <- switch(df_type,
        csv = smart_read_csv(data_source_path),
        tsv = smart_read_csv(data_source_path, delim = "\t"),
        xlsx = read_excel_inline(data_source_path),
        xls = read_excel_inline(data_source_path),
        xlsm = read_excel_inline(data_source_path),
        json = {
          if (!requireNamespace("jsonlite", quietly = TRUE)) {
            cli::cli_abort("Package {.pkg jsonlite} is required to read JSON files. Install with: install.packages('jsonlite')")
          }
          jsonlite::fromJSON(data_source_path)
        },
        dta = {
          if (!requireNamespace("haven", quietly = TRUE)) {
            cli::cli_abort("Package {.pkg haven} is required to read Stata files. Install with: install.packages('haven')")
          }
          as.data.frame(haven::read_dta(data_source_path))
        },
        rds = {
          obj <- readRDS(data_source_path)
          if (!is.data.frame(obj)) {
            cli::cli_abort("RDS file does not contain a data frame. Found: {.type {class(obj)}}")
          }
          obj
        },
        raise_invalid_data_type_error(df_type)
      )

      if (!is.data.frame(df)) {
        cli::cli_abort("Failed to read data from {.path {data_source_path}}. Expected a data frame but got {.type {class(df)}}.")
      }

      # Validate and clean structure (but don't standardize column names yet)
      df <- validate_df_structure(df, data_source_path)

      # Recognize columns
      auto_mapping <- recognize_columns(df, min_confidence = 0.7)

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

      # Add confirmed mappings to user_input (but don't override existing ones)
      for (std_col in names(mapping)) {
        opt_key <- paste0("data.colnames.", std_col)
        if (!opt_key %in% names(user_input)) {
          user_input[[opt_key]] <- mapping[[std_col]]
        }
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
