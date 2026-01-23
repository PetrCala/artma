#' Assign NA to a column in a data frame
assign_na_col <- function(df, colname) {
  df[[colname]] <- rep(NA, nrow(df))
  df
}

#' @title Get standard column names
#' @description Get a vector of column names from the user options template.
#' @param filter_fn *\[function\]* A function to filter the option definitions.
#' @return *\[character\]* A vector of column names.
get_standardized_colnames <- function(filter_fn = function(x) TRUE) {
  box::use(
    artma / options / template[get_option_defs],
    artma / libs / polyfills[keep, map_chr, str_remove]
  )
  opt_path <- "data.colnames"
  defs <- get_option_defs(opt_path = opt_path)
  defs <- keep(defs, filter_fn)
  names <- map_chr(defs, "name")
  str_remove(names, paste0("^", opt_path, "\\."))
}

#' @title Get required columns
#' @description Get a vector of columns required by the analysis to exist in the data frame.
#' @return *\[character\]* A vector of column names.
get_required_colnames <- function() {
  get_standardized_colnames(filter_fn = ~ !isTRUE(.x$allow_na))
}

#' Get the number of studies in an analysis data frame.
#'
#' @param df *\[data.frame\]* The analysis data frame.
#' @param study_colname [str] The column name holding names of all studies.
#' `int` The number of studies.
get_number_of_studies <- function(df) {
  if (!"study" %in% colnames(df)) {
    cli::cli_abort("The data frame does not have a 'study' column.", class = "missing_study_column")
  }
  length(table(df$study))
}

#' @title Standardize column names
#' @description Standardize the column names of a data frame to a single source of truth set of column names.
#' @param df *\[data.frame\]* The data frame to standardize
#' @param auto_detect *\[logical\]* If TRUE and mapping not found, attempt auto-detection
#' @return *\[data.frame\]* The standardized data frame
standardize_column_names <- function(df, auto_detect = TRUE) {
  box::use(
    artma / libs / validation[validate],
    artma / options / utils[get_option_group],
    artma / libs / utils[get_verbosity]
  )

  validate(is.data.frame(df))

  names(df) <- make.names(names(df))

  # Try to get existing column mapping from options
  map <- tryCatch(
    {
      opt_map <- get_option_group("artma.data.colnames")
      # Filter out NA values
      opt_map[!is.na(opt_map)]
    },
    error = function(e) list()
  )

  map <- lapply(map, make.names) # Handle non-standard column names
  required_colnames <- get_required_colnames()

  # Check if we have all required columns mapped
  missing_required <- base::setdiff(required_colnames, names(map))

  # If mapping is incomplete and auto_detect is enabled, try to recognize columns
  if (length(missing_required) > 0 && auto_detect) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Column mapping incomplete, attempting automatic recognition...")
    }

    box::use(
      artma / data / column_recognition[recognize_columns],
      artma / data / interactive_mapping[column_mapping_workflow]
    )

    # Get automatic recognition
    auto_mapping <- recognize_columns(df, min_confidence = 0.7)

    # Always present detected columns to user for confirmation
    config_setup <- getOption("artma.data.config_setup", "auto")

    if (length(auto_mapping) > 0) {
      # Columns were detected - always present them for confirmation
      if (get_verbosity() >= 3) {
        cli::cli_alert_info("Starting column mapping workflow...")
      }
      final_mapping <- column_mapping_workflow(
        df = df,
        auto_mapping = auto_mapping,
        options_file_name = getOption("artma.options_file_name"),
        force_interactive = (config_setup == "manual")
      )
    } else {
      # No columns detected, go straight to interactive mapping
      if (get_verbosity() >= 3) {
        cli::cli_alert_info("No columns automatically detected, starting interactive mapping...")
      }
      final_mapping <- column_mapping_workflow(
        df = df,
        auto_mapping = list(),
        options_file_name = getOption("artma.options_file_name"),
        force_interactive = TRUE
      )
    }

    # Update the map with the final mapping
    for (std_col in names(final_mapping)) {
      map[[std_col]] <- make.names(final_mapping[[std_col]])
    }
  }

  # Check that every required column is mapped
  missing_required <- base::setdiff(required_colnames, names(map))
  if (length(missing_required)) {
    cli::cli_abort(c(
      "x" = "Missing mapping for required columns: {.val {missing_required}}",
      "i" = "Please specify column mappings in your options file or enable automatic detection"
    ))
  }

  # Check that every required column exists in the data frame
  mapped_cols <- unlist(unname(map[names(map) %in% required_colnames]))
  absent_required <- mapped_cols[!mapped_cols %in% names(df)]
  if (length(absent_required)) {
    cli::cli_abort(c(
      "x" = "These required columns are absent in the data frame: {.val {absent_required}}",
      "i" = "Available columns: {.val {paste(names(df), collapse = ', ')}}"
    ))
  }

  # Rename columns to standardized names - only when present in the data frame
  reverse_map <- stats::setNames(names(map), unlist(map))
  names(df)[names(df) %in% names(reverse_map)] <- reverse_map[names(df)[names(df) %in% names(reverse_map)]]

  missing_required <- setdiff(required_colnames, colnames(df))
  if (length(missing_required)) {
    cli::cli_abort("Failed to standardize the following columns: {.val {missing_required}}")
  }

  df
}


#' @description Raise an error for an invalid data type.
#' @param data_type [str] The invalid data type.
#' @return `NULL`
raise_invalid_data_type_error <- function(data_type) {
  box::use(artma / const[CONST])

  cli::cli_abort(c(
    cli::format_inline("{CONST$PACKAGE_NAME} does not currently support the following data type {.val {data_type}}."),
    cli::format_inline("Supported data types are {.val {CONST$DATA$TYPES}}.")
  ))
}


#' @title Determine data type
#' @description Determine a data type based on its path.
#' @param path [str] The path to the data.
#' `str` The data type
determine_df_type <- function(path, should_validate = TRUE) {
  box::use(
    artma / const[CONST],
    artma / libs / validation[validate]
  )

  validate(is.character(path))

  if (!file.exists(path)) {
    cli::cli_abort("The specified data file path {.path {path}} is invalid. No such file found.")
  }

  file_extension <- tools::file_ext(path)

  if (should_validate && !(file_extension %in% CONST$DATA$TYPES)) {
    raise_invalid_data_type_error(file_extension)
  }

  file_extension
}

determine_vector_type <- function(data, recognized_data_types = NULL) {
  box::use(
    artma / libs / validation[validate]
  )

  validate(is.vector(data))

  data_type <- if (length(data[!is.na(data)]) == 0) {
    "empty"
  } else if (is.logical(data)) {
    "dummy"
  } else if (is.character(data)) {
    "category"
  } else if (is.numeric(data)) {
    clean_data <- data[!is.na(data)]
    if (all(clean_data == floor(clean_data))) {
      if (all(clean_data >= 0 & clean_data <= 100)) {
        "perc"
      } else {
        "int"
      }
    } else {
      "float"
    }
  } else {
    "unknown"
  }

  if (!is.null(recognized_data_types)) {
    if (!(data_type %in% recognized_data_types)) {
      cli::cli_abort("The data type {.val {data_type}} is not supported. Please use one of the following types: {.val {recognized_data_types}}.")
    }
  }

  data_type
}

box::export(
  assign_na_col,
  determine_df_type,
  determine_vector_type,
  get_number_of_studies,
  get_required_colnames,
  get_standardized_colnames,
  raise_invalid_data_type_error,
  standardize_column_names
)
