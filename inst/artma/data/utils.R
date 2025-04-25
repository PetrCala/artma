#' Assign NA to a column in a data frame
assign_na_col <- function(df, colname) {
  df[[colname]] <- rep(NA, nrow(df))
  return(df)
}

#' @title Get standard column names
#' @description Get a vector of column names from the user options template.
#' @param filter_fn *\[function\]* A function to filter the option definitions.
#' @return *\[character\]* A vector of column names.
get_standardized_colnames <- function(filter_fn = function(x) TRUE) {
  box::use(artma / options / template[get_option_defs])
  opt_path <- "data.colnames"
  defs <- get_option_defs(opt_path = opt_path)
  defs <- purrr::keep(defs, filter_fn)
  names <- purrr::map_chr(defs, "name")
  stringr::str_remove(names, paste0("^", opt_path, "\\."))
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
  return(length(table(df$study)))
}

#' @title Standardize column names
#' @description Standardize the column names of a data frame to a single source of truth set of column names.
#' @param df *\[data.frame\]* The data frame to standardize
#' @return *\[data.frame\]* The standardized data frame
standardize_column_names <- function(df) {
  box::use(
    artma / libs / validation[validate],
    artma / options / utils[get_option_group]
  )

  validate(is.data.frame(df))

  names(df) <- make.names(names(df))

  map <- get_option_group("artma.data.colnames")
  map <- lapply(map, make.names) # Handle non-standard column names
  required_colnames <- get_required_colnames()

  # Check that every required column is mapped in the user options file
  missing_required <- base::setdiff(required_colnames, names(map))
  if (length(missing_required)) {
    cli::cli_abort("Missing mapping for required columns: {.val {missing_required}}")
  }

  # Check that every required column exists in the data frame
  mapped_cols <- unlist(unname(map[names(map) %in% required_colnames]))
  absent_required <- mapped_cols[!mapped_cols %in% names(df)]
  if (length(absent_required)) {
    cli::cli_abort("These required columns are absent in the data frame: {.val {absent_required}}")
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

  cli::cli_abort(
    glue::glue(
      cli::format_inline("{CONST$PACKAGE_NAME} does not currently support the following data type {.val {data_type}}."),
      cli::format_inline("Supported data types are {.val {CONST$DATA$TYPES}}."),
      .sep = "\n"
    )
  )
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
