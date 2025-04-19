#' Assign NA to a column in a data frame
assign_na_col <- function(df, colname) {
  df[[colname]] <- rep(NA, nrow(df))
  return(df)
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
  raise_invalid_data_type_error
)
