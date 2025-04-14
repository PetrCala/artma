#' Assign NA to a column in a data frame
#' @export
assign_na_col <- function(df, colname) {
  df[[colname]] <- rep(NA, nrow(df))
  return(df)
}

#' Get the number of studies in an analysis data frame.
#'
#' @param df *\[data.frame\]* The analysis data frame.
#' @param study_colname [str] The column name holding names of all studies.
#' `int` The number of studies.
#' @export
get_number_of_studies <- function(df) {
  if (!"study" %in% colnames(df)) {
    cli::cli_abort("The data frame does not have a 'study' column.", class = "missing_study_column")
  }
  return(length(table(df$study)))
}

#' @description Raise an error for an invalid data type.
#' @param data_type [str] The invalid data type.
#' @return `NULL`
#' @export
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
#' @export
determine_data_type <- function(path, should_validate = TRUE) {
  box::use(
    artma / const[CONST],
    artma / libs / validation[validate]
  )

  validate(is.character(path))

  if (!file.exists(path)) cli::cli_abort("The specified data file path {.path {path}} is invalid. No such file found.")

  file_extension <- tools::file_ext(path)

  if (should_validate && !(file_extension %in% CONST$DATA$TYPES)) {
    raise_invalid_data_type_error(file_extension)
  }

  file_extension
}
