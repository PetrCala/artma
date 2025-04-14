#' @title Read data
#' @description Read data from a path. Returns a data frame.
#' @param path [str] The path to the data source. If NULL, the options data source path is used.
#' @return *\[data.frame\]* The data frame.
#' @export
read_data <- function(path = NULL) {
  box::use(
    artma / data / utils[
      determine_data_type,
      raise_invalid_data_type_error
    ]
  )

  path <- if (!is.null(path)) path else getOption("artma.data.source_path")

  data_type <- determine_data_type(path, should_validate = TRUE)

  df <- switch(data_type,
    csv = utils::read.csv(path),
    tsv = utils::read.delim(path),
    xlsx = readxl::read_excel(path),
    # "xls",
    # "xlsm",
    # "json",
    # "dta",
    # "rds"
    raise_invalid_data_type_error(data_type)
  )

  if (!is.data.frame(df)) {
    cli::cli_abort("Failed to read data from {.path {path}}. Expected a data frame but got {.type {class(df)}}.")
  }

  if (nrow(df) == 0) {
    cli::cli_abort("The data frame read from {.path {path}} is empty.")
  }

  if (ncol(df) == 0) {
    cli::cli_abort("The data frame read from {.path {path}} has no columns.")
  }

  df
}
