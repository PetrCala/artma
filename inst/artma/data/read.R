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
    csv = read.csv(path),
    tsv = read.delim(path),
    xlsx = readxl::read_excel(path),
    # "xls",
    # "xlsm",
    # "json",
    # "dta",
    # "rds"
    raise_invalid_data_type_error(data_type)
  )

  df
}
