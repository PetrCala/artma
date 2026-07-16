#' @title Read a data file by type
#' @description Single dispatch that reads a data file into a raw data frame
#'   based on its detected type. No normalization or column standardization is
#'   applied here; callers run \code{normalize_read_df} and
#'   \code{validate_df_structure} afterwards. Excel files are read entirely as
#'   text so that column typing happens through the same coercion path as every
#'   other format (see \code{coerce_df_columns}).
#' @param path *\[character\]* Path to the data file.
#' @param type *\[character\]* The data type (file extension) to read as.
#' @return *\[data.frame\]* The raw data frame with original column names.
#' @keywords internal
read_by_type <- function(path, type) {
  box::use(
    artma / data / utils[raise_invalid_data_type_error],
    artma / data / smart_detection[smart_read_csv]
  )

  df <- switch(type,
    csv = smart_read_csv(path),
    tsv = smart_read_csv(path, delim = "\t"),
    xlsx = ,
    xls = ,
    xlsm = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        cli::cli_abort("Package {.pkg readxl} is required to read Excel files. Install with: install.packages('readxl')")
      }
      # Read every column as text; shared normalization coerces types afterwards.
      as.data.frame(
        suppressWarnings(
          readxl::read_excel(path, col_types = "text", na = character(0), trim_ws = TRUE)
        ),
        stringsAsFactors = FALSE
      )
    },
    json = {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        cli::cli_abort("Package {.pkg jsonlite} is required to read JSON files. Install with: install.packages('jsonlite')")
      }
      # JSON stays a first-class format but must flatten to a tabular record set.
      parsed <- jsonlite::fromJSON(path, flatten = TRUE)
      if (!is.data.frame(parsed)) {
        cli::cli_abort(c(
          "x" = "JSON file {.path {path}} did not flatten to a data frame.",
          "i" = "Provide the data as an array of records (objects with the same fields), for example {.code [{{\"study_id\": 1, \"effect\": 0.5}}]}."
        ))
      }
      parsed
    },
    dta = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        cli::cli_abort("Package {.pkg haven} is required to read Stata files. Install with: install.packages('haven')")
      }
      as.data.frame(haven::read_dta(path))
    },
    rds = {
      obj <- readRDS(path)
      if (!is.data.frame(obj)) {
        cli::cli_abort("RDS file does not contain a data frame. Found: {.type {class(obj)}}")
      }
      obj
    },
    raise_invalid_data_type_error(type)
  )

  if (!is.data.frame(df)) {
    cli::cli_abort("Failed to read data from {.path {path}}. Expected a data frame but got {.type {class(df)}}.")
  }

  df
}


#' @title Read and normalize a data file
#' @description Read a file through the single \code{read_by_type} dispatch, then
#'   apply the shared post-read normalization and structural validation. Used by
#'   both \code{read_data} and the options-file column preprocessing so that both
#'   paths read a given file identically.
#' @param path *\[character\]* Path to the data file (required).
#' @return *\[data.frame\]* The normalized, validated data frame with original
#'   column names.
#' @keywords internal
read_file <- function(path) {
  box::use(
    artma / data / utils[determine_df_type],
    artma / data / normalize[normalize_read_df],
    artma / data / smart_detection[validate_df_structure]
  )

  if (is.null(path) || !nzchar(path)) {
    cli::cli_abort("No data source path provided.")
  }

  if (!file.exists(path)) {
    cli::cli_abort("Data file not found: {.path {path}}")
  }

  df_type <- determine_df_type(path, should_validate = TRUE)

  df <- tryCatch(
    read_by_type(path, df_type),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to read data from {.path {path}}",
        "i" = "File type: {.val {df_type}}",
        "i" = "Error: {e$message}"
      ))
    }
  )

  df <- normalize_read_df(df)
  validate_df_structure(df, path)
}


#' @title Read data
#' @description Read data from a path. Returns a raw data frame with the
#'   original column names, validated and normalized but not standardized.
#'   Column name standardization is applied downstream (after schema
#'   reconciliation).
#' @param path *\[str, optional\]* The path to the data source. If NULL, the options data source path is used.
#' @return *\[data.frame\]* The data frame with original column names.
read_data <- function(path = NULL) {
  box::use(artma / libs / core / utils[get_verbosity])

  path <- if (!is.null(path)) path else getOption("artma.data.source_path", NULL)

  if (is.null(path) || !nzchar(path)) {
    cli::cli_abort("No data source path provided. Please set {.field artma.data.source_path} option.")
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Reading data from {.path {path}}")
  }

  df <- read_file(path)

  if (get_verbosity() >= 3) {
    cli::cli_alert_success("Data read successfully: {nrow(df)} rows, {ncol(df)} columns")
  }

  df
}

box::export(read_by_type, read_file, read_data)
