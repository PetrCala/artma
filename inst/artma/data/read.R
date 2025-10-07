#' @title Read data
#' @description Read data from a path. Returns a data frame with smart handling of various formats.
#' @param path *\[str, optional\]* The path to the data source. If NULL, the options data source path is used.
#' @return *\[data.frame\]* The data frame.
read_data <- function(path = NULL) {
  box::use(
    artma / data / utils[
      determine_df_type,
      raise_invalid_data_type_error,
      standardize_column_names
    ],
    artma / data / smart_detection[
      smart_read_csv,
      validate_df_structure
    ],
    artma / libs / utils[get_verbosity]
  )

  path <- if (!is.null(path)) path else getOption("artma.data.source_path")

  if (is.null(path) || !nzchar(path)) {
    cli::cli_abort("No data source path provided. Please set {.field artma.data.source_path} option.")
  }

  if (!file.exists(path)) {
    cli::cli_abort("Data file not found: {.path {path}}")
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Reading data from {.path {path}}")
  }

  df_type <- determine_df_type(path, should_validate = TRUE)

  # Read based on file type with enhanced handling
  df <- tryCatch(
    {
      switch(df_type,
        csv = smart_read_csv(path),
        tsv = smart_read_csv(path, delim = "\t"),
        xlsx = {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            cli::cli_abort("Package {.pkg readxl} is required to read Excel files. Install with: install.packages('readxl')")
          }
          readxl::read_excel(path, na = c("", "NA", "N/A", "na", "n/a", "NULL", "null"))
        },
        xls = {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            cli::cli_abort("Package {.pkg readxl} is required to read Excel files. Install with: install.packages('readxl')")
          }
          readxl::read_excel(path, na = c("", "NA", "N/A", "na", "n/a", "NULL", "null"))
        },
        xlsm = {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            cli::cli_abort("Package {.pkg readxl} is required to read Excel files. Install with: install.packages('readxl')")
          }
          readxl::read_excel(path, na = c("", "NA", "N/A", "na", "n/a", "NULL", "null"))
        },
        json = {
          if (!requireNamespace("jsonlite", quietly = TRUE)) {
            cli::cli_abort("Package {.pkg jsonlite} is required to read JSON files. Install with: install.packages('jsonlite')")
          }
          jsonlite::fromJSON(path)
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
        raise_invalid_data_type_error(df_type)
      )
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to read data from {.path {path}}",
        "i" = "File type: {.val {df_type}}",
        "i" = "Error: {e$message}"
      ))
    }
  )

  if (!is.data.frame(df)) {
    cli::cli_abort("Failed to read data from {.path {path}}. Expected a data frame but got {.type {class(df)}}.")
  }

  # Validate and clean structure
  df <- validate_df_structure(df, path)

  # Standardize column names based on user options
  df <- standardize_column_names(df)

  if (get_verbosity() >= 3) {
    cli::cli_alert_success("Data read successfully: {nrow(df)} rows, {ncol(df)} columns")
  }

  df
}

box::export(read_data)
