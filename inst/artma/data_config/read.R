#' @title Get data config
#' @description Get the fully-resolved data config by merging the base config
#'   (auto-generated from the dataframe) with sparse overrides from the options file.
#'   If the dataframe source is not available but overrides exist, returns overrides as-is.
#' @param create_if_missing *\[logical\]* Whether to create the data config if
#'   the dataframe source path is not available. Defaults to `TRUE`.
#' @param fix_if_invalid *\[logical\]* Whether to fix the data config if it is
#'   invalid. Defaults to `FALSE`.
#' @return *\[list\]* The fully-resolved data config.
get_data_config <- function(
    create_if_missing = TRUE,
    fix_if_invalid = FALSE) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity],
    artma / data_config / defaults[build_base_config],
    artma / data_config / resolve[
      read_df_for_config,
      merge_config
    ]
  )

  validate(
    is.logical(create_if_missing),
    is.logical(fix_if_invalid)
  )

  # Read sparse overrides from options
  overrides <- getOption("artma.data.config")
  if (!is.list(overrides)) overrides <- list()

  # Try to build base config from the dataframe
  df <- tryCatch(
    read_df_for_config(),
    error = function(e) {
      if (get_verbosity() >= 4) {
        cli::cli_inform(
          "Could not read dataframe for config resolution: {e$message}"
        )
      }
      NULL
    }
  )

  if (is.null(df)) {
    # No dataframe available -- return overrides as-is
    if (length(overrides) > 0) {
      return(overrides)
    }
    if (!create_if_missing) {
      cli::cli_abort(
        "The data config cannot be resolved: no dataframe source and no overrides."
      )
    }
    return(list())
  }

  base_config <- build_base_config(df)

  # Merge sparse overrides on top of base
  merge_config(base_config, overrides)
}

box::export(get_data_config)
