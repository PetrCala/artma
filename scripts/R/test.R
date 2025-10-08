# nolint start

#' @keywords internal
test <- function() {
  # Local methods
  box::use(
    artma / const[CONST],
    artma / paths[PATHS],
    artma / testing / mocks / mock_df[create_mock_df],
    artma / data_config / parse[parse_df_into_data_config],
    artma / libs / utils[get_verbosity]
  )

  # mock_df_path <- PATHS$FILE_MOCKS_TMP_DATA
  mock_df_path <- "~/code/meta/artma/local/data/data_set_master_thesis_cala.csv"
  # mock_df <- create_mock_df(
  #   with_file_creation = TRUE,
  #   file_path = mock_df_path
  # )

  # Artma methods
  options_file_name <- CONST$MOCKS$TMP_OPTIONS_FILE_NAME

  withr::local_options(list("artma.verbose" = 3))
  artma::options.delete(options_file_name = options_file_name, skip_confirmation = TRUE)

  # New workflow: much simpler column specification
  # The package will now automatically recognize columns and only prompt for missing ones
  artma::options.create(
    options_file_name = options_file_name,
    user_input = list(
      "verbose" = get_verbosity(),
      "cache.use_cache" = FALSE, # Disable cache to see progress bars
      "data.source_path" = mock_df_path,
      "data.winsorization_level" = 0.1,
      "data.na_handling" = "stop",
      "data.config_setup" = "auto" # "auto" will recognize columns automatically, "manual" for interactive
      # Column mappings will be auto-detected - no need to specify them manually!
      # If auto-detection fails, the package will prompt interactively
    ),
    should_overwrite = TRUE
  )

  artma::run(
    # methods = c("all"),
    methods = c("bma"),
    options_file_name = options_file_name
  )

  # artma::main(
  #   options = options_file_name,
  #   FUN = function() {
  #     box::use(artma / data_config / read[get_data_config])
  #     config <- get_data_config(fix_if_invalid = TRUE)
  #   }
  # )
}

test()
# nolint end
