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
      "cache.use_cache" = TRUE,
      "data.source_path" = mock_df_path,
      "data.na_handling" = "stop",
      "data.config_setup" = "auto" # "auto" will recognize columns automatically, "manual" for interactive
      # Column mappings will be auto-detected - no need to specify them manually!
      # If auto-detection fails, the package will prompt interactively
    ),
    should_overwrite = TRUE
  )

  artma::run(
    # methods = c("all"),
    methods = c("nonlinear_tests"),
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


#' @keywords internal
test_column_recognition <- function() {
  # Test the new column recognition system independently
  box::use(
    artma / data / smart_detection[smart_read_csv, detect_delimiter],
    artma / data / column_recognition[recognize_columns],
    artma / libs / utils[get_verbosity]
  )

  withr::local_options(list("artma.verbose" = 4))

  data_path <- "~/code/meta/artma/local/data/data_set_master_thesis_cala.csv"

  # Test delimiter detection
  cli::cli_h1("Testing delimiter detection")
  delim <- detect_delimiter(data_path)
  cli::cli_alert_info("Detected delimiter: {.val {delim}}")

  # Test smart CSV reading
  cli::cli_h1("Testing smart CSV reading")
  df <- smart_read_csv(data_path)
  cli::cli_alert_success("Read {nrow(df)} rows and {ncol(df)} columns")

  # Test column recognition
  cli::cli_h1("Testing column recognition")
  mapping <- recognize_columns(df, min_confidence = 0.7)

  cli::cli_h2("Recognized columns")
  for (std_col in names(mapping)) {
    cli::cli_inform("{.field {std_col}} -> {.val {mapping[[std_col]]}}")
  }

  invisible(list(df = df, mapping = mapping))
}

test()
# nolint end
