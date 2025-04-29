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

  mock_df_path <- PATHS$FILE_MOCKS_TMP_DATA
  mock_df <- create_mock_df(
    with_file_creation = TRUE,
    file_path = mock_df_path
  )

  # Artma methods
  options_file_name <- CONST$MOCKS$TMP_OPTIONS_FILE_NAME

  withr::local_options(list("artma.verbose" = 3))
  artma::options.delete(options_file_name = options_file_name, skip_confirmation = TRUE)
  artma::options.create(
    options_file_name = options_file_name,
    user_input = list(
      "verbose" = get_verbosity(),
      "data.source_path" = mock_df_path,
      "data.na_handling" = "stop",
      "data.config_setup" = "auto",
      "data.colnames.obs_id" = NA,
      "data.colnames.study" = "study",
      "data.colnames.study_id" = NA,
      "data.colnames.effect" = "effect",
      "data.colnames.se" = "se",
      "data.colnames.t_stat" = NA,
      "data.colnames.n_obs" = "n_obs",
      "data.colnames.study_size" = NA,
      "data.colnames.reg_dof" = NA,
      "data.colnames.precision" = NA
    ),
    should_overwrite = TRUE
  )

  artma::run(
    methods = c("all"),
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
