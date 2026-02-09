box::use(
  testthat[test_that, expect_equal],
  artma / testing / mocks / index[MOCKS]
)

test_that("prepare_data reads source data only once on a cache-miss execution", {
  df <- MOCKS$create_mock_df(seed = 42)
  tmp_file <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp_file, row.names = FALSE)
  withr::defer(unlink(tmp_file), envir = parent.frame())

  # Seed computed-column entries so prepare_data() does not attempt to persist
  # data-config updates to a runtime options file in this unit test.
  precomputed_overrides <- list(
    t_stat = list(var_name = "t_stat", is_computed = TRUE),
    study_size = list(var_name = "study_size", is_computed = TRUE),
    reg_dof = list(var_name = "reg_dof", is_computed = TRUE),
    precision = list(var_name = "precision", is_computed = TRUE)
  )

  withr::local_options(list(
    "artma.cache.use_cache" = FALSE,
    "artma.data.source_path" = tmp_file,
    "artma.data.colnames.study_id" = "study_id",
    "artma.data.colnames.effect" = "effect",
    "artma.data.colnames.se" = "se",
    "artma.data.colnames.n_obs" = "n_obs",
    "artma.data.config" = precomputed_overrides,
    "artma.data.config_setup" = "auto",
    "artma.data.na_handling" = "remove",
    "artma.calc.se_zero_handling" = "ignore",
    "artma.verbose" = 3
  ))

  box::use(
    artma / data / index[prepare_data],
    artma / data_config / resolve[invalidate_df_cache]
  )

  invalidate_df_cache()

  msgs <- testthat::capture_messages(prepare_data())
  read_msgs <- grep("Reading data from", msgs, value = TRUE)

  expect_equal(length(read_msgs), 1L)

  invalidate_df_cache()
})
