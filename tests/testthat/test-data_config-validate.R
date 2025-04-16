box::use(
  testthat[test_that, expect_true, expect_false],
  artma / data_config / validate[data_config_filename_is_valid]
)

test_that("data_config_filename_is_valid returns TRUE for valid filenames", {
  expect_true(data_config_filename_is_valid("dataset1.config.json"))
  expect_true(data_config_filename_is_valid("experiment_2.config.json"))
})

test_that("data_config_filename_is_valid returns FALSE for invalid filenames", {
  expect_false(data_config_filename_is_valid("test-config.json"))
  expect_false(data_config_filename_is_valid("dataset.json"))
})

test_that("validate_data_config_filename returns FALSE for empty strings and other invalid inputs", {
  expect_false(data_config_filename_is_valid(""))
  expect_false(data_config_filename_is_valid(NULL))
  expect_false(data_config_filename_is_valid(NA))
  expect_false(data_config_filename_is_valid(1))
  expect_false(data_config_filename_is_valid(TRUE))
  expect_false(data_config_filename_is_valid(FALSE))
})
