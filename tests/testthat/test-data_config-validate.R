box::use(
  testthat[test_that, expect_true, expect_false],
  artma / data_config / validate[validate_data_config_filename]
)

test_that("validate_data_config_filename returns TRUE for valid filenames", {
  expect_true(validate_data_config_filename("dataset1.config.json"))
  expect_true(validate_data_config_filename("experiment_2.config.json"))
})

test_that("validate_data_config_filename returns FALSE for invalid filenames", {
  expect_false(validate_data_config_filename("test-config.json"))
  expect_false(validate_data_config_filename("dataset.json"))
})

test_that("validate_data_config_filename returns FALSE for empty strings and other invalid inputs", {
  expect_false(validate_data_config_filename(""))
  expect_false(validate_data_config_filename(NULL))
  expect_false(validate_data_config_filename(NA))
  expect_false(validate_data_config_filename(1))
  expect_false(validate_data_config_filename(TRUE))
  expect_false(validate_data_config_filename(FALSE))
})
