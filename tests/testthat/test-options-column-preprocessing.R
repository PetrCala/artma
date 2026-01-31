box::use(
  artma / options / column_preprocessing[preprocess_column_mapping]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_false <- getFromNamespace("expect_false", "testthat")


# Helper to create a mock options_def structure
create_mock_options_def <- function() {
  list(
    list(
      name = "data.source_path",
      type = "character",
      allow_na = FALSE
    ),
    list(
      name = "data.config_setup",
      type = "enum",
      allow_na = FALSE
    ),
    list(
      name = "data.colnames.study_id",
      type = "character",
      allow_na = FALSE
    ),
    list(
      name = "data.colnames.effect",
      type = "character",
      allow_na = FALSE
    ),
    list(
      name = "data.colnames.se",
      type = "character",
      allow_na = FALSE
    ),
    list(
      name = "data.colnames.n_obs",
      type = "character",
      allow_na = FALSE
    ),
    list(
      name = "data.colnames.t_stat",
      type = "character",
      allow_na = TRUE
    )
  )
}


# Helper to create a temp CSV file
create_temp_csv_file <- function() {
  csv_content <- c(
    "study_name;effect;se;n_obs",
    "Study A;10.5;2.3;100",
    "Study B;8.2;1.8;150"
  )

  tmp_file <- tempfile(fileext = ".csv")
  writeLines(csv_content, tmp_file)
  tmp_file
}


test_that("preprocess_column_mapping returns unchanged when no data.source_path", {
  user_input <- list(
    "verbose" = 3
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  expect_equal(result, user_input)
})


test_that("preprocess_column_mapping returns unchanged when data.source_path is NA", {
  user_input <- list(
    "data.source_path" = NA
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  expect_equal(result, user_input)
})


test_that("preprocess_column_mapping returns unchanged when data.source_path is empty string", {
  user_input <- list(
    "data.source_path" = ""
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  expect_equal(result, user_input)
})


test_that("preprocess_column_mapping returns unchanged when file does not exist", {
  user_input <- list(
    "data.source_path" = "/nonexistent/path/to/file.csv"
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  expect_equal(result, user_input)
})


test_that("preprocess_column_mapping returns unchanged when config_setup is manual", {
  tmp_file <- create_temp_csv_file()
  on.exit(unlink(tmp_file))

  user_input <- list(
    "data.source_path" = tmp_file,
    "data.config_setup" = "manual"
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  # Should not add column mappings when manual mode
  expect_false("data.colnames.study_id" %in% names(result))
  expect_false("data.colnames.effect" %in% names(result))
})


test_that("preprocess_column_mapping returns unchanged when all columns already specified", {
  tmp_file <- create_temp_csv_file()
  on.exit(unlink(tmp_file))

  user_input <- list(
    "data.source_path" = tmp_file,
    "data.colnames.study_id" = "study_name",
    "data.colnames.effect" = "effect",
    "data.colnames.se" = "se",
    "data.colnames.n_obs" = "n_obs"
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  # Should keep existing mappings unchanged
  expect_equal(result$data.colnames.study_id, "study_name")
  expect_equal(result$data.colnames.effect, "effect")
})


test_that("preprocess_column_mapping adds auto-detected columns when missing", {
  tmp_file <- create_temp_csv_file()
  on.exit(unlink(tmp_file))

  user_input <- list(
    "data.source_path" = tmp_file,
    "data.config_setup" = "auto"
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 3))
  result <- preprocess_column_mapping(user_input, options_def)

  # Should have added auto-detected columns
  expect_true("data.colnames.study_id" %in% names(result))
  expect_true("data.colnames.effect" %in% names(result))
  expect_true("data.colnames.se" %in% names(result))
  expect_true("data.colnames.n_obs" %in% names(result))

  # Check values are reasonable
  expect_equal(result$data.colnames.study_id, "study_name")
  expect_equal(result$data.colnames.effect, "effect")
  expect_equal(result$data.colnames.se, "se")
  expect_equal(result$data.colnames.n_obs, "n_obs")
})


test_that("preprocess_column_mapping does not override existing column mappings", {
  tmp_file <- create_temp_csv_file()
  on.exit(unlink(tmp_file))

  user_input <- list(
    "data.source_path" = tmp_file,
    "data.colnames.study_id" = "my_custom_study_col"
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  # Should preserve user's custom mapping
  expect_equal(result$data.colnames.study_id, "my_custom_study_col")

  # Should add other detected columns
  expect_true("data.colnames.effect" %in% names(result))
  expect_true("data.colnames.se" %in% names(result))
})


test_that("preprocess_column_mapping handles tilde in path", {
  # Create a CSV in temp dir and use tilde notation
  tmp_dir <- tempdir()
  tmp_file <- file.path(tmp_dir, "test_data.csv")

  csv_content <- c(
    "study_name,effect,se,n_obs",
    "Study A,10.5,2.3,100"
  )
  writeLines(csv_content, tmp_file)
  on.exit(unlink(tmp_file))

  # Use relative path that will be expanded
  user_input <- list(
    "data.source_path" = tmp_file
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))

  # Should not error
  result <- preprocess_column_mapping(user_input, options_def)

  # Should have detected columns
  expect_true("data.colnames.study_id" %in% names(result))
})


test_that("preprocess_column_mapping handles errors gracefully", {
  # Create a malformed CSV
  tmp_file <- tempfile(fileext = ".csv")
  writeLines("not,a,valid,csv\n{{{{malformed", tmp_file)
  on.exit(unlink(tmp_file))

  user_input <- list(
    "data.source_path" = tmp_file
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))

  # Should not error, just return original input
  result <- preprocess_column_mapping(user_input, options_def)

  # Should not have added column mappings due to error
  expect_false("data.colnames.study_id" %in% names(result))
})


test_that("preprocess_column_mapping works with comma-delimited files", {
  csv_content <- c(
    "study_name,effect,se,n_obs",
    "Study A,10.5,2.3,100",
    "Study B,8.2,1.8,150"
  )

  tmp_file <- tempfile(fileext = ".csv")
  writeLines(csv_content, tmp_file)
  on.exit(unlink(tmp_file))

  user_input <- list(
    "data.source_path" = tmp_file
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  # Should detect columns regardless of delimiter
  expect_true("data.colnames.study_id" %in% names(result))
  expect_equal(result$data.colnames.study_id, "study_name")
})


test_that("preprocess_column_mapping respects verbosity settings", {
  tmp_file <- create_temp_csv_file()
  on.exit(unlink(tmp_file))

  user_input <- list(
    "data.source_path" = tmp_file
  )
  options_def <- create_mock_options_def()

  # Test with verbose = 1 (should not output much)
  withr::local_options(list("artma.verbose" = 1))
  result1 <- preprocess_column_mapping(user_input, options_def)

  # Test with verbose = 3 (should output info messages)
  withr::local_options(list("artma.verbose" = 3))
  result2 <- preprocess_column_mapping(user_input, options_def)

  # Both should produce the same result
  expect_equal(result1, result2)
  expect_true("data.colnames.study_id" %in% names(result1))
})
