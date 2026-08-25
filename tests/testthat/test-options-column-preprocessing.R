box::use(
  testthat[expect_equal, expect_false, expect_true, test_that]
)

box::use(
  artma / options / column_preprocessing[preprocess_column_mapping]
)


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
      name = "data.columns",
      type = "list",
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


# When there is nothing to auto-detect (no source path, NA/empty path, or a
# path that does not resolve to a file), the mapping is returned unchanged.
test_that("preprocess_column_mapping returns input unchanged when there is nothing to auto-detect", {
  options_def <- create_mock_options_def()
  withr::local_options(list("artma.verbose" = 1))

  cases <- list(
    "no data.source_path" = list("verbose" = 3),
    "data.source_path is NA" = list("data.source_path" = NA),
    "data.source_path is empty string" = list("data.source_path" = ""),
    "file does not exist" = list("data.source_path" = "/nonexistent/path/to/file.csv")
  )

  for (label in names(cases)) {
    user_input <- cases[[label]]
    expect_equal(
      preprocess_column_mapping(user_input, options_def),
      user_input,
      info = label
    )
  }
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

  # Should not add column records when manual mode
  expect_false("data.columns" %in% names(result))
})


test_that("preprocess_column_mapping returns unchanged when the column store is already specified", {
  tmp_file <- create_temp_csv_file()
  on.exit(unlink(tmp_file))

  provided_columns <- list(
    study_id = list(source_name = "study_name"),
    effect = list(source_name = "effect"),
    se = list(source_name = "se"),
    n_obs = list(source_name = "n_obs")
  )
  user_input <- list(
    "data.source_path" = tmp_file,
    "data.columns" = provided_columns
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  # Should keep the provided store unchanged
  expect_equal(result$data.columns, provided_columns)
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

  # Should have added auto-detected role records to the unified store.
  # Only genuine renames are stored; columns already carrying the standard
  # name (effect, se, n_obs) need no record.
  records <- result$data.columns
  expect_true(is.list(records))
  expect_equal(records$study_id$source_name, "study_name")
  expect_false("effect" %in% names(records))
  expect_false("se" %in% names(records))
  expect_false("n_obs" %in% names(records))
})


test_that("preprocess_column_mapping does not override a user-supplied column store", {
  tmp_file <- create_temp_csv_file()
  on.exit(unlink(tmp_file))

  user_input <- list(
    "data.source_path" = tmp_file,
    "data.columns" = list(
      study_id = list(source_name = "my_custom_study_col")
    )
  )
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  # Should preserve the user's store as-is
  expect_equal(
    result$data.columns,
    list(study_id = list(source_name = "my_custom_study_col"))
  )
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

  # Should not have added column records due to error
  expect_false("data.columns" %in% names(result))
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
  expect_true("study_id" %in% names(result$data.columns))
  expect_equal(result$data.columns$study_id$source_name, "study_name")
})


test_that("preprocess_column_mapping detects columns in a Latin-1 encoded file", {
  # Regression: a citation-style study label carrying a raw 0xFC byte ("Müller"
  # in Windows-1252) made is_likely_study_key()'s character-class regexes error
  # out, the whole detection step was swallowed by the surrounding tryCatch,
  # and the options file was written with no mapping at all. Encoding is now
  # repaired at the read boundary, so detection runs normally.
  tmp_file <- tempfile(fileext = ".csv")
  con <- file(tmp_file, open = "wb")
  writeBin(charToRaw(paste0(paste(c(
    "study_name,effect,standard_error,sample_size",
    "M\xfcller et al. (2011),0.5,0.1,100",
    "Smith (2015),0.7,0.2,150",
    "Jones (2019),0.3,0.05,220"
  ), collapse = "\n"), "\n")), con)
  close(con)
  on.exit(unlink(tmp_file))

  user_input <- list("data.source_path" = tmp_file)
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  expect_equal(result$data.columns$study_id$source_name, "study_name")
  expect_equal(result$data.columns$se$source_name, "standard_error")
  expect_equal(result$data.columns$n_obs$source_name, "sample_size")
})


test_that("preprocess_column_mapping does not guess a mapping for an undetectable required column", {
  # n_obs is required but absent from the data. In a non-interactive session
  # (such as this test run) the flow must leave it unmapped rather than fall
  # back to climenu's first menu entry, which used to record
  # n_obs: source_name: effect.
  csv_content <- c(
    "effect,se,study_id",
    "0.5,0.1,Study A",
    "0.7,0.2,Study B"
  )
  tmp_file <- tempfile(fileext = ".csv")
  writeLines(csv_content, tmp_file)
  on.exit(unlink(tmp_file))

  user_input <- list("data.source_path" = tmp_file)
  options_def <- create_mock_options_def()

  withr::local_options(list("artma.verbose" = 1))
  result <- preprocess_column_mapping(user_input, options_def)

  expect_false("n_obs" %in% names(result$data.columns))
})


test_that("non-interactive options_create leaves an undetectable required column unmapped", {
  box::use(
    artma / options / files[options_file_path, read_options_file]
  )

  csv_content <- c(
    "effect,se,study_id",
    "0.5,0.1,Study A",
    "0.7,0.2,Study B"
  )
  tmp_file <- tempfile(fileext = ".csv")
  writeLines(csv_content, tmp_file)
  on.exit(unlink(tmp_file))

  tmp_dir <- withr::local_tempdir()
  file_name <- "missing-nobs.yaml"

  withr::local_options(list("artma.verbose" = 1))

  artma::options_create(
    options_file_name = file_name,
    options_dir = tmp_dir,
    user_input = list("data.source_path" = tmp_file),
    should_validate = FALSE,
    should_overwrite = TRUE
  )

  written <- read_options_file(options_file_path(tmp_dir, file_name))
  columns <- written$data$columns

  # With nothing to record, the columns node stays empty (read back as NA).
  # The menu-default fallback used to record n_obs: source_name: effect here,
  # which made the next run rename effect to n_obs and abort.
  expect_false(is.list(columns) && "n_obs" %in% names(columns))
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
  expect_true("study_id" %in% names(result1$data.columns))
})
