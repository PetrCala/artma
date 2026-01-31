box::use(
  artma / data / interactive_mapping[
    confirm_column_mapping,
    save_column_mapping_to_options,
    format_mapping_display
  ],
  artma / data / column_recognition[get_required_column_names]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_no_error <- getFromNamespace("expect_no_error", "testthat")


# Note: interactive_column_mapping and column_mapping_workflow require
# user interaction via climenu, so they are tested in E2E tests instead.
# However, we test the non-interactive helper functions here.


test_that("confirm_column_mapping returns mapping unchanged", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect",
    se = "se",
    n_obs = "n_obs"
  )

  required_cols <- c("study_id", "effect", "se", "n_obs")

  withr::local_options(list("artma.verbose" = 1))
  result <- confirm_column_mapping(mapping, required_cols)

  expect_equal(result, mapping)
})


test_that("confirm_column_mapping handles empty mapping", {
  mapping <- list()
  required_cols <- c("study_id", "effect", "se", "n_obs")

  withr::local_options(list("artma.verbose" = 1))
  result <- confirm_column_mapping(mapping, required_cols)

  expect_equal(result, mapping)
})


test_that("confirm_column_mapping handles partial mapping", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect"
  )

  required_cols <- c("study_id", "effect", "se", "n_obs")

  withr::local_options(list("artma.verbose" = 1))
  result <- confirm_column_mapping(mapping, required_cols)

  expect_equal(result, mapping)
})


test_that("save_column_mapping_to_options sets session options correctly", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect_size",
    se = "std_error"
  )

  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.colnames.study_id" = NULL,
    "artma.data.colnames.effect" = NULL,
    "artma.data.colnames.se" = NULL
  ))

  # Save without options file (just to session)
  save_column_mapping_to_options(mapping, options_file_name = NULL)

  # Check that options were set
  expect_equal(getOption("artma.data.colnames.study_id"), "study_name")
  expect_equal(getOption("artma.data.colnames.effect"), "effect_size")
  expect_equal(getOption("artma.data.colnames.se"), "std_error")
})


test_that("save_column_mapping_to_options handles empty mapping", {
  mapping <- list()

  withr::local_options(list("artma.verbose" = 1))

  expect_no_error(
    save_column_mapping_to_options(mapping, options_file_name = NULL)
  )
})


test_that("save_column_mapping_to_options handles multiple columns", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect",
    se = "se",
    n_obs = "n_obs",
    t_stat = "t_statistic",
    obs_id = "sid"
  )

  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.colnames.study_id" = NULL,
    "artma.data.colnames.effect" = NULL,
    "artma.data.colnames.se" = NULL,
    "artma.data.colnames.n_obs" = NULL,
    "artma.data.colnames.t_stat" = NULL,
    "artma.data.colnames.obs_id" = NULL
  ))

  save_column_mapping_to_options(mapping, options_file_name = NULL)

  # Verify all were set
  expect_equal(getOption("artma.data.colnames.study_id"), "study_name")
  expect_equal(getOption("artma.data.colnames.effect"), "effect")
  expect_equal(getOption("artma.data.colnames.se"), "se")
  expect_equal(getOption("artma.data.colnames.n_obs"), "n_obs")
  expect_equal(getOption("artma.data.colnames.t_stat"), "t_statistic")
  expect_equal(getOption("artma.data.colnames.obs_id"), "sid")
})


test_that("confirm_column_mapping works with verbose output", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect"
  )

  required_cols <- c("study_id", "effect", "se", "n_obs")

  withr::local_options(list("artma.verbose" = 4))

  # Should not error even with verbose output
  expect_no_error(
    confirm_column_mapping(mapping, required_cols)
  )
})


test_that("save_column_mapping_to_options works with verbose output", {
  mapping <- list(
    study_id = "study_name"
  )

  withr::local_options(list(
    "artma.verbose" = 4,
    "artma.data.colnames.study_id" = NULL
  ))

  expect_no_error(
    save_column_mapping_to_options(mapping, options_file_name = NULL)
  )
})


test_that("format_mapping_display correctly separates required and optional", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect_size",
    se = "se",
    n_obs = "n_obs",
    t_stat = "t_statistic"
  )

  required_cols <- get_required_column_names()

  result <- format_mapping_display(mapping, required_cols)

  expect_true(is.list(result))
  expect_true("required" %in% names(result))
  expect_true("optional" %in% names(result))

  # All required columns should be in required list
  expect_true(all(required_cols %in% names(result$required)))

  # Optional columns should be in optional list
  expect_true("t_stat" %in% names(result$optional))
  expect_false("t_stat" %in% names(result$required))
})


test_that("format_mapping_display handles empty optional columns", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect_size",
    se = "se",
    n_obs = "n_obs"
  )

  required_cols <- get_required_column_names()

  result <- format_mapping_display(mapping, required_cols)

  expect_equal(length(result$optional), 0)
  expect_equal(length(result$required), 4)
})
