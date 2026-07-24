box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_no_error,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(
  artma / data / interactive_mapping[
    confirm_column_mapping,
    format_mapping_display
  ],
  artma / data_config / column_mapping[save_column_mapping_to_options],
  artma / data / column_recognition[get_required_column_names]
)


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
    "artma.data.columns" = list()
  ))

  # Save without options file (just to session)
  save_column_mapping_to_options(mapping, options_file_name = NULL)

  # Check that role records were written to the unified store
  store <- getOption("artma.data.columns")
  expect_equal(store$study_id$source_name, "study_name")
  expect_equal(store$effect$source_name, "effect_size")
  expect_equal(store$se$source_name, "std_error")
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
    "artma.data.columns" = list()
  ))

  save_column_mapping_to_options(mapping, options_file_name = NULL)

  # Genuine renames are stored; identity mappings (effect, se, n_obs) are not,
  # since a column already carrying the standard name needs no record.
  store <- getOption("artma.data.columns")
  expect_equal(store$study_id$source_name, "study_name")
  expect_equal(store$t_stat$source_name, "t_statistic")
  expect_equal(store$obs_id$source_name, "sid")
  expect_null(store$effect)
  expect_null(store$se)
  expect_null(store$n_obs)
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
    "artma.data.columns" = list()
  ))

  expect_no_error(
    save_column_mapping_to_options(mapping, options_file_name = NULL)
  )
})


test_that("save_column_mapping_to_options preserves existing analysis fields", {
  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.columns" = list(
      effect = list(bma = TRUE)
    )
  ))

  save_column_mapping_to_options(
    list(effect = "effect_size"),
    options_file_name = NULL
  )

  store <- getOption("artma.data.columns")
  expect_equal(store$effect$source_name, "effect_size")
  expect_true(store$effect$bma)
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


test_that("read_stored_columns seeds from the options file, not the session", {
  box::use(
    artma / data_config / column_mapping[read_stored_columns],
    artma / options / files[write_options_file]
  )

  tmp_dir <- withr::local_tempdir()
  file_name <- "seed-test.yaml"

  write_options_file(
    file.path(tmp_dir, file_name),
    list(data = list(columns = list(gdp_growth = list(bma = TRUE))))
  )

  # The session store is empty, as it is during options file creation.
  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.columns" = list()
  ))

  store <- read_stored_columns(file_name, options_dir = tmp_dir)

  expect_true(store$gdp_growth$bma)
})


test_that("save_column_mapping_to_options keeps records already in the file", {
  box::use(
    artma / data_config / column_mapping[save_column_mapping_to_options],
    artma / options / files[options_file_path, read_options_file, write_options_file]
  )

  tmp_dir <- withr::local_tempdir()
  file_name <- "keep-records.yaml"
  path <- options_file_path(tmp_dir, file_name)

  # Build a file valid against the real template, then seed it with records
  # a user would have configured before the mapping flow runs.
  artma::options.create(
    options_file_name = file_name,
    options_dir = tmp_dir,
    user_input = list("data.source_path" = "some-data.csv"),
    should_validate = FALSE,
    should_overwrite = TRUE
  )

  seeded <- read_options_file(path)
  seeded$data$columns <- list(
    gdp_growth = list(bma = TRUE),
    effect = list(bma = FALSE)
  )
  write_options_file(path, seeded)

  # Empty session store, as during options file creation.
  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.columns" = list()
  ))

  save_column_mapping_to_options(
    list(effect = "effect_size"),
    options_file_name = file_name,
    options_dir = tmp_dir
  )

  written <- read_options_file(path)$data$columns

  # The moderator record survives the write
  expect_true(written$gdp_growth$bma)

  # The mapped role record gains source_name without losing its other fields
  expect_equal(written$effect$source_name, "effect_size")
  expect_false(written$effect$bma)
})


test_that("read_stored_columns falls back to the session store when absent", {
  box::use(artma / data_config / column_mapping[read_stored_columns])

  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.columns" = list(effect = list(source_name = "b"))
  ))

  store <- read_stored_columns("no-such-file-here.yaml")

  expect_equal(store$effect$source_name, "b")
})
