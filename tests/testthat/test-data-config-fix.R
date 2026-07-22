box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(
  artma / data_config / write[fix_data_config],
  artma / data / mock[create_mock_df],
  artma / options / files[options_file_path, read_options_file, write_options_file]
)

# fix_data_config clears analysis overrides but must preserve every source_name
# mapping, including those on moderator records, which cannot be re-derived
# from the dataframe.

test_that("fix_data_config keeps moderator source_names, drops analysis overrides", {
  tmp_dir <- withr::local_tempdir()
  file_name <- "cfg.yaml"
  data_path <- file.path(tmp_dir, "data.csv")
  utils::write.csv(create_mock_df(n_studies = 3, seed = 1), data_path, row.names = FALSE)

  write_options_file(options_file_path(tmp_dir, file_name), list(general = list(name = "x")))

  store <- list(
    effect = list(source_name = "effect_size", bma = FALSE),
    gdp_growth = list(source_name = "GDP", bma = TRUE),
    inflation = list(bma = TRUE) # override only, no rename
  )

  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.source_path" = data_path,
    "artma.data.columns" = store,
    "artma.temp.file_name" = file_name,
    "artma.temp.dir_name" = tmp_dir
  ))

  fix_data_config()

  result <- getOption("artma.data.columns")

  # Renames survive, on both role and moderator records
  expect_equal(result$effect$source_name, "effect_size")
  expect_equal(result$gdp_growth$source_name, "GDP")

  # Analysis overrides are cleared everywhere
  expect_null(result$effect$bma)
  expect_null(result$gdp_growth$bma)

  # A record carrying only an override, no rename, is dropped entirely
  expect_null(result$inflation)

  # The change is persisted to the options file
  written <- read_options_file(options_file_path(tmp_dir, file_name))$data$columns
  expect_equal(written$gdp_growth$source_name, "GDP")
  expect_null(written$gdp_growth$bma)
})
