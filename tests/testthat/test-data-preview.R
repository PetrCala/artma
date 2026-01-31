box::use(
  testthat[
    expect_error,
    expect_invisible,
    expect_null,
    test_that
  ],
  withr[local_options]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_error <- getFromNamespace("expect_error", "testthat")
expect_invisible <- getFromNamespace("expect_invisible", "testthat")

test_that("data.preview with data frame and preprocess FALSE returns invisibly", {
  withr::local_options(list(artma.verbose = 0))

  df <- data.frame(
    study_id = c("A", "B"),
    effect = c(0.5, 0.3),
    se = c(0.1, 0.15),
    n_obs = c(100, 200)
  )

  out <- artma::data.preview(df, preprocess = FALSE)

  expect_null(out)
  expect_invisible(artma::data.preview(df, preprocess = FALSE))
})

test_that("data.preview with file path and preprocess FALSE reads raw and returns invisibly", {
  withr::local_options(list(artma.verbose = 0))

  tmp_csv <- withr::local_tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      study_id = c("S1", "S2"),
      effect = c(1.0, 2.0),
      se = c(0.1, 0.2),
      n_obs = c(50, 75)
    ),
    tmp_csv,
    row.names = FALSE
  )

  out <- artma::data.preview(tmp_csv, preprocess = FALSE)

  expect_null(out)
  expect_invisible(artma::data.preview(tmp_csv, preprocess = FALSE))
})

test_that("data.preview rejects invalid data", {
  withr::local_options(list(artma.verbose = 0))

  expect_error(
    artma::data.preview(c("/path/a.csv", "/path/b.csv"), preprocess = FALSE),
    "single file path"
  )

  expect_error(
    artma::data.preview(list(a = 1), preprocess = FALSE),
    "NULL, a file path"
  )

  expect_error(
    artma::data.preview(1L, preprocess = FALSE),
    "NULL, a file path"
  )
})

test_that("data.preview with path and preprocess FALSE uses raw read (no options)", {
  withr::local_options(list(artma.verbose = 0))

  tmp_csv <- withr::local_tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      MyStudy = c("S1", "S2"),
      EffectSize = c(1.0, 2.0),
      StdErr = c(0.1, 0.2),
      N = c(50, 75)
    ),
    tmp_csv,
    row.names = FALSE
  )

  # Raw read must not standardize column names; View is side effect, we only check no error
  expect_null(artma::data.preview(tmp_csv, preprocess = FALSE))
})

test_that("read_data_raw returns data frame without standardizing column names", {
  box::use(artma / data / read[read_data_raw])

  withr::local_options(list(artma.verbose = 0))

  tmp_csv <- withr::local_tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      MyStudy = c("S1", "S2"),
      EffectSize = c(1.0, 2.0),
      StdErr = c(0.1, 0.2),
      N = c(50, 75)
    ),
    tmp_csv,
    row.names = FALSE
  )

  df <- read_data_raw(tmp_csv)

  expect_true(is.data.frame(df))
  expect_true("MyStudy" %in% names(df))
  expect_true("EffectSize" %in% names(df))
  expect_true("StdErr" %in% names(df))
  expect_true("N" %in% names(df))
  expect_equal(nrow(df), 2L)
})
