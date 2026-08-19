box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_invisible,
    expect_match,
    expect_null,
    expect_true,
    skip_if,
    test_that
  ],
  withr[local_options]
)

test_that("data_preview with data frame and preprocess FALSE returns invisibly", {
  withr::local_options(list(artma.verbose = 0))

  df <- data.frame(
    study_id = c("A", "B"),
    effect = c(0.5, 0.3),
    se = c(0.1, 0.15),
    n_obs = c(100, 200)
  )

  out <- artma::data_preview(df, preprocess = FALSE)

  expect_null(out)
  expect_invisible(artma::data_preview(df, preprocess = FALSE))
})

test_that("data_preview with file path and preprocess FALSE reads raw and returns invisibly", {
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

  out <- artma::data_preview(tmp_csv, preprocess = FALSE)

  expect_null(out)
  expect_invisible(artma::data_preview(tmp_csv, preprocess = FALSE))
})

test_that("data_preview rejects invalid data", {
  withr::local_options(list(artma.verbose = 0))

  expect_error(
    artma::data_preview(c("/path/a.csv", "/path/b.csv"), preprocess = FALSE),
    "single file path"
  )

  expect_error(
    artma::data_preview(list(a = 1), preprocess = FALSE),
    "NULL, a file path"
  )

  expect_error(
    artma::data_preview(1L, preprocess = FALSE),
    "NULL, a file path"
  )
})

test_that("the data viewer is never opened in a non-interactive session", {
  box::use(artma / libs / core / utils[data_viewer_available])

  skip_if(interactive(), "Only meaningful in a non-interactive session")

  # utils::View() aborts the R session on Unix builds whose only viewer is the
  # X11 data entry window, so data_preview() must not reach it here.
  expect_false(data_viewer_available())
})

test_that("the data viewer is never opened while R CMD check is running", {
  box::use(artma / libs / core / utils[data_viewer_available])

  # `utils::View()` crashed the CRAN checks for 0.3.3 (a null dereference in
  # the X11 data entry window), which takes the whole check down instead of
  # failing a single expectation.
  withr::local_envvar(c("_R_CHECK_PACKAGE_NAME_" = "artma"))

  expect_false(data_viewer_available())
})

test_that("data_preview prints a preview when no viewer is available", {
  box::use(artma / libs / core / utils[data_viewer_available])

  skip_if(data_viewer_available(), "A data viewer is available in this session")

  withr::local_options(list(artma.verbose = 3))

  df <- data.frame(study_id = c("A", "B"), effect = c(0.5, 0.3))

  emitted <- paste(
    testthat::capture_messages(artma::data_preview(df, preprocess = FALSE)),
    collapse = ""
  )

  expect_match(emitted, "No data viewer")
  expect_match(emitted, "study_id")
})

test_that("read_data returns data frame without standardizing column names", {
  box::use(artma / data / read[read_data])

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

  df <- read_data(tmp_csv)

  expect_true(is.data.frame(df))
  expect_true("MyStudy" %in% names(df))
  expect_true("EffectSize" %in% names(df))
  expect_true("StdErr" %in% names(df))
  expect_true("N" %in% names(df))
  expect_equal(nrow(df), 2L)
})
