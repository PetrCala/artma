box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_true,
    test_that
  ]
)

box::use(
  artma / data / mock[create_mock_df, materialize_mock_data_path, mock_data_file_path]
)

# The whole point of the mock data path is that it outlives the R session, so
# the scratch user data directory used here sits *outside* `tempdir()` - a
# temporary redirect would make the durability assertion vacuous.
local_user_data_dir <- function(.local_envir = parent.frame()) {
  dir <- file.path(getwd(), sprintf("mock-user-data-%s", Sys.getpid()))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(dir, recursive = TRUE), envir = .local_envir)
  withr::local_envvar(R_USER_DATA_DIR = dir, .local_envir = .local_envir)
  normalizePath(dir, winslash = "/")
}

test_that("materialize_mock_data_path writes a durable file outside tempdir()", {
  base <- local_user_data_dir()
  withr::local_options(artma.verbose = 1)

  path <- materialize_mock_data_path()

  expect_true(file.exists(path))
  expect_equal(path, mock_data_file_path())
  expect_true(startsWith(normalizePath(path, winslash = "/"), base))
  expect_false(startsWith(
    normalizePath(path, winslash = "/"),
    normalizePath(tempdir(), winslash = "/")
  ))
  expect_true(nrow(utils::read.csv(path)) > 0)
})

test_that("materialize_mock_data_path reuses an existing mock file", {
  local_user_data_dir()
  withr::local_options(artma.verbose = 1)

  path <- materialize_mock_data_path()
  Sys.setFileTime(path, Sys.time() - 3600)
  mtime_before <- file.mtime(path)

  expect_equal(materialize_mock_data_path(), path)
  expect_equal(file.mtime(path), mtime_before)

  expect_equal(materialize_mock_data_path(regenerate = TRUE), path)
  expect_true(file.mtime(path) > mtime_before)
})

test_that("create_mock_df still writes to an explicit throwaway path", {
  file_path <- withr::local_tempfile(fileext = ".csv")

  df <- create_mock_df(nrow = 20, n_studies = 3, with_file_creation = TRUE, file_path = file_path)

  expect_true(file.exists(file_path))
  expect_equal(nrow(utils::read.csv(file_path)), nrow(df))
})
