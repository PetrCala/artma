# The dotted API names are deprecated aliases for the snake_case exports
# (contributingGuides/API.md). One test per renamed group asserts the alias
# warns with the lifecycle class and forwards to the same result.

box::use(testthat[
  test_that, expect_identical, expect_warning, expect_null, expect_true
])

test_that("options.list is a deprecated alias of options_list", {
  options_dir <- withr::local_tempdir()

  expected <- options_list(options_dir = options_dir)

  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    result <- options.list(options_dir = options_dir),
    class = "lifecycle_warning_deprecated"
  )
  expect_identical(result, expected)
})

test_that("data.preview is a deprecated alias of data_preview", {
  df <- data.frame(effect = c(0.1, 0.2), se = c(0.01, 0.02))

  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    result <- suppressMessages(data.preview(df, preprocess = FALSE)),
    class = "lifecycle_warning_deprecated"
  )
  expect_null(result)
})

test_that("config.get is a deprecated alias of config_get", {
  # The deprecation warning fires before the body, so the alias wiring is
  # observable even though the call itself errors on a missing options file.
  # Muffle every warning to keep the parallel test runner clean, then assert
  # the lifecycle one was among them.
  withr::local_options(lifecycle_verbosity = "warning")

  caught <- list()
  withCallingHandlers(
    tryCatch(
      config.get(options_file_name = "does-not-exist.yaml"),
      error = function(e) NULL
    ),
    warning = function(w) {
      caught[[length(caught) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(vapply(caught, inherits, logical(1), "lifecycle_warning_deprecated")))
})
