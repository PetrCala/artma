# The dotted API names are deprecated aliases for the snake_case exports
# (contributingGuides/API.md). One test per renamed group asserts the alias
# warns with the lifecycle class and forwards to the same result.

box::use(testthat[
  test_that, expect_identical, expect_warning
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
