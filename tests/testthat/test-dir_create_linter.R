box::use(
  testthat[skip, skip_if_not_installed, test_that]
)

get_dir_create_linter <- function() {
  skip_if_not_installed("lintr")

  linters <- load_repo_linters()
  if (is.null(linters)) {
    skip("scripts/linters.R is not available in this test environment")
  }

  linters$dir_create_linter()
}

test_that("dir_create_linter skips allowed usages", {
  linter <- get_dir_create_linter()

  lintr::expect_lint("", NULL, linter)
  lintr::expect_lint("fs::dir_create", NULL, linter)
})

test_that("dir_create_linter disallows usage of dir.create()", {
  skip_if_not_installed("rex")

  linter <- get_dir_create_linter()
  lint_msg <- rex::rex("Usage of dir.create() is not allowed.")

  lintr::expect_lint("dir.create()", lint_msg, linter)
  lintr::expect_lint("dir.create(file.path('some', 'path.R'))", lint_msg, linter)
})
