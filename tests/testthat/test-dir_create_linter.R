box::use(
  testthat[test_that],
  lintr[expect_lint]
)

dir_create_linter <- getFromNamespace("dir_create_linter", "artma")

test_that("dir_create_linter skips allowed usages", {
  linter <- dir_create_linter()

  expect_lint("", NULL, linter)
  expect_lint("fs::dir_create", NULL, linter)
})

test_that("dir_create_linter disallows usage of dir.create()", {
  linter <- dir_create_linter()
  lint_msg <- rex::rex("Usage of dir.create() is not allowed.")

  expect_lint("dir.create()", lint_msg, linter)
  expect_lint("dir.create(file.path('some', 'path.R'))", lint_msg, linter)
})
