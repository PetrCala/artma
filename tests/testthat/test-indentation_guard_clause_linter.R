box::use(
  testthat[expect_gt, expect_length, skip, skip_if_not_installed, test_that]
)

get_guard_clause_linter <- function() {
  skip_if_not_installed("lintr")

  linters <- load_repo_linters()
  if (is.null(linters)) {
    skip("scripts/linters.R is not available in this test environment")
  }

  linters$indentation_guard_clause_linter()
}

test_that("indentation_guard_clause_linter allows guard clause without braces", {
  linter <- get_guard_clause_linter()

  source_text <- "foo <- function(x) {\n  if (x < 0)\n    stop('negative input')\n\n  x\n}\n"

  lints <- lintr::lint(text = source_text, linters = linter)

  expect_length(lints, 0L)
})

test_that("indentation_guard_clause_linter keeps regular indentation issues", {
  linter <- get_guard_clause_linter()

  source_text <- "foo <- function() {\nif (TRUE)\n  1\n}\n"

  lints <- lintr::lint(text = source_text, linters = linter)

  expect_gt(length(lints), 0L)
})

test_that("indentation_guard_clause_linter requires indentation on guard body", {
  linter <- get_guard_clause_linter()

  source_text <- "if (ready)\nreturn(TRUE)\n"

  lints <- lintr::lint(text = source_text, linters = linter)

  expect_gt(length(lints), 0L)
})

test_that("indentation_guard_clause_linter flags over-indented guard body", {
  linter <- get_guard_clause_linter()

  source_text <- "if (ready)\n    return(TRUE)\n"

  lints <- lintr::lint(text = source_text, linters = linter)

  expect_gt(length(lints), 0L)
})
