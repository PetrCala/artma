box::use(
  testthat[test_that, expect_length, expect_gt],
  lintr[lint]
)

indentation_guard_clause_linter <- getFromNamespace("indentation_guard_clause_linter", "artma")

test_that("indentation_guard_clause_linter allows guard clause without braces", {
  linter <- indentation_guard_clause_linter()

  source_text <- "foo <- function(x) {\n  if (x < 0)\n    stop('negative input')\n\n  x\n}\n"

  lints <- lint(text = source_text, linters = linter)

  expect_length(lints, 0L)
})

test_that("indentation_guard_clause_linter keeps regular indentation issues", {
  linter <- indentation_guard_clause_linter()

  source_text <- "foo <- function() {\nif (TRUE)\n  1\n}\n"

  lints <- lint(text = source_text, linters = linter)

  expect_gt(length(lints), 0L)
})

test_that("indentation_guard_clause_linter requires indentation on guard body", {
  linter <- indentation_guard_clause_linter()

  source_text <- "if (ready)\nreturn(TRUE)\n"

  lints <- lint(text = source_text, linters = linter)

  expect_gt(length(lints), 0L)
})

test_that("indentation_guard_clause_linter flags over-indented guard body", {
  linter <- indentation_guard_clause_linter()

  source_text <- "if (ready)\n    return(TRUE)\n"

  lints <- lint(text = source_text, linters = linter)

  expect_gt(length(lints), 0L)
})
