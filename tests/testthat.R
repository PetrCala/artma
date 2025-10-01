# nolint start: undesirable_function_linter.
library(testthat)
library(artma)
# nolint end: undesirable_function_linter.

testthat::test_check("artma", reporter = "summary")
