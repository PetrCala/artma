box::use(
  testthat[
    expect_equal,
    expect_identical,
    test_that
  ]
)

box::use(artma / libs / result_formatters[significance_mark])

test_that("significance_mark handles vector inputs", {
  p_values <- c(0.001, 0.02, 0.08, 0.2, NA, Inf)
  expect_equal(
    significance_mark(p_values),
    c("***", "**", "*", "", "", "")
  )
})

test_that("significance_mark preserves input names", {
  named_values <- c(alpha = 0.01, beta = 0.1, gamma = 1)
  expect_identical(
    significance_mark(named_values),
    c(alpha = "***", beta = "*", gamma = "")
  )
})

test_that("significance_mark returns empty vector for empty input", {
  expect_identical(significance_mark(numeric(0)), character(0))
})
