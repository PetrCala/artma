box::use(
  testthat[expect_identical, expect_true, test_that],
  withr[local_options]
)

box::use(artma / calc / meta[normal_p_value, t_stat])

test_that("normal_p_value computes the two-sided normal p-value", {
  expect_identical(normal_p_value(1.96, 1), 2 * stats::pnorm(1.96, lower.tail = FALSE))
})

test_that("normal_p_value returns NA for non-finite or non-positive inputs", {
  expect_true(is.na(normal_p_value(NA_real_, 1)))
  expect_true(is.na(normal_p_value(1, NA_real_)))
  expect_true(is.na(normal_p_value(Inf, 1)))
  expect_true(is.na(normal_p_value(1, 0)))
  expect_true(is.na(normal_p_value(1, -1)))
})

test_that("t_stat keeps the effect sign for zero standard errors", {
  local_options("artma.verbose" = 1)

  result <- t_stat(effect = c(0.5, -0.5, 0, 0.2), se = c(0, 0, 0, 0.1))

  expect_identical(result[1], Inf)
  expect_identical(result[2], -Inf)
  expect_true(is.na(result[3]))
  expect_identical(result[4], 2)
})
