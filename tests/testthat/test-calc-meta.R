box::use(
  testthat[expect_identical, expect_true, test_that]
)

box::use(artma / calc / meta[normal_p_value])

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
