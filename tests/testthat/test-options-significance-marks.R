box::use(
  testthat[
    expect_false,
    expect_true,
    test_that
  ],
  withr[local_options]
)

box::use(artma / options / significance_marks[resolve_add_significance_marks])

test_that("shared significance option controls output", {
  local_options(
    "artma.verbose" = 0,
    "artma.methods.add_significance_marks" = FALSE
  )

  expect_false(resolve_add_significance_marks("artma.methods.linear_tests"))
})

test_that("legacy option names are still honoured when shared is unset", {
  local_options(
    "artma.verbose" = 0,
    "artma.methods.add_significance_marks" = NULL,
    "artma.methods.linear_tests.add_significance_marks" = TRUE
  )

  expect_true(resolve_add_significance_marks("artma.methods.linear_tests"))
})

test_that("shared option overrides conflicting legacy values", {
  local_options(
    "artma.verbose" = 0,
    "artma.methods.add_significance_marks" = FALSE,
    "artma.methods.linear_tests.add_significance_marks" = TRUE
  )

  expect_false(resolve_add_significance_marks("artma.methods.linear_tests"))
})
