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

  expect_false(resolve_add_significance_marks())
})

test_that("legacy per-method option names are ignored", {
  local_options(
    "artma.verbose" = 0,
    "artma.methods.add_significance_marks" = NULL,
    "artma.methods.linear_tests.add_significance_marks" = FALSE
  )

  # Only the canonical key drives the value; with it unset, the template default
  # (TRUE) applies rather than the legacy per-method key.
  expect_true(resolve_add_significance_marks())
})

test_that("canonical option overrides any legacy value", {
  local_options(
    "artma.verbose" = 0,
    "artma.methods.add_significance_marks" = FALSE,
    "artma.methods.linear_tests.add_significance_marks" = TRUE
  )

  expect_false(resolve_add_significance_marks())
})
