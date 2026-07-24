box::use(
  testthat[
    expect_equal,
    expect_null,
    expect_warning,
    test_that
  ],
  withr[local_options]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_null <- getFromNamespace("expect_null", "testthat")
expect_warning <- getFromNamespace("expect_warning", "testthat")

# -- resolve_se_zero_handling ------------------------------------------------------

test_that("resolve_se_zero_handling is a no-op when the option is already configured", {
  box::use(artma / data / configure[resolve_se_zero_handling])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = "stop",
    "artma.verbose" = 1
  ))

  df <- data.frame(se = c(0.1, 0))
  resolve_se_zero_handling(df)

  expect_equal(getOption("artma.calc.se_zero_handling"), "stop")
})

test_that("resolve_se_zero_handling is a no-op when no zero SE rows are found", {
  box::use(artma / data / configure[resolve_se_zero_handling])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = NULL,
    "artma.verbose" = 1
  ))

  df <- data.frame(se = c(0.1, 0.2))
  resolve_se_zero_handling(df)

  expect_null(getOption("artma.calc.se_zero_handling"))
})

test_that("resolve_se_zero_handling defaults to 'remove' in a non-interactive session", {
  box::use(artma / data / configure[resolve_se_zero_handling])

  withr::local_options(list(
    "artma.calc.se_zero_handling" = NULL,
    "artma.autonomy.level" = NULL,
    "artma.verbose" = 2
  ))

  df <- data.frame(se = c(0.1, 0, 0.2))
  expect_warning(resolve_se_zero_handling(df), "stricter validation")

  expect_equal(getOption("artma.calc.se_zero_handling"), "remove")
})
