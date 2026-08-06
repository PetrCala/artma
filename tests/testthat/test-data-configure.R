box::use(
  testthat[
    expect_equal,
    expect_no_error,
    expect_null,
    expect_warning,
    test_that
  ],
  withr[local_options]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_no_error <- getFromNamespace("expect_no_error", "testthat")
expect_null <- getFromNamespace("expect_null", "testthat")
expect_warning <- getFromNamespace("expect_warning", "testthat")

# -- resolve_na_handling ------------------------------------------------------

test_that("resolve_na_handling defaults to 'stop' in a non-interactive session without aborting", {
  # issue #401: the non-interactive default must not be able to halt the run
  # by itself; handle_missing_values() is what decides whether missing
  # optional values are actually fatal (they are not).
  box::use(
    artma / data / configure[resolve_na_handling],
    artma / data / na_handling[handle_missing_values]
  )

  withr::local_options(list(
    "artma.data.na_handling" = NULL,
    "artma.verbose" = 1
  ))

  df <- data.frame(
    study_id = c("a", "b", "c"),
    effect = c(0.1, 0.2, 0.3),
    se = c(0.01, 0.02, 0.03),
    n_obs = c(10L, 20L, 30L),
    moderator = c(1, NA, 3),
    stringsAsFactors = FALSE
  )

  resolve_na_handling(df)
  expect_equal(getOption("artma.data.na_handling"), "stop")

  expect_no_error(result <- handle_missing_values(df))
  expect_equal(nrow(result), 3)
})

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
