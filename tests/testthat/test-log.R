box::use(
  testthat[
    expect_equal, expect_false, expect_message, expect_no_message, expect_true, test_that
  ]
)

test_that("log_error emits only at verbosity >= 1", {
  box::use(artma / libs / core / log[log_error])

  withr::local_options(list(artma.verbose = 0))
  expect_no_message(log_error("boom"))

  for (level in 1:4) {
    withr::local_options(list(artma.verbose = level))
    expect_message(log_error("boom"), "boom")
  }
})

test_that("log_warn emits only at verbosity >= 2", {
  box::use(artma / libs / core / log[log_warn])

  for (level in c(0, 1)) {
    withr::local_options(list(artma.verbose = level))
    expect_no_message(log_warn("careful"))
  }

  for (level in 2:4) {
    withr::local_options(list(artma.verbose = level))
    expect_message(log_warn("careful"), "careful")
  }
})

test_that("log_info emits only at verbosity >= 3", {
  box::use(artma / libs / core / log[log_info])

  for (level in c(0, 1, 2)) {
    withr::local_options(list(artma.verbose = level))
    expect_no_message(log_info("fyi"))
  }

  for (level in 3:4) {
    withr::local_options(list(artma.verbose = level))
    expect_message(log_info("fyi"), "fyi")
  }
})

test_that("log_debug emits only at verbosity >= 4", {
  box::use(artma / libs / core / log[log_debug])

  for (level in c(0, 1, 2, 3)) {
    withr::local_options(list(artma.verbose = level))
    expect_no_message(log_debug("trace"))
  }

  withr::local_options(list(artma.verbose = 4))
  expect_message(log_debug("trace"), "trace")
})

test_that("all wrappers default to CONST$DEFAULT_VERBOSITY when the option is unset", {
  box::use(
    artma / const[CONST],
    artma / libs / core / log[log_error, log_warn, log_info, log_debug]
  )

  withr::local_options(list(artma.verbose = NULL))
  expect_equal(CONST$DEFAULT_VERBOSITY, 3)

  # Default verbosity (3) is info: error/warn/info emit, debug stays silent.
  expect_message(log_error("boom"), "boom")
  expect_message(log_warn("careful"), "careful")
  expect_message(log_info("fyi"), "fyi")
  expect_no_message(log_debug("trace"))
})

test_that("is_*_enabled predicates match the documented thresholds", {
  box::use(
    artma / libs / core / log[
      is_error_enabled, is_warn_enabled, is_info_enabled, is_debug_enabled
    ]
  )

  expectations <- list(
    "0" = c(FALSE, FALSE, FALSE, FALSE),
    "1" = c(TRUE, FALSE, FALSE, FALSE),
    "2" = c(TRUE, TRUE, FALSE, FALSE),
    "3" = c(TRUE, TRUE, TRUE, FALSE),
    "4" = c(TRUE, TRUE, TRUE, TRUE)
  )

  for (level_str in names(expectations)) {
    withr::local_options(list(artma.verbose = as.integer(level_str)))
    expected <- expectations[[level_str]]

    expect_equal(is_error_enabled(), expected[1], info = paste("level", level_str))
    expect_equal(is_warn_enabled(), expected[2], info = paste("level", level_str))
    expect_equal(is_info_enabled(), expected[3], info = paste("level", level_str))
    expect_equal(is_debug_enabled(), expected[4], info = paste("level", level_str))
  }
})

test_that("wrappers interpolate cli glue expressions from the caller's environment", {
  box::use(artma / libs / core / log[log_info])

  withr::local_options(list(artma.verbose = 3))

  n_rows <- 42
  label <- "widgets"
  expect_message(log_info("Read {n_rows} {label}"), "Read 42 widgets")
})

test_that("wrappers forward extra cli arguments (pluralization, inline classes)", {
  box::use(artma / libs / core / log[log_warn])

  withr::local_options(list(artma.verbose = 2))

  n <- 3
  expect_message(log_warn("Found {n} issue{?s}"), "Found 3 issues")
})
