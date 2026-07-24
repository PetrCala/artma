box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_identical,
    expect_named,
    expect_null,
    expect_true,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / options / resolver[opt_spec, resolve_options],
  artma / methods / linear_tests[resolve_linear_tests_options]
)


# --- resolver unit behavior -------------------------------------------------

test_that("resolve_options applies template defaults for unset options", {
  resolved <- resolve_options(list(), list(
    a = opt_spec(default = 3L, type = "numeric"),
    b = opt_spec(default = "x", type = "character")
  ))
  expect_identical(resolved$a, 3L)
  expect_identical(resolved$b, "x")
})

test_that("resolve_options reads set values from the group", {
  resolved <- resolve_options(list(a = 7L), list(
    a = opt_spec(default = 3L, type = "numeric")
  ))
  expect_identical(resolved$a, 7L)
})

test_that("resolve_options casts values with the spec cast", {
  resolved <- resolve_options(list(n = 80.9), list(
    n = opt_spec(default = 80L, type = "numeric", cast = as.integer)
  ))
  expect_identical(resolved$n, 80L)
})

test_that("resolve_options enforces constraints with the spec message", {
  expect_error(
    resolve_options(list(x = -1), list(
      x = opt_spec(
        default = 1, type = "numeric",
        constraint = function(v) v >= 0,
        constraint_msg = "x must be non-negative."
      )
    )),
    "x must be non-negative.",
    class = "assertion_error"
  )
})

test_that("resolve_options rejects a value of the wrong type", {
  expect_error(
    resolve_options(list(flag = "nope"), list(
      flag = opt_spec(default = TRUE, type = "logical")
    )),
    class = "validation_error"
  )
})

test_that("resolve_options skips cast and constraint for allow_na NA values", {
  resolved <- resolve_options(list(), list(
    x = opt_spec(
      default = NA_real_, type = "numeric", allow_na = TRUE,
      cast = as.integer,
      constraint = function(v) v > 0,
      constraint_msg = "must be positive"
    )
  ))
  expect_true(is.na(resolved$x))
})

test_that("resolve_options reads global options through key", {
  local_options("artma.output.number_of_decimals" = 2L)
  resolved <- resolve_options(list(), list(
    round_to = opt_spec(
      default = 3L, type = "numeric",
      key = "artma.output.number_of_decimals", cast = as.integer
    )
  ))
  expect_identical(resolved$round_to, 2L)
})

test_that("resolve_options reads a renamed group key with from", {
  resolved <- resolve_options(list(`nested.key` = 5L), list(
    flat = opt_spec(default = 1L, type = "numeric", from = "nested.key")
  ))
  expect_identical(resolved$flat, 5L)
})


# --- linear_tests characterization ------------------------------------------
# Pins the resolved_options that linear_tests passes to run_linear_models. The
# values here reproduce the pre-refactor inline read/validate/assert/cast block
# exactly, so the resolver migration is a no-behavior-change move.

local_linear_options <- function(..., .env = parent.frame()) {
  local_options(
    list(
      "artma.methods.add_significance_marks" = TRUE,
      "artma.output.number_of_decimals" = 3L,
      ...
    ),
    .local_envir = .env
  )
}

test_that("resolve_linear_tests_options returns template defaults", {
  local_linear_options()
  resolved <- resolve_linear_tests_options()

  expect_named(
    resolved,
    c("add_significance_marks", "bootstrap_replications", "conf_level", "round_to")
  )
  expect_identical(resolved$add_significance_marks, TRUE)
  expect_identical(resolved$bootstrap_replications, 999L)
  expect_equal(resolved$conf_level, 0.95)
  expect_identical(resolved$round_to, 3L)
})

test_that("resolve_linear_tests_options honours non-default values", {
  local_linear_options(
    "artma.methods.add_significance_marks" = FALSE,
    "artma.methods.linear_tests.bootstrap_replications" = 100L,
    "artma.methods.linear_tests.conf_level" = 0.9,
    "artma.output.number_of_decimals" = 2L
  )
  resolved <- resolve_linear_tests_options()

  expect_identical(resolved$add_significance_marks, FALSE)
  expect_identical(resolved$bootstrap_replications, 100L)
  expect_equal(resolved$conf_level, 0.9)
  expect_identical(resolved$round_to, 2L)
})

test_that("resolve_linear_tests_options rejects an out-of-range conf_level", {
  local_linear_options("artma.methods.linear_tests.conf_level" = 1.5)
  expect_error(
    resolve_linear_tests_options(),
    "Confidence level must lie in the \\(0, 1\\) interval.",
    class = "assertion_error"
  )
})
