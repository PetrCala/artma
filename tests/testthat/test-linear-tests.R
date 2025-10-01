box::use(
  testthat[
    expect_equal,
    expect_named,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_options]
)

default_linear_df <- function() {
  data.frame(
    effect = c(0.2, 0.25, 0.35, 0.4, 0.5, 0.55),
    se = c(0.12, 0.1, 0.11, 0.09, 0.08, 0.07),
    study = rep(c("A", "B", "C"), each = 2),
    study_id = rep(1:3, each = 2),
    study_size = rep(2, 6),
    precision = 1 / c(0.12, 0.1, 0.11, 0.09, 0.08, 0.07)
  )
}

linear_methods <- function() {
  c(
    "OLS",
    "Fixed Effects",
    "Between Effects",
    "Random Effects",
    "Study Weighted OLS",
    "Precision Weighted OLS"
  )
}

expected_columns <- function() {
  c(
    "method_id",
    "method",
    "term",
    "term_label",
    "estimate",
    "std_error",
    "t_statistic",
    "p_value",
    "significance_mark",
    "formatted_estimate",
    "formatted_std_error",
    "ci_lower",
    "ci_upper",
    "formatted_ci",
    "n_obs",
    "conf_level",
    "bootstrap_replications",
    "used_bootstrap",
    "round_to"
  )
}

test_that("linear tests produce structured output", {
  box::use(artma / methods / linear_tests[run])

  local_options(list(
    artma.verbose = 0,
    "artma.methods.linear_tests.add_significance_marks" = TRUE,
    "artma.methods.linear_tests.bootstrap_replications" = 0,
    "artma.methods.linear_tests.conf_level" = 0.9,
    "artma.methods.linear_tests.verbose" = FALSE
  ))

  df <- default_linear_df()
  result <- run(df)

  expect_named(result, expected_columns())
  expect_setequal(unique(result$method), linear_methods())
  expect_true(all(result$term %in% c("effect", "publication_bias")))
  expect_equal(unique(result$n_obs), nrow(df))
  expect_equal(unique(result$conf_level), 0.9)
  expect_equal(unique(result$bootstrap_replications), 0)
})

test_that("significance marks can be disabled", {
  box::use(artma / methods / linear_tests[run])

  local_options(list(
    artma.verbose = 0,
    "artma.methods.linear_tests.add_significance_marks" = FALSE,
    "artma.methods.linear_tests.bootstrap_replications" = 0,
    "artma.methods.linear_tests.conf_level" = 0.95,
    "artma.methods.linear_tests.verbose" = FALSE
  ))

  result <- run(default_linear_df())

  expect_true(all(is.na(result$significance_mark)))
  expect_true(all(!grepl("\\*", result$formatted_estimate, fixed = FALSE, perl = FALSE)))
})

test_that("bootstrap intervals are computed when enabled", {
  box::use(artma / methods / linear_tests[run])

  local_options(list(
    artma.verbose = 0,
    "artma.methods.linear_tests.add_significance_marks" = TRUE,
    "artma.methods.linear_tests.bootstrap_replications" = 50,
    "artma.methods.linear_tests.conf_level" = 0.9,
    "artma.methods.linear_tests.verbose" = FALSE
  ))

  result <- run(default_linear_df())

  bootstrap_methods <- setdiff(linear_methods(), "Between Effects")
  expect_true(all(result$used_bootstrap[result$method %in% bootstrap_methods]))
  expect_true(all(!result$used_bootstrap[result$method == "Between Effects"]))

  ols_rows <- result[result$method == "OLS", ]
  expect_true(any(is.finite(ols_rows$ci_lower)))
  expect_true(any(is.finite(ols_rows$ci_upper)))
})
