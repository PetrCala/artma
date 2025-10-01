box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_gt,
    expect_named,
    expect_true,
    test_that
  ],
  withr[local_options]
)

box::use(artma / methods / linear_tests[linear_tests])

make_demo_data <- function() {
  set.seed(42)
  n_studies <- 6L
  per_study <- 5L
  study_ids <- rep(paste0("S", seq_len(n_studies)), each = per_study)
  se_vals <- runif(n_studies * per_study, min = 0.05, max = 0.15)
  data.frame(
    study_id = study_ids,
    effect = rnorm(n_studies * per_study, mean = 0.2, sd = 0.05),
    se = se_vals,
    study_size = sample(20:80, n_studies * per_study, replace = TRUE),
    precision = 1 / se_vals,
    check.names = FALSE
  )
}

test_that("linear tests return tidy coefficients and summary", {
  df <- make_demo_data()

  local_options(
    "artma.methods.add_significance_marks" = TRUE,
    "artma.methods.linear_tests.bootstrap_replications" = 10L,
    "artma.methods.linear_tests.conf_level" = 0.9,
    "artma.output.number_of_decimals" = 2,
    "artma.verbose" = 1
  )

  res <- linear_tests(df)

  expect_named(
    res,
    c("coefficients", "summary", "skipped", "options"),
    ignore.order = TRUE
  )

  expect_equal(
    sort(unique(res$coefficients$model)),
    sort(c(
      "ols", "fe", "be", "re", "ols_study_weighted", "ols_precision_weighted"
    ))
  )

  expect_equal(nrow(res$coefficients), 12L)
  expect_named(
    res$coefficients,
    c(
      "estimate", "std_error", "statistic", "p_value", "term", "model",
      "model_label", "n_obs", "term_label", "bootstrap_lower",
      "bootstrap_upper", "significance", "estimate_rounded",
      "std_error_rounded", "estimate_formatted", "std_error_formatted",
      "bootstrap_formatted"
    )
  )

  expect_gt(nrow(res$summary), 0)
  expect_equal(
    rownames(res$summary),
    c(
      "Publication Bias", "(Std. Error)", "Bootstrap CI (PB)",
      "Effect Beyond Bias", "(Std. Error)", "Bootstrap CI (Effect)",
      "Total Observations"
    )
  )
  expect_true(all(res$coefficients$significance %in% c("", "*", "**", "***")))
  expect_equal(res$options$bootstrap_replications, 10L)
})

test_that("linear tests gracefully skip models with missing columns", {
  df <- make_demo_data()
  df$precision <- NULL

  local_options(
    "artma.methods.add_significance_marks" = FALSE,
    "artma.methods.linear_tests.bootstrap_replications" = 0L,
    "artma.methods.linear_tests.conf_level" = 0.95,
    "artma.output.number_of_decimals" = 3,
    "artma.verbose" = 1
  )

  res <- linear_tests(df)

  expect_false("ols_precision_weighted" %in% res$coefficients$model)
  expect_true("ols_precision_weighted" %in% names(res$skipped))
  expect_true(grepl("Missing required columns", res$skipped$ols_precision_weighted$reason))
})
