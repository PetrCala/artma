box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_gt,
    expect_named,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / econometric / linear[run_linear_models],
  artma / methods / linear_tests[linear_tests]
)

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
  skip_if_not_installed("plm")

  df <- make_demo_data()

  local_options(
    "artma.methods.add_significance_marks" = TRUE,
    "artma.methods.linear_tests.bootstrap_replications" = 10L,
    "artma.methods.linear_tests.conf_level" = 0.9,
    "artma.output.number_of_decimals" = 2,
    "artma.verbose" = 1
  )

  res <- linear_tests(df)

  expect_named(res, c("tables", "plots", "meta"))
  expect_named(res$tables, "summary")
  expect_named(
    res$meta,
    c("coefficients", "skipped", "options"),
    ignore.order = TRUE
  )

  expect_equal(
    sort(unique(res$meta$coefficients$model)),
    sort(c(
      "ols", "fe", "be", "re", "ols_study_weighted", "ols_precision_weighted"
    ))
  )

  expect_equal(nrow(res$meta$coefficients), 12L)
  expect_named(
    res$meta$coefficients,
    c(
      "estimate", "std_error", "statistic", "p_value", "term", "model",
      "model_label", "n_obs", "term_label", "bootstrap_lower",
      "bootstrap_upper", "significance", "estimate_rounded",
      "std_error_rounded", "estimate_formatted", "std_error_formatted",
      "bootstrap_formatted"
    )
  )

  expect_gt(nrow(res$tables$summary), 0)
  expect_equal(
    rownames(res$tables$summary),
    c(
      "Publication Bias", "(Std. Error)", "Bootstrap CI (PB)",
      "Effect Beyond Bias", "(Std. Error)", "Bootstrap CI (Effect)",
      "Total Observations"
    )
  )
  expect_true(all(res$meta$coefficients$significance %in% c("", "*", "**", "***")))
  expect_equal(res$meta$options$bootstrap_replications, 10L)
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

  expect_false("ols_precision_weighted" %in% res$meta$coefficients$model)
  expect_true("ols_precision_weighted" %in% names(res$meta$skipped))
  expect_true(grepl("Missing required columns", res$meta$skipped$ols_precision_weighted$reason))
})

test_that("bootstrap CIs are deterministic and seed-identical to the tidy-path implementation", {
  skip_if_not_installed("plm")

  df <- make_demo_data()
  opts <- list(
    add_significance_marks = FALSE,
    bootstrap_replications = 50L,
    conf_level = 0.9,
    round_to = 3L
  )

  set.seed(123)
  res <- run_linear_models(df, options = opts)

  set.seed(123)
  res_repeat <- run_linear_models(df, options = opts)

  ci_cols <- c("model", "term", "bootstrap_lower", "bootstrap_upper")
  ci <- res$coefficients[, ci_cols]
  rownames(ci) <- NULL
  expect_equal(ci, res_repeat$coefficients[, ci_cols], ignore_attr = TRUE)

  finite_rows <- is.finite(ci$bootstrap_lower) & is.finite(ci$bootstrap_upper)
  expect_true(all(ci$bootstrap_lower[finite_rows] <= ci$bootstrap_upper[finite_rows]))

  # Reference values computed with the pre-optimization implementation, which
  # ran the full clustered-vcov tidy path in every bootstrap replication. The
  # fast boot_coefs path must stay seed-identical to it.
  expected <- data.frame(
    model = rep(
      c("ols", "fe", "be", "re", "ols_study_weighted", "ols_precision_weighted"),
      each = 2L
    ),
    term = rep(c("effect", "publication_bias"), times = 6L),
    bootstrap_lower = c(
      0.160739545997458, -0.601087904579698,
      0.140819890400399, -0.600311615629511,
      NA, NA,
      0.155088688976474, -0.649012667394041,
      0.162905232119029, -0.812806189499674,
      0.120950452943169, -0.593431488826163
    ),
    bootstrap_upper = c(
      0.237035405467370, 0.165658257380718,
      0.243966037478644, 0.440057444128715,
      NA, NA,
      0.251343193299353, 0.228828857125176,
      0.259964458196330, 0.177322819406577,
      0.249764223666147, 0.467372767218415
    ),
    stringsAsFactors = FALSE
  )

  expect_equal(ci, expected, tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("panel models are skipped with a clear message when plm is unavailable", {
  df <- make_demo_data()

  res <- run_linear_models(
    df,
    options = list(
      add_significance_marks = FALSE,
      bootstrap_replications = 0L,
      conf_level = 0.95,
      round_to = 3L
    ),
    is_pkg_available = function(pkg) pkg != "plm"
  )

  panel_models <- c("fe", "be", "re")

  expect_false(any(panel_models %in% res$coefficients$model))
  expect_true(all(panel_models %in% names(res$skipped)))

  reasons <- vapply(res$skipped[panel_models], function(item) item$reason, character(1))
  expect_true(all(grepl("plm", reasons, fixed = TRUE)))
  expect_true(all(grepl("install.packages", reasons, fixed = TRUE)))

  expect_true("ols" %in% res$coefficients$model)
})
