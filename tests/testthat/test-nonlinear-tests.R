box::use(
  testthat[
    expect_equal,
    expect_gt,
    expect_named,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / methods / nonlinear_tests[nonlinear_tests],
  artma / libs / nonlinear_tests[run_nonlinear_methods]
)

make_demo_data <- function() {
  n_studies <- 8L
  per_study <- 6L
  study_ids <- rep(paste0("S", seq_len(n_studies)), each = per_study)
  base_se <- seq(0.05, 0.12, length.out = per_study)
  se_vals <- rep(base_se, times = n_studies)
  effect_base <- seq(0.06, 0.3, length.out = n_studies)
  effect_offsets <- seq(-0.07, 0.07, length.out = per_study)
  effect_vals <- rep(effect_base, each = per_study) + rep(effect_offsets, times = n_studies)
  data.frame(
    study_id = study_ids,
    effect = effect_vals,
    se = se_vals,
    stringsAsFactors = FALSE
  )
}

test_that("nonlinear tests return tidy coefficients and summary", {
  df <- make_demo_data()

  local_options(
    "artma.methods.nonlinear_tests.add_significance_marks" = TRUE,
    "artma.methods.nonlinear_tests.round_to" = 2L,
    "artma.methods.nonlinear_tests.stem_representative_sample" = "medians",
    "artma.methods.nonlinear_tests.selection_cutoffs" = c(1.96, 2.58),
    "artma.methods.nonlinear_tests.selection_symmetric" = FALSE,
    "artma.methods.nonlinear_tests.selection_model" = "normal",
    "artma.methods.nonlinear_tests.hierarchical_iterations" = 50L,
    "artma.output.number_of_decimals" = 3L,
    "artma.verbose" = 0L
  )

  res <- suppressWarnings(nonlinear_tests(df))

  expect_named(
    res,
    c("coefficients", "summary", "skipped", "options"),
    ignore.order = TRUE
  )

  expect_gt(nrow(res$coefficients), 0L)
  expect_named(
    res$coefficients,
    c(
      "model", "model_label", "term", "estimate", "std_error", "p_value",
      "n_obs_total", "n_obs_model", "estimate_formatted", "std_error_formatted"
    )
  )
  expect_setequal(unique(res$coefficients$term), c("publication_bias", "effect"))
  expect_true(all(res$coefficients$n_obs_total == nrow(df)))

  expect_gt(nrow(res$summary), 0L)
  expect_equal(
    rownames(res$summary),
    c(
      "Publication Bias", "(Std. Error)", "Effect Beyond Bias",
      "(Std. Error)", "Total observations", "Model observations"
    )
  )
  expect_equal(res$summary$Metric, rownames(res$summary))
  expect_equal(res$options$round_to, 2L)
})

test_that("nonlinear methods record skipped models when input is insufficient", {
  df <- data.frame(
    effect = 0.15,
    se = 0.2,
    study_id = "S1",
    stringsAsFactors = FALSE
  )
  options <- list(
    add_significance_marks = FALSE,
    round_to = 3L,
    stem_representative_sample = "medians",
    selection_cutoffs = c(1.96),
    selection_symmetric = FALSE,
    selection_model = "normal",
    hierarchical_iterations = 10L
  )

  res <- run_nonlinear_methods(df, options)

  expect_equal(nrow(res$coefficients), 0L)
  expect_equal(nrow(res$summary), 0L)
  expect_named(
    res$skipped,
    c("waap", "top10", "stem", "hierarchical", "selection", "endogenous_kink")
  )
  expect_true(all(vapply(res$skipped, function(entry) {
    is.list(entry) && is.character(entry$reason) && nzchar(entry$reason)
  }, logical(1))))
  expect_equal(res$options, options)
})
