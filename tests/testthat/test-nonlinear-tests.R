box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_gt,
    expect_identical,
    expect_match,
    expect_named,
    expect_s3_class,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_options, local_tempdir]
)

box::use(
  artma / methods / nonlinear_tests[nonlinear_tests],
  artma / econometric / nonlinear[run_nonlinear_methods]
)

make_demo_data <- function() {
  # STEM picks the MSE-minimising number of (precision-ordered) studies; a
  # small, widely spread study set can land on its algorithmic floor of 3,
  # which the panel now treats as a degenerate corner solution and skips.
  # More studies with tightly clustered effects keep STEM's window well
  # above that floor so this fixture continues to exercise every model.
  n_studies <- 15L
  per_study <- 6L
  study_ids <- rep(paste0("S", seq_len(n_studies)), each = per_study)
  base_se <- seq(0.05, 0.12, length.out = per_study)
  se_vals <- rep(base_se, times = n_studies)
  effect_base <- 0.15 + seq(-0.01, 0.01, length.out = n_studies)
  effect_offsets <- seq(-0.02, 0.02, length.out = per_study)
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
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 0L
  )

  res <- suppressWarnings(nonlinear_tests(df))

  expect_named(res, c("tables", "plots", "meta"))
  expect_named(res$tables, "summary")
  expect_named(res$plots, c("stem_funnel", "stem_mse"))
  expect_s3_class(res$plots$stem_funnel, "recordedplot")
  expect_s3_class(res$plots$stem_mse, "recordedplot")
  expect_named(
    res$meta,
    c("coefficients", "skipped", "options"),
    ignore.order = TRUE
  )

  expect_gt(nrow(res$meta$coefficients), 0L)
  expect_named(
    res$meta$coefficients,
    c(
      "model", "model_label", "term", "estimate", "std_error", "p_value",
      "n_obs_total", "n_obs_model", "estimate_formatted", "std_error_formatted"
    )
  )
  expect_setequal(unique(res$meta$coefficients$term), c("publication_bias", "effect"))
  expect_true(all(res$meta$coefficients$n_obs_total == nrow(df)))

  expect_gt(nrow(res$tables$summary), 0L)
  expect_equal(
    rownames(res$tables$summary),
    c(
      "Publication Bias", "(Std. Error)", "Effect Beyond Bias",
      "(Std. Error)", "Total observations", "Model observations"
    )
  )
  expect_equal(res$tables$summary$Metric, rownames(res$tables$summary))
  expect_equal(res$meta$options$round_to, 2L)
})

test_that("nonlinear tests writes STEM diagnostic plot files when export is enabled", {
  df <- make_demo_data()
  dir <- local_tempdir()

  local_options(
    "artma.methods.nonlinear_tests.stem_representative_sample" = "medians",
    "artma.visualization.export_graphics" = TRUE,
    "artma.visualization.export_path" = dir,
    "artma.output.save_results" = FALSE,
    "artma.verbose" = 0L
  )

  res <- suppressWarnings(nonlinear_tests(df))

  expect_setequal(list.files(dir), c("stem_funnel.png", "stem_mse.png"))
  expect_s3_class(res$plots$stem_funnel, "recordedplot")
  expect_s3_class(res$plots$stem_mse, "recordedplot")
})

make_degenerate_options <- function() {
  list(
    add_significance_marks = FALSE,
    round_to = 3L,
    stem_representative_sample = "medians",
    selection_cutoffs = c(1.96),
    selection_symmetric = FALSE,
    selection_model = "normal",
    hierarchical_iterations = 10L
  )
}

# Characterization test for B7 (formatting-helper consolidation): pins the
# exact formatted cells produced by the deterministic estimators (WAAP, STEM),
# including the current literal "NA" string rendered for a model that has no
# publication-bias term. This must stay byte-for-byte identical across the
# format_estimate()/format_standard_error()/normal_p_value() consolidation.
test_that("WAAP and STEM formatted cells are pinned across the formatting refactor", {
  df <- make_demo_data()
  options <- list(
    add_significance_marks = TRUE,
    round_to = 2L,
    stem_representative_sample = "medians",
    selection_cutoffs = c(1.96, 2.58),
    selection_symmetric = FALSE,
    selection_model = "normal",
    hierarchical_iterations = 50L
  )

  res <- suppressWarnings(run_nonlinear_methods(df, options))

  waap_rows <- res$coefficients[res$coefficients$model == "waap", ]
  waap_pb <- waap_rows[waap_rows$term == "publication_bias", ]
  waap_eff <- waap_rows[waap_rows$term == "effect", ]
  expect_identical(waap_pb$estimate_formatted, "NA")
  expect_identical(waap_pb$std_error_formatted, "")
  expect_identical(waap_eff$estimate_formatted, "0.13***")
  expect_identical(waap_eff$std_error_formatted, "(0.01)")

  stem_rows <- res$coefficients[res$coefficients$model == "stem", ]
  stem_pb <- stem_rows[stem_rows$term == "publication_bias", ]
  stem_eff <- stem_rows[stem_rows$term == "effect", ]
  expect_identical(stem_pb$estimate_formatted, "NA")
  expect_identical(stem_pb$std_error_formatted, "")
  expect_identical(stem_eff$estimate_formatted, "0.15***")
  expect_identical(stem_eff$std_error_formatted, "(0.02)")

  summary <- res$summary
  expect_identical(summary$WAAP[summary$Metric == "Publication Bias"], "NA")
  expect_identical(summary$Stem[summary$Metric == "Publication Bias"], "NA")
})

test_that("WAAP and Top10 are skipped when a single observation dominates the weights", {
  n_moderate <- 20
  df <- data.frame(
    effect = rep(0.2, n_moderate + 1),
    se = c(rep(0.05, n_moderate), 0.0001),
    study_id = paste0("S", seq_len(n_moderate + 1)),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(run_nonlinear_methods(df, make_degenerate_options()))

  expect_true("waap" %in% names(res$skipped))
  expect_match(res$skipped$waap$reason, "dominated by a single extreme-precision observation")
  expect_false("waap" %in% res$coefficients$model)
})

test_that("Top10 is skipped when a single observation dominates the weights", {
  df <- data.frame(
    effect = rep(0.2, 50),
    se = c(rep(0.05, 45), rep(0.01, 4), 0.0001),
    study_id = paste0("S", seq_len(50)),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(run_nonlinear_methods(df, make_degenerate_options()))

  expect_true("top10" %in% names(res$skipped))
  expect_match(res$skipped$top10$reason, "dominated by a single extreme-precision observation")
  expect_false("top10" %in% res$coefficients$model)
})

test_that("STEM is skipped when it lands on its algorithmic minimum window", {
  set.seed(123)
  n <- 30
  se <- seq(0.02, 0.3, length.out = n)
  effect <- c(0.9, 0.95, 0.85, rnorm(n - 3, 0, 0.05))
  df <- data.frame(
    effect = effect,
    se = se,
    study_id = paste0("S", seq_len(n)),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(run_nonlinear_methods(df, make_degenerate_options()))

  expect_true("stem" %in% names(res$skipped))
  expect_match(res$skipped$stem$reason, "corner solution")
  expect_false("stem" %in% res$coefficients$model)
})

test_that("Selection model is skipped on a boundary (non-converged) solution", {
  set.seed(42)
  n <- 60
  df <- data.frame(
    effect = rnorm(n, 0, 0.001),
    se = rep(1, n),
    study_id = paste0("S", seq_len(n)),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(run_nonlinear_methods(df, make_degenerate_options()))

  expect_true("selection" %in% names(res$skipped))
  expect_false("selection" %in% res$coefficients$model)
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
