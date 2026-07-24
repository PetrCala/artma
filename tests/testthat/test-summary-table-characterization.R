box::use(
  testthat[
    expect_equal,
    test_that
  ],
  withr[local_options, local_seed]
)

box::use(
  artma / methods / linear_tests[linear_tests],
  artma / methods / nonlinear_tests[nonlinear_tests]
)

# These tests pin the exact printed shape of the linear- and nonlinear-tests
# summary tables on fixed fixtures, ahead of consolidating the three
# `build_summary_table()`/`build_exogeneity_summary()` implementations
# (linear.R, nonlinear.R, exogeneity.R) into one shared builder in
# `libs/formatting/`. Migrated call sites must keep producing the exact same
# data frames captured here. Exogeneity's own build_exogeneity_summary is
# already pinned directly by tests/testthat/test-econometric-exogeneity.R.
#
# Every seeded draw below pins the RNG kind explicitly (via `local_seed()`)
# rather than relying on `set.seed()` alone: other test files can leave the
# global RNG kind changed (e.g. method orchestration switches it to
# "L'Ecuyer-CMRG" for per-method streams), which would otherwise make the
# fixture data, and therefore the pinned tables below, depend on run order.

make_linear_demo_data <- function() {
  local_seed(42, .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
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

make_nonlinear_demo_data <- function() {
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

test_that("linear_tests summary table is pinned on fixture data", {
  testthat::skip_if_not_installed("plm")

  df <- make_linear_demo_data()

  local_options(
    "artma.methods.add_significance_marks" = TRUE,
    "artma.methods.linear_tests.bootstrap_replications" = 10L,
    "artma.methods.linear_tests.conf_level" = 0.9,
    "artma.output.number_of_decimals" = 2,
    "artma.verbose" = 1
  )

  local_seed(100, .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
  res <- suppressWarnings(suppressMessages(linear_tests(df)))

  expected <- structure(
    list(
      Metric = c(
        "Publication Bias", "(Std. Error)", "Bootstrap CI (PB)",
        "Effect Beyond Bias", "(Std. Error)", "Bootstrap CI (Effect)",
        "Total Observations"
      ),
      OLS = c(
        "-0.25", "(0.30)", "[-0.61, 0.38]", "0.21***", "(0.03)",
        "[0.13, 0.25]", "30"
      ),
      `Fixed Effects` = c(
        "-0.19", "(0.33)", "[-0.54, 0.02]", "0.20", "(0.33)",
        "[0.18, 0.25] †", "30"
      ),
      `Between Effects` = c(
        "-1.63", "(2.24)", "[-3.95, 0.21]", "0.36", "(0.25)",
        "[0.17, 0.62] †", "30"
      ),
      `Random Effects` = c(
        "-0.23", "(0.29)", "[-0.59, 0.16]", "0.21***", "(0.03)",
        "[0.18, 0.24]", "30"
      ),
      `Study Weighted OLS` = c(
        "-0.33", "(0.27)", "[-0.67, 0.07]", "0.22***", "(0.02)",
        "[0.19, 0.24]", "30"
      ),
      `Precision Weighted OLS` = c(
        "-0.24", "(0.24)", "[-0.53, 0.03]", "0.21***", "(0.03)",
        "[0.18, 0.23]", "30"
      )
    ),
    row.names = c(
      "Publication Bias", "(Std. Error)", "Bootstrap CI (PB)",
      "Effect Beyond Bias", "(Std. Error)", "Bootstrap CI (Effect)",
      "Total Observations"
    ),
    class = "data.frame"
  )

  expect_equal(res$tables$summary, expected)
})

test_that("nonlinear_tests summary table is pinned on fixture data", {
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

  df <- make_nonlinear_demo_data()
  local_seed(7, .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
  res <- suppressWarnings(nonlinear_tests(df))

  expected <- structure(
    list(
      Metric = c(
        "Publication Bias", "(Std. Error)", "Effect Beyond Bias",
        "(Std. Error)", "Total observations", "Model observations"
      ),
      WAAP = c("NA", "", "0.13***", "(0.01)", "90", "15"),
      Stem = c("NA", "", "0.15***", "(0.02)", "90", "14"),
      Hierarch = c("0.56***", "(0.16)", "0.08", "(0.15)", "90", "90"),
      `Endogenous Kink` = c("0.57***", "(0.03)", "0.10***", "(0.00)", "90", "90")
    ),
    row.names = c(
      "Publication Bias", "(Std. Error)", "Effect Beyond Bias",
      "(Std. Error)", "Total observations", "Model observations"
    ),
    class = "data.frame"
  )

  expect_equal(res$tables$summary, expected)
})
