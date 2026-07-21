box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_true,
    test_that
  ],
  withr[local_options],
  artma / econometric / p_hacking[
    compute_pvalues,
    maive_p_from_coef,
    prepare_maive_data,
    run_single_caliper,
    run_binomial,
    run_fisher,
    run_caliper_tests,
    run_p_hacking_tests
  ]
)

# compute_pvalues -----------------------------------------------------------

test_that("compute_pvalues returns two-sided normal p-values", {
  # t = effect / se; p = 2 * pnorm(-|t|)
  effect <- c(0, 1.96, qnorm(0.995), -1.96)
  se <- c(1, 1, 1, 1)

  pvals <- compute_pvalues(effect, se)

  expect_equal(pvals[1], 1, tolerance = 1e-12)
  expect_equal(pvals[2], 0.0499957902964409, tolerance = 1e-12)
  expect_equal(pvals[3], 0.01, tolerance = 1e-12)
  # Symmetric in the sign of the effect
  expect_equal(pvals[2], pvals[4], tolerance = 1e-12)
})

test_that("compute_pvalues errors on mismatched lengths", {
  expect_error(compute_pvalues(c(1, 2), c(1)))
})

# maive_p_from_coef ---------------------------------------------------------

test_that("maive_p_from_coef matches the two-sided normal p-value", {
  expect_equal(maive_p_from_coef(1.96, 1), 0.0499957902964409, tolerance = 1e-12)
  expect_equal(maive_p_from_coef(0, 2), 1, tolerance = 1e-12)
})

test_that("maive_p_from_coef returns NA for degenerate inputs", {
  expect_true(is.na(maive_p_from_coef(1, 0)))
  expect_true(is.na(maive_p_from_coef(1, -1)))
  expect_true(is.na(maive_p_from_coef(Inf, 1)))
  expect_true(is.na(maive_p_from_coef(1, NA_real_)))
})

# prepare_maive_data --------------------------------------------------------

test_that("prepare_maive_data renames columns to the MAIVE contract", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.05, 0.06, 0.07),
    n_obs = c(100, 200, 300),
    study_id = c(1, 1, 2)
  )

  out <- prepare_maive_data(df)

  expect_equal(colnames(out), c("bs", "sebs", "Ns", "studyid"))
  expect_equal(out$bs, df$effect)
  expect_equal(out$sebs, df$se)
  expect_equal(out$Ns, df$n_obs)
  expect_equal(out$studyid, df$study_id)
})

test_that("prepare_maive_data omits studyid when study_id is absent", {
  df <- data.frame(effect = 1, se = 0.1, n_obs = 10)
  out <- prepare_maive_data(df)
  expect_equal(colnames(out), c("bs", "sebs", "Ns"))
})

test_that("prepare_maive_data errors when required columns are missing", {
  expect_error(prepare_maive_data(data.frame(effect = 1, se = 0.1)))
})

# run_single_caliper --------------------------------------------------------

test_that("run_single_caliper counts observations on each side of the threshold", {
  # Interval around 1.96 with width 0.05 is (1.91, 2.01).
  #   in-interval: 1.92, 1.93 (below), 1.97, 1.99 (above)
  #   excluded:    0.50, 3.00
  t_stats <- c(1.92, 1.93, 1.97, 1.99, 0.50, 3.00)

  res <- run_single_caliper(t_stats, threshold = 1.96, width = 0.05)

  expect_equal(res$n_above, 2)
  expect_equal(res$n_below, 2)
  expect_equal(res$share_above, 0.5)
  # binom.test(2, 4, p = 0.5)$p.value
  expect_equal(res$p_value, 1, tolerance = 1e-12)
})

test_that("run_single_caliper returns NA estimate when the interval is empty", {
  t_stats <- c(0.1, 0.2, 0.3)
  res <- run_single_caliper(t_stats, threshold = 1.96, width = 0.05)

  expect_true(is.na(res$share_above))
  expect_true(is.na(res$p_value))
  expect_equal(res$n_above, 0)
  expect_equal(res$n_below, 0)
})

test_that("run_single_caliper does not flag clean data (no discontinuity)", {
  # Uniform t-stats around the threshold: no excess mass above it.
  set.seed(259)
  t_stats <- stats::runif(400, 1.86, 2.06)

  res <- run_single_caliper(t_stats, threshold = 1.96, width = 0.1)

  expect_true(res$p_value > 0.05)
})

test_that("run_single_caliper flags hacked data (excess mass just above threshold)", {
  # Almost everything in the window sits just above the threshold.
  t_stats <- c(rep(1.97, 38), rep(1.94, 2))

  res <- run_single_caliper(t_stats, threshold = 1.96, width = 0.05)

  expect_true(res$p_value < 0.05)
})

# run_caliper_tests ---------------------------------------------------------

test_that("run_caliper_tests produces one cell per threshold-width pair", {
  local_options(artma.verbose = 1)
  set.seed(101)
  t_stats <- c(rnorm(50, 1.96, 0.1), rnorm(50, 0, 1))

  results <- run_caliper_tests(
    t_stats = t_stats,
    thresholds = c(1.96, 2.58),
    widths = c(0.05, 0.1),
    show_progress = FALSE
  )

  expect_equal(length(results), 4L)
  thresholds_seen <- vapply(results, function(x) x$threshold, numeric(1))
  widths_seen <- vapply(results, function(x) x$width, numeric(1))
  expect_equal(sort(unique(thresholds_seen)), c(1.96, 2.58))
  expect_equal(sort(unique(widths_seen)), c(0.05, 0.1))
  expect_true(all(vapply(results, function(x) x$n_above >= 0, logical(1))))
})

test_that("run_caliper_tests dedupes duplicated thresholds and widths", {
  local_options(artma.verbose = 1)
  set.seed(102)
  t_stats <- rnorm(100, 1.96, 0.2)

  results <- run_caliper_tests(
    t_stats = t_stats,
    thresholds = c(1.96, 1.96),
    widths = c(0.05, 0.05),
    show_progress = FALSE
  )

  expect_equal(length(results), 1L)
})

# Elliott wrappers ----------------------------------------------------------

test_that("run_binomial matches the upper-tail binomial p-value", {
  # p-values inside [0, 0.05]; midpoint is 0.025.
  #   above midpoint: 0.03, 0.04  -> kk = 2, nn = 4
  #   p = 1 - pbinom(kk - 1, nn, 0.5) = 1 - pbinom(1, 4, 0.5) = 0.6875
  pvalues <- c(0.01, 0.02, 0.03, 0.04)
  expect_equal(run_binomial(pvalues, 0, 0.05, type = "c"), 0.6875, tolerance = 1e-12)
})

test_that("run_binomial validates its interval and type", {
  expect_error(run_binomial(c(0.01, 0.02), 0.05, 0.05))
  expect_error(run_binomial(c(0.01, 0.02), 0, 0.05, type = "x"))
})

test_that("run_fisher matches the chi-squared tail probability", {
  # stat = -2 * sum(log(1 - (p - p_min) / (p_max - p_min))); df = 2 * n
  pvalues <- c(0.01, 0.02)
  expect_equal(run_fisher(pvalues, 0, 0.05), 0.832305204038496, tolerance = 1e-12)
})

test_that("run_fisher flags clustering just below the threshold", {
  # p-values bunched near the upper bound (just-significant) are the p-hacking
  # signature and must yield a smaller p-value than p-values spread near zero.
  near_threshold <- c(0.048, 0.049, 0.0495, 0.0499)
  near_zero <- c(0.001, 0.002, 0.003, 0.004)

  p_suspicious <- run_fisher(near_threshold, 0, 0.05)
  p_clean <- run_fisher(near_zero, 0, 0.05)

  expect_true(p_suspicious < p_clean)
  expect_true(p_suspicious < 0.05)
})

# run_p_hacking_tests integration -------------------------------------------

base_p_hacking_options <- function(...) {
  defaults <- list(
    include_caliper = TRUE,
    caliper_thresholds = c(1.96, 2.58),
    caliper_widths = c(0.05, 0.1),
    caliper_display_ratios = TRUE,
    include_elliott = FALSE,
    lcm_iterations = 50L,
    lcm_grid_points = 50L,
    simulate_cdfs_chunk_size = 512L,
    include_discontinuity = FALSE,
    discontinuity_bandwidth = 0.05,
    include_cox_shi = FALSE,
    cox_shi_bins = 20L,
    cox_shi_order = 2L,
    cox_shi_bounds = 1L,
    include_maive = FALSE,
    maive_method = 3L,
    maive_weight = 0L,
    maive_instrument = 1L,
    maive_studylevel = 2L,
    maive_se = 1L,
    maive_ar = 0L,
    maive_first_stage = 0L,
    add_significance_marks = TRUE,
    round_to = 3L
  )
  utils::modifyList(defaults, list(...))
}

make_p_hacking_df <- function() {
  # 6 observations with hand-known two-sided p-values via t = effect / se.
  #   t = 2.576 -> p = 0.010 (significant at 0.05)
  #   t = 2.100 -> p ~ 0.036 (significant at 0.05)
  #   t = 1.960 -> p = 0.050 (significant at 0.05, boundary inclusive)
  #   t = 1.500 -> p ~ 0.134
  #   t = 1.000 -> p ~ 0.317
  #   t = 0.500 -> p ~ 0.617
  t_stats <- c(2.576, 2.100, 1.960, 1.500, 1.000, 0.500)
  data.frame(
    effect = t_stats,
    se = rep(1, length(t_stats)),
    study_id = c(1, 1, 2, 2, 3, 3),
    n_obs = c(100, 120, 140, 160, 180, 200)
  )
}

test_that("run_p_hacking_tests reports p-value counts and a caliper table", {
  local_options(artma.verbose = 1)
  df <- make_p_hacking_df()

  result <- run_p_hacking_tests(df, base_p_hacking_options())

  expect_equal(result$n_pvalues, nrow(df))
  # p <= 0.05 for the first three rows (p = 0.010, 0.036, 0.050).
  expect_equal(result$n_significant_005, 3L)
  # p <= 0.10 adds none of the remaining rows (next is p ~ 0.134).
  expect_equal(result$n_significant_010, 3L)
  expect_true(is.data.frame(result$caliper))
  expect_true(nrow(result$caliper) > 0)
  expect_true(any(grepl("Share above", result$caliper[[1]])))
  expect_true(any(grepl("p-value", result$caliper[[1]])))
})

test_that("run_p_hacking_tests dedupes duplicated caliper thresholds/widths without crashing", {
  local_options(artma.verbose = 1)
  df <- make_p_hacking_df()

  result <- run_p_hacking_tests(
    df,
    base_p_hacking_options(caliper_thresholds = c(1.96, 1.96), caliper_widths = c(0.1, 0.1))
  )

  expect_true(is.data.frame(result$caliper))
  expect_equal(nrow(result$caliper), 3L)
  expect_equal(ncol(result$caliper), 2L)
})

test_that("run_p_hacking_tests skips gracefully when required columns are missing", {
  local_options(artma.verbose = 1)
  df <- data.frame(effect = c(0.1, 0.2, 0.3))

  result <- run_p_hacking_tests(df, base_p_hacking_options())

  expect_true(!is.null(result$skipped))
  expect_true(grepl("se", result$skipped$reason))
})

test_that("run_p_hacking_tests runs the Elliott suite when requested", {
  local_options(
    artma.verbose = 1,
    artma.methods.p_hacking_tests.simulate_cdfs.use_cpp = FALSE
  )
  set.seed(202)
  df <- make_p_hacking_df()

  result <- run_p_hacking_tests(
    df,
    base_p_hacking_options(include_caliper = FALSE, include_elliott = TRUE)
  )

  expect_true(is.data.frame(result$elliott))
  expect_true("Binomial [0, 0.05]" %in% result$elliott$Test)
  expect_true("Fisher [0, 0.05]" %in% result$elliott$Test)
})
