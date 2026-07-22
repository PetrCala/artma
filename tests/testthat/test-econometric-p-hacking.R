box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_match,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options],
  artma / econometric / p_hacking[
    compute_pvalues,
    resolve_t_stats,
    maive_p_from_coef,
    maive_first_stage_f,
    maive_first_stage_is_weak,
    resolve_maive_first_stage,
    format_maive_results,
    prepare_maive_data,
    run_single_caliper,
    run_binomial,
    run_lcm,
    run_discontinuity,
    run_cox_shi,
    run_fisher,
    run_caliper_tests,
    run_p_hacking_tests,
    resolve_caliper_tail,
    cluster_robust_share_test,
    caliper_direction,
    build_caliper_summary
  ],
  artma / calc / methods / elliott[cox_shi_test, simulate_cdfs_parallel]
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

# resolve_t_stats -------------------------------------------------------------

test_that("resolve_t_stats falls back to effect / se when t_stat is absent", {
  df <- data.frame(effect = c(1, 2), se = c(1, 2))
  expect_equal(resolve_t_stats(df), c(1, 1))
})

test_that("resolve_t_stats prefers a reported t_stat over effect / se", {
  df <- data.frame(effect = c(1, 2), se = c(1, 1), t_stat = c(5, NA))
  # Row 1 uses the reported 5 (not effect/se = 1); row 2 falls back to 2/1 = 2.
  expect_equal(resolve_t_stats(df), c(5, 2))
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

# maive_first_stage_f -------------------------------------------------------

test_that("maive_first_stage_f reads the statistic MAIVE reports", {
  expect_equal(maive_first_stage_f(list(`F-test` = 4.549)), 4.549)
  expect_equal(maive_first_stage_f(list(`F-test` = "80.935")), 80.935)
})

test_that("maive_first_stage_f returns NA when instrumenting is off", {
  # MAIVE sets the field to the literal string "NA" when instrument = 0.
  expect_true(is.na(maive_first_stage_f(list(`F-test` = "NA"))))
  expect_true(is.na(maive_first_stage_f(list())))
  expect_true(is.na(maive_first_stage_f(list(`F-test` = Inf))))
})

# maive_first_stage_is_weak -------------------------------------------------

test_that("maive_first_stage_is_weak applies the F < 10 rule of thumb", {
  expect_true(maive_first_stage_is_weak(4.549))
  expect_false(maive_first_stage_is_weak(10))
  expect_false(maive_first_stage_is_weak(80.935))
  expect_false(maive_first_stage_is_weak(NA_real_))
})

# resolve_maive_first_stage -------------------------------------------------

test_that("resolve_maive_first_stage passes explicit choices through untouched", {
  wide <- c(100, 10^9)

  # A wide N spread would trigger the log form under 2, but 0 stays 0.
  levels_choice <- resolve_maive_first_stage(0, wide)
  expect_equal(levels_choice$value, 0L)
  expect_false(levels_choice$automatic)

  log_choice <- resolve_maive_first_stage(1, c(100, 200))
  expect_equal(log_choice$value, 1L)
  expect_false(log_choice$automatic)
})

test_that("resolve_maive_first_stage picks log once N spans the threshold", {
  # The master-thesis case: 135 to 14.7M, about 5 orders of magnitude.
  res <- resolve_maive_first_stage(2, c(135, 14746755))

  expect_equal(res$value, 1L)
  expect_true(res$automatic)
  expect_equal(res$orders, 5.038, tolerance = 1e-3)
})

test_that("resolve_maive_first_stage keeps levels for a narrow N spread", {
  res <- resolve_maive_first_stage(2, c(500, 900, 4000))

  expect_equal(res$value, 0L)
  expect_true(res$automatic)
  expect_true(res$orders < 3)
})

test_that("resolve_maive_first_stage decides on the threshold boundary", {
  # Exactly 3 orders of magnitude counts as wide.
  expect_equal(resolve_maive_first_stage(2, c(10, 10000))$value, 1L)
  # Just under does not.
  expect_equal(resolve_maive_first_stage(2, c(10, 9999))$value, 0L)
})

test_that("resolve_maive_first_stage ignores unusable sample sizes", {
  # Zero, negative, and non-finite N cannot enter a ratio.
  res <- resolve_maive_first_stage(2, c(0, -5, NA, Inf, 100, 10^6))
  expect_equal(res$value, 1L)
  expect_equal(res$orders, 4)
})

test_that("resolve_maive_first_stage falls back to levels without a usable spread", {
  for (degenerate in list(numeric(0), c(NA_real_, NA_real_), 1000, c(0, -1))) {
    res <- resolve_maive_first_stage(2, degenerate)
    expect_equal(res$value, 0L)
    expect_true(res$automatic)
    expect_true(is.na(res$orders))
  }
})

test_that("resolve_maive_first_stage never reads anything but sample size", {
  # Same N, wildly different effects and SEs: the choice must not move.
  a <- resolve_maive_first_stage(2, c(135, 14746755))
  b <- resolve_maive_first_stage(2, c(135, 14746755))
  expect_equal(a, b)
  expect_equal(names(formals(resolve_maive_first_stage)), c("first_stage", "n_obs"))
})

# format_maive_results ------------------------------------------------------

test_that("format_maive_results flags a weak first stage", {
  out <- format_maive_results(
    list(
      beta = 3.095, SE = 3.612, `pub bias p-value` = 0.241,
      egger_coef = 2.078, egger_se = 1.771,
      `F-test` = 4.549, Hausman = 0.847, Chi2 = 3.841
    ),
    list(round_to = 3)
  )

  expect_true("  Instrument strength" %in% out$Statistic)
  expect_match(out$Value[out$Statistic == "  Instrument strength"], "weak")
})

test_that("format_maive_results leaves a strong first stage unflagged", {
  out <- format_maive_results(
    list(
      beta = 6.993, SE = 0.393, `pub bias p-value` = 0.241,
      egger_coef = 2.078, egger_se = 1.771,
      `F-test` = 80.935, Hausman = 0.847, Chi2 = 3.841
    ),
    list(round_to = 3)
  )

  expect_false("  Instrument strength" %in% out$Statistic)
  expect_equal(out$Value[out$Statistic == "F-test (1st stage IV)"], "80.935")
})

test_that("format_maive_results records the first stage form when given one", {
  output <- list(
    beta = 6.993, SE = 0.393, `pub bias p-value` = 0.241,
    egger_coef = 2.078, egger_se = 1.771,
    `F-test` = 80.935, Hausman = 0.847, Chi2 = 3.841
  )

  auto <- format_maive_results(
    output, list(round_to = 3),
    list(value = 1L, automatic = TRUE, orders = 5.04)
  )
  expect_equal(auto$Value[auto$Statistic == "1st stage form"], "log (auto)")

  explicit <- format_maive_results(
    output, list(round_to = 3),
    list(value = 0L, automatic = FALSE, orders = NA_real_)
  )
  expect_equal(explicit$Value[explicit$Statistic == "1st stage form"], "levels")

  # Omitting it keeps the table exactly as it was before this row existed.
  expect_false("1st stage form" %in% format_maive_results(output, list(round_to = 3))$Statistic)
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

# Caliper tail selection ------------------------------------------------------

test_that("resolve_caliper_tail picks the tail holding most of the mass", {
  expect_equal(resolve_caliper_tail(c(1, 2, 3, -1), tail = "auto"), "positive")
  expect_equal(resolve_caliper_tail(c(-1, -2, -3, 1), tail = "auto"), "negative")
  # Explicit choices are never overridden.
  expect_equal(resolve_caliper_tail(c(-1, -2, -3), tail = "positive"), "positive")
  expect_equal(resolve_caliper_tail(c(1, 2, 3), tail = "absolute"), "absolute")
})

test_that("resolve_caliper_tail defaults to positive without usable values", {
  expect_equal(resolve_caliper_tail(c(0, 0, NA_real_), tail = "auto"), "positive")
  expect_equal(resolve_caliper_tail(numeric(0), tail = "auto"), "positive")
})

test_that("resolve_caliper_tail rejects an unknown tail", {
  expect_error(resolve_caliper_tail(c(1, 2), tail = "sideways"))
})

test_that("run_single_caliper inspects the negative tail in a negative literature", {
  # Excess mass just below -1.96, plus noise well away from the threshold.
  t_stats <- c(rep(-1.97, 18), rep(-1.94, 3), rep(-0.4, 40))

  auto <- run_single_caliper(t_stats, threshold = 1.96, width = 0.05)
  expect_equal(auto$tail, "negative")
  expect_equal(auto$n_above, 18)
  expect_equal(auto$n_below, 3)
  expect_equal(auto$direction, "above")

  # The old signed behaviour looks at the empty positive tail.
  signed <- run_single_caliper(t_stats, threshold = 1.96, width = 0.05, tail = "positive")
  expect_equal(signed$n_above, 0)
  expect_true(is.na(signed$share_above))
})

test_that("run_single_caliper pools both tails when asked", {
  t_stats <- c(rep(-1.97, 10), rep(1.97, 8), rep(-1.94, 2))

  res <- run_single_caliper(t_stats, threshold = 1.96, width = 0.05, tail = "absolute")

  expect_equal(res$tail, "absolute")
  expect_equal(res$n_above, 18)
  expect_equal(res$n_below, 2)
})

# Caliper study clustering ----------------------------------------------------

test_that("run_single_caliper clusters window observations by study", {
  # 16 above / 3 below, but half the above-threshold mass comes from 2 studies.
  t_stats <- c(rep(1.97, 16), rep(1.94, 3))
  study_id <- c(rep("s18", 4), rep("s45", 4), paste0("s", 1:8), c("s1", "s2", "s3"))

  unclustered <- run_single_caliper(t_stats, threshold = 1.96, width = 0.05)
  clustered <- run_single_caliper(t_stats, threshold = 1.96, width = 0.05, study_id = study_id)

  expect_equal(clustered$share_above, unclustered$share_above)
  expect_equal(clustered$cluster_method, "study")
  expect_equal(clustered$n_studies, 10L)
  # Clustering must not make the evidence look stronger than the naive test.
  expect_true(clustered$p_value > unclustered$p_value)
})

test_that("cluster_robust_share_test falls back to the exact test without clustering", {
  above <- c(rep(TRUE, 8), rep(FALSE, 2))

  no_ids <- cluster_robust_share_test(above, NULL)
  expect_equal(no_ids$cluster_method, "none")
  expect_equal(no_ids$p_value, stats::binom.test(8, 10, p = 0.5)$p.value, tolerance = 1e-12)

  # A single study gives no between-study variation to estimate.
  one_study <- cluster_robust_share_test(above, rep("s1", 10))
  expect_equal(one_study$cluster_method, "none")
  expect_equal(one_study$n_studies, 1L)

  # Every study identical: zero between-cluster spread, so fall back as well.
  degenerate <- cluster_robust_share_test(c(TRUE, TRUE, TRUE, TRUE), c("a", "b", "c", "d"))
  expect_true(degenerate$cluster_method %in% c("none", "study"))
})

test_that("cluster_robust_share_test errors on a mismatched study_id length", {
  expect_error(cluster_robust_share_test(c(TRUE, FALSE), c("a")))
})

test_that("run_single_caliper rejects a study_id of the wrong length", {
  expect_error(run_single_caliper(c(1.97, 1.94), study_id = c("a")))
})

# Caliper direction -----------------------------------------------------------

test_that("caliper_direction labels which side carries the excess", {
  expect_equal(caliper_direction(0.8), "above")
  expect_equal(caliper_direction(0.3), "below")
  expect_equal(caliper_direction(0.5), "balanced")
  expect_true(is.na(caliper_direction(NA_real_)))
})

test_that("run_caliper_tests only stars an excess above the threshold", {
  # 4 above / 16 below at the 1.645 threshold: significant, but anti-p-hacking.
  t_stats <- c(rep(1.66, 4), rep(1.60, 16))

  results <- run_caliper_tests(
    t_stats,
    thresholds = 1.645,
    widths = 0.1,
    show_progress = FALSE
  )

  expect_equal(results[[1]]$direction, "below")
  expect_false(grepl("\\*", results[[1]]$p_value))

  # Flip the imbalance and the same p-value does get starred.
  flipped <- run_caliper_tests(
    c(rep(1.66, 16), rep(1.60, 4)),
    thresholds = 1.645,
    widths = 0.1,
    show_progress = FALSE
  )
  expect_equal(flipped[[1]]$direction, "above")
  expect_match(flipped[[1]]$p_value, "\\*")
})

test_that("build_caliper_summary shows a direction row and study counts", {
  t_stats <- c(rep(1.97, 6), rep(1.94, 2))
  study_id <- c("a", "a", "b", "b", "c", "c", "d", "d")

  results <- run_caliper_tests(
    t_stats,
    thresholds = 1.96,
    widths = 0.05,
    study_id = study_id,
    show_progress = FALSE
  )
  summary <- build_caliper_summary(results, list(display_ratios = TRUE))

  expect_true(any(grepl("Direction", summary[[1]])))
  expect_true(any(grepl("excess above", summary[[2]])))
  expect_true(any(grepl("6/2 \\(4 studies\\)", summary[[2]])))
  expect_equal(attr(summary, "caliper_tail"), "positive")
  expect_equal(attr(summary, "caliper_cluster_method"), "study")
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

test_that("run_caliper_tests defaults thresholds to match the options template", {
  set.seed(103)
  t_stats <- rnorm(30)

  results <- run_caliper_tests(t_stats, widths = c(0.1), show_progress = FALSE)

  thresholds_seen <- sort(unique(vapply(results, function(x) x$threshold, numeric(1))))
  expect_equal(thresholds_seen, c(1.645, 1.96, 2.58))
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
    simulate_cdfs_seed = 123L,
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
    maive_seed = 123L,
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
  # 4 rows per width: share above, direction, p-value, n count.
  expect_equal(nrow(result$caliper), 4L)
  expect_equal(ncol(result$caliper), 2L)
})

test_that("run_p_hacking_tests runs the Elliott suite when requested", {
  local_options(
    artma.verbose = 1,
    artma.cache.use_cache = FALSE,
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
  expect_equal(sum(result$elliott$Test == "Observations in [0, 0.1]"), 1L)
  expect_false("Observations <= 0.1" %in% result$elliott$Test)
})

# Skip reasons ---------------------------------------------------------------

test_that("run_p_hacking_tests records a skip reason when the CDF simulation yields no draws", {
  local_options(artma.verbose = 1, artma.cache.use_cache = FALSE)
  df <- make_p_hacking_df()

  result <- run_p_hacking_tests(
    df,
    base_p_hacking_options(
      include_caliper = FALSE,
      include_elliott = TRUE,
      lcm_iterations = 0L
    )
  )

  expect_true(!is.null(result$skipped$lcm_005))
  expect_true(!is.null(result$skipped$lcm_01))
  # The row is still reported (not silently absent), just as NA.
  lcm_rows <- result$elliott[grepl("^LCM", result$elliott$Test), ]
  expect_equal(nrow(lcm_rows), 2L)
})

test_that("run_p_hacking_tests records a skip reason for a singular Cox-Shi covariance", {
  local_options(artma.verbose = 1, artma.cache.use_cache = FALSE)
  set.seed(404, kind = "Mersenne-Twister", normal.kind = "Inversion")
  t_stats <- abs(stats::rnorm(300, mean = 1, sd = 1))
  df <- data.frame(effect = t_stats, se = rep(1, length(t_stats)))

  result <- run_p_hacking_tests(
    df,
    base_p_hacking_options(
      include_caliper = FALSE,
      include_elliott = TRUE,
      include_cox_shi = TRUE,
      cox_shi_bins = 20L
    )
  )

  expect_match(result$skipped$cox_shi_005, "singular")
})

test_that("run_p_hacking_tests records a skip reason when MAIVE lacks n_obs", {
  local_options(artma.verbose = 1)
  df <- make_p_hacking_df()
  df$n_obs <- NULL

  result <- run_p_hacking_tests(
    df,
    base_p_hacking_options(include_caliper = FALSE, include_maive = TRUE)
  )

  expect_match(result$skipped$maive, "n_obs")
  expect_true(is.null(result$maive))
})

test_that("the MAIVE skip reason keeps the install hint when the package is absent", {
  local_options(artma.verbose = 1)
  local_pretend_packages_absent("MAIVE")

  result <- run_p_hacking_tests(
    make_p_hacking_df(),
    base_p_hacking_options(include_caliper = FALSE, include_maive = TRUE)
  )

  expect_true(is.null(result$maive))
  expect_match(result$skipped$maive, "MAIVE")
  # The actionable half lives in a cli bullet, which e$message would drop.
  expect_match(result$skipped$maive, "install.packages")
})

test_that("the MAIVE skip reason reports the installed version when it is too old", {
  # Without the package the absence check fires first and never reaches the
  # version branch under test.
  skip_if_not_installed("MAIVE")
  local_options(artma.verbose = 1)
  local_pretend_package_version("MAIVE", "0.0.2.11")

  result <- run_p_hacking_tests(
    make_p_hacking_df(),
    base_p_hacking_options(include_caliper = FALSE, include_maive = TRUE)
  )

  expect_true(is.null(result$maive))
  expect_match(result$skipped$maive, "0\\.2\\.4 or higher")
  expect_match(result$skipped$maive, "0\\.0\\.2\\.11")
})

test_that("run_lcm skips with a reason instead of failing silently", {
  local_pretend_packages_absent("fdrtool")

  result <- run_lcm(c(0.01, 0.02, 0.03), 0, 0.05, cdfs = c(0.1, 0.2, 0.3))

  expect_true(is.na(result))
  expect_match(attr(result, "reason"), "fdrtool")
})

# LCM / MAIVE reproducibility --------------------------------------------

test_that("simulate_cdfs_parallel is reproducible when seed is passed explicitly", {
  res_a <- simulate_cdfs_parallel(iterations = 20, grid_points = 30, seed = 123, show_progress = FALSE)
  res_b <- simulate_cdfs_parallel(iterations = 20, grid_points = 30, seed = 123, show_progress = FALSE)

  expect_equal(res_a, res_b)
})

test_that("run_p_hacking_tests produces identical LCM p-values across two runs", {
  local_options(artma.verbose = 1, artma.cache.use_cache = FALSE)
  df <- make_p_hacking_df()
  opts <- base_p_hacking_options(
    include_caliper = FALSE,
    include_elliott = TRUE,
    lcm_iterations = 200L,
    lcm_grid_points = 200L
  )

  result_a <- run_p_hacking_tests(df, opts)
  result_b <- run_p_hacking_tests(df, opts)

  lcm_a <- result_a$elliott[grepl("^LCM", result_a$elliott$Test), "P-value"]
  lcm_b <- result_b$elliott[grepl("^LCM", result_b$elliott$Test), "P-value"]
  expect_equal(lcm_a, lcm_b)
})

test_that("run_p_hacking_tests produces identical MAIVE bootstrap results across two runs", {
  skip_if_not_installed("MAIVE")
  local_options(artma.verbose = 1)
  df <- make_p_hacking_df()
  opts <- base_p_hacking_options(
    include_caliper = FALSE,
    include_maive = TRUE,
    maive_se = 3L # Wild bootstrap: irreproducible without a threaded seed.
  )

  result_a <- run_p_hacking_tests(df, opts)
  result_b <- run_p_hacking_tests(df, opts)

  expect_equal(result_a$maive, result_b$maive)
})

# Discontinuity alignment -----------------------------------------------------

test_that("run_discontinuity uses automatic bandwidth and never errors uncaught", {
  local_options(artma.verbose = 1)
  # Sparse, mass-near-cutoff p-values that used to trip the manual retry loop.
  set.seed(40)
  pvalues <- stats::runif(40)

  result <- run_discontinuity(pvalues, cutoff = 0.05)

  expect_true(is.numeric(result))
  ok <- is.na(result) || (is.finite(result) && result >= 0 && result <= 1)
  expect_true(ok)
  if (is.na(result)) {
    expect_true(!is.null(attr(result, "reason")))
  }
})

# Cox-Shi ------------------------------------------------------------------

# Golden values below were produced once with the authors' canonical `Tests.R`
# (nvkudrin/Detecting-p-hacking-Code, Elliott, Kudrin & Wuthrich 2022) on the
# panels built by `cox_shi_panel()`. They are hard-coded on purpose: the tests
# must not reach out to GitHub.
cox_shi_panel <- function(seed, n = 800L) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  t_stats <- abs(stats::rnorm(n, mean = 1.5, sd = 1))
  list(
    pvalues = 2 * (1 - stats::pnorm(t_stats)),
    t_stats = t_stats,
    study_id = 1 + ((seq_len(n) - 1) %/% 20)
  )
}

test_that("cox_shi_test matches the canonical implementation, unclustered", {
  panel <- cox_shi_panel(101)

  # ind of length 1 selects the multinomial (unclustered) covariance.
  expect_equal(
    cox_shi_test(panel$pvalues, 0, 0, 0.05, J = 10, K = 1, use_bounds = 0),
    0.052081436460,
    tolerance = 1e-6
  )
  expect_equal(
    cox_shi_test(panel$pvalues, 0, 0, 0.10, J = 10, K = 2, use_bounds = 1),
    0.019154644996,
    tolerance = 1e-6
  )
})

test_that("cox_shi_test matches the canonical implementation, clustered", {
  panel <- cox_shi_panel(202)

  expect_equal(
    cox_shi_test(panel$pvalues, panel$study_id, 0, 0.05, J = 10, K = 1, use_bounds = 0),
    0.558552296381,
    tolerance = 1e-6
  )
  expect_equal(
    cox_shi_test(panel$pvalues, panel$study_id, 0, 0.10, J = 10, K = 2, use_bounds = 1),
    0.635590223696,
    tolerance = 1e-6
  )
})

test_that("cox_shi_test returns 1 for a monotone p-curve with no binding constraint", {
  # Strictly decreasing bin counts: the interior solution leaves JX = 0, which
  # canonically yields p = 1 rather than NA.
  n_bins <- 10L
  bins <- seq(0, 0.05, length.out = n_bins + 1)
  counts <- c(200, 150, 110, 80, 60, 45, 34, 26, 20, 15)
  pvalues <- unlist(lapply(
    seq_len(n_bins),
    function(j) rep(mean(c(bins[j], bins[j + 1])), counts[j])
  ))

  expect_equal(
    cox_shi_test(pvalues, 0, 0, 0.05, J = n_bins, K = 1, use_bounds = 0),
    1,
    tolerance = 1e-12
  )
  expect_equal(
    cox_shi_test(pvalues, 0, 0, 0.05, J = n_bins, K = 2, use_bounds = 0),
    1,
    tolerance = 1e-12
  )
})

# Bin counts of the class-size p-curve (meta-analysis.cz, PCC estimates) at
# width 0.005 over [0, 0.10]. The curve spikes in [0.045, 0.05]: 36 estimates
# against 16 in the preceding bin, the classic just-under-0.05 signature.
class_shaped_pcurve <- function() {
  counts <- c(
    673, 80, 46, 42, 29, 35, 31, 23, 16, 36,
    19, 21, 19, 20, 19, 20, 20, 11, 11, 18
  )
  edges <- seq(0, 0.10, by = 0.005)
  midpoints <- (edges[-length(edges)] + edges[-1]) / 2
  rep(midpoints, counts)
}

# `cox_shi_bins` is applied to both windows (as in the canonical `Example.R`,
# which fixes J = 10 regardless of the interval), so [0, 0.10] runs at half the
# bin resolution of [0, 0.05]. Its 0.01-wide bins merge the spike with its
# neighbour -- 16 + 36 = 52 against 31 + 23 = 54 -- so the curve reads as
# monotone and the p-value climbs. The nested windows therefore disagree
# sharply. That is a property of the binning, not a defect in the port: on the
# class-size data both windows agree once the bin *width* is held fixed instead
# of the bin *count* (p = 0.0004 on [0, 0.05] with J = 20, p = 0.0044 on
# [0, 0.10] with J = 20). All four values below were cross-checked against the
# canonical `Tests.R` and agreed exactly.
test_that("cox_shi_test reproduces the nested-window disagreement", {
  pvalues <- class_shaped_pcurve()

  expect_equal(
    cox_shi_test(pvalues, 0, 0, 0.05, J = 10, K = 2, use_bounds = 1),
    0.092150969638,
    tolerance = 1e-6
  )
  expect_equal(
    cox_shi_test(pvalues, 0, 0, 0.10, J = 10, K = 2, use_bounds = 1),
    0.362149788835,
    tolerance = 1e-6
  )
})

test_that("cox_shi_test resolves the sub-bin spike when the bin width is halved", {
  pvalues <- class_shaped_pcurve()

  expect_equal(
    cox_shi_test(pvalues, 0, 0, 0.05, J = 20, K = 2, use_bounds = 1),
    0,
    tolerance = 1e-9
  )
  expect_equal(
    cox_shi_test(pvalues, 0, 0, 0.10, J = 20, K = 2, use_bounds = 1),
    0.261350676443,
    tolerance = 1e-6
  )
})

test_that("cox_shi_test skips with a reason when the covariance is singular", {
  # Too few p-values in the window for 20 bins: empty bins make omega singular.
  set.seed(404, kind = "Mersenne-Twister", normal.kind = "Inversion")
  pvalues <- 2 * (1 - stats::pnorm(abs(stats::rnorm(300, mean = 1, sd = 1))))

  result <- cox_shi_test(pvalues, seq_along(pvalues), 0, 0.05, J = 20L, K = 2L, use_bounds = 1L)

  expect_true(is.na(result))
  expect_match(attr(result, "reason"), "singular")
})

test_that("run_cox_shi preserves the skip reason for the caller to record", {
  set.seed(404, kind = "Mersenne-Twister", normal.kind = "Inversion")
  pvalues <- 2 * (1 - stats::pnorm(abs(stats::rnorm(300, mean = 1, sd = 1))))

  result <- run_cox_shi(pvalues, seq_along(pvalues), 0, 0.05, n_bins = 20L)

  expect_true(is.na(result))
  expect_match(attr(result, "reason"), "singular")
})

test_that("run_p_hacking_tests reports numeric Cox-Shi rows on clustered data", {
  local_options(
    artma.verbose = 1,
    artma.cache.use_cache = FALSE,
    artma.methods.p_hacking_tests.simulate_cdfs.use_cpp = FALSE
  )
  panel <- cox_shi_panel(202)
  df <- data.frame(
    effect = panel$t_stats,
    se = rep(1, length(panel$t_stats)),
    study_id = panel$study_id
  )

  result <- run_p_hacking_tests(
    df,
    base_p_hacking_options(
      include_caliper = FALSE,
      include_elliott = TRUE,
      include_cox_shi = TRUE,
      cox_shi_bins = 10L
    )
  )

  cox_rows <- result$elliott[grepl("^Cox-Shi", result$elliott$Test), ]
  expect_equal(nrow(cox_rows), 2)
  expect_false(any(grepl("NA", cox_rows$`P-value`)))
})
