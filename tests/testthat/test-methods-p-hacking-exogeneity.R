box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_match,
    expect_no_error,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options, with_options],
  artma / data / mock[create_mock_df],
  artma / methods / p_hacking_tests[p_hacking_tests],
  artma / methods / exogeneity_tests[exogeneity_tests]
)

# p_hacking_tests wrapper ---------------------------------------------------

# Keep the run fast and dependency-light: caliper only.
local_caliper_only_options <- function(.local_envir = parent.frame()) {
  local_options(
    artma.verbose = 1,
    artma.methods.p_hacking_tests.include_elliott = FALSE,
    artma.methods.p_hacking_tests.include_discontinuity = FALSE,
    artma.methods.p_hacking_tests.include_cox_shi = FALSE,
    .local_envir = .local_envir
  )
}

make_p_hacking_df <- function(seed = 1, n = 60) {
  set.seed(seed)
  df <- data.frame(
    effect = rnorm(n, 0.3, 0.1),
    se = runif(n, 0.05, 0.3),
    study_id = rep(seq_len(15), length.out = n)
  )
  df$t_stat <- df$effect / df$se
  df
}

test_that("p_hacking_tests returns the standard contract with a caliper table", {
  local_caliper_only_options()

  result <- p_hacking_tests(make_p_hacking_df())

  expect_true(is.list(result))
  expect_true(is.data.frame(result$tables$caliper))
  expect_true(nrow(result$tables$caliper) > 0)

  # The caliper grid also arrives as long-format estimates: one row per
  # threshold-width pair, with the share above the threshold as the estimate.
  estimates <- result$estimates
  expect_true(nrow(estimates) > 0)
  expect_true(all(grepl("^threshold_", estimates$model)))
  expect_true(all(grepl("^width_", estimates$term)))
  expect_true(is.numeric(estimates$estimate))
})

test_that("p_hacking_tests aborts when a required column is missing", {
  local_options(artma.verbose = 1)
  # Missing t_stat and study_id.
  expect_error(p_hacking_tests(data.frame(effect = 1:3, se = rep(1, 3))))
})

test_that("p_hacking_tests prints a significance legend matching significance_mark thresholds", {
  local_caliper_only_options()

  output <- testthat::capture_messages(p_hacking_tests(make_p_hacking_df()))

  expect_true(any(grepl("Significance marks: \\* p <= 0.1, \\*\\* p <= 0.05, \\*\\*\\* p <= 0.01", output)))
})

# Elliott supports and Cox-Shi skip reasons ----------------------------------

elliott_base_options <- function() {
  list(
    artma.verbose = 1,
    artma.cache.use_cache = FALSE,
    artma.methods.p_hacking_tests.include_caliper = FALSE,
    artma.methods.p_hacking_tests.include_discontinuity = FALSE,
    artma.methods.p_hacking_tests.lcm_iterations = 50,
    artma.methods.p_hacking_tests.lcm_grid_points = 50
  )
}

# A realistic clustered p-curve (it respects the Elliott theoretical bounds,
# so the default Cox-Shi windows stay quiet), plus a heap of p-values at
# 0.125. The heap is invisible to the default supports but breaks monotonicity
# on a [0, 0.15] window, so widening the support flips the conclusion.
make_heap_bump_df <- function() {
  set.seed(202, kind = "Mersenne-Twister", normal.kind = "Inversion")
  panel_t <- abs(stats::rnorm(800, mean = 1.5, sd = 1))
  heap_t <- rep(stats::qnorm(1 - 0.125 / 2), 150)
  t_stats <- c(panel_t, heap_t)
  study_id <- c(
    1 + ((seq_along(panel_t) - 1) %/% 20),
    41 + ((seq_along(heap_t) - 1) %/% 20)
  )
  data.frame(
    effect = t_stats,
    se = rep(1, length(t_stats)),
    t_stat = t_stats,
    study_id = study_id
  )
}

# Strip significance marks from a formatted p-value cell.
formatted_p_to_num <- function(x) as.numeric(sub("\\*+$", "", x))

test_that("a custom elliott_supports option flows through to the Cox-Shi call", {
  skip_if_not_installed("NlcOptim")
  skip_if_not_installed("quadprog")
  df <- make_heap_bump_df()

  run_with_supports <- function(supports) {
    opts <- elliott_base_options()
    opts$artma.methods.p_hacking_tests.elliott_supports <- supports
    with_options(opts, p_hacking_tests(df))
  }

  default_result <- run_with_supports(c(0.05, 0.1))
  custom_result <- run_with_supports(c(0.05, 0.15))

  expect_true("Cox-Shi [0, 0.15]" %in% custom_result$tables$elliott$Test)
  expect_false(any(grepl("[0, 0.10]", custom_result$tables$elliott$Test, fixed = TRUE)))

  default_p <- default_result$tables$elliott[
    default_result$tables$elliott$Test == "Cox-Shi [0, 0.10]", "P-value"
  ]
  custom_p <- custom_result$tables$elliott[
    custom_result$tables$elliott$Test == "Cox-Shi [0, 0.15]", "P-value"
  ]
  # Both windows yield a numeric p-value and the support choice changes it:
  # the heap at p = 0.125 rejects only on the wider window.
  expect_false(grepl("NA", default_p))
  expect_false(grepl("NA", custom_p))
  expect_true(formatted_p_to_num(default_p) > 0.05)
  expect_true(formatted_p_to_num(custom_p) < 0.05)
  expect_false(identical(default_p, custom_p))
})

test_that("p_hacking_tests surfaces the Cox-Shi skip reason in output and meta", {
  # P-values heaped on two points leave most Cox-Shi bins empty, making the
  # bin covariance singular, so the test must skip with a visible reason.
  pvalues <- rep(c(0.001, 0.045), each = 100)
  t_stats <- stats::qnorm(1 - pvalues / 2)
  df <- data.frame(
    effect = t_stats,
    se = rep(1, length(t_stats)),
    t_stat = t_stats,
    study_id = rep(seq_len(20), each = 10)
  )

  # capture_messages() forces its `code` argument lazily in this frame (like
  # the capture.output() it replaces), so a plain assignment here lands in
  # this test_that block's own scope; <<- would skip past it instead.
  result <- NULL
  messages <- testthat::capture_messages(
    with_options(elliott_base_options(), result <- p_hacking_tests(df))
  )

  # The summary table keeps NA in the p-value column.
  cox_rows <- result$tables$elliott[grepl("^Cox-Shi", result$tables$elliott$Test), ]
  expect_equal(nrow(cox_rows), 2L)
  expect_true(all(grepl("NA", cox_rows$`P-value`)))

  # The reason lands in meta for programmatic consumers.
  expect_match(result$meta$skipped_models$cox_shi_005$reason, "singular")
  expect_equal(result$meta$skipped_models$cox_shi_005$label, "Cox-Shi [0, 0.05]")

  # And it is printed after the summary table instead of a bare NA.
  expect_true(any(grepl("singular", messages)))
  expect_true(any(grepl("Cox-Shi \\[0, 0.05\\]", messages)))
})

# exogeneity_tests wrapper --------------------------------------------------

make_exogeneity_df <- function(seed = 2024, n = 200) {
  set.seed(seed)
  n_obs <- sample(30:600, n, replace = TRUE)
  se <- 2 / sqrt(n_obs) + abs(rnorm(n, 0, 0.01))
  effect <- 0.5 + 1.0 * se + rnorm(n, 0, 0.05)
  data.frame(
    effect = effect,
    se = se,
    study_id = rep(seq_len(40), length.out = n),
    n_obs = n_obs,
    study_size = n_obs
  )
}

test_that("exogeneity_tests returns the standard contract with IV results", {
  skip_if_not_installed("AER")
  local_options(artma.verbose = 1)

  result <- exogeneity_tests(make_exogeneity_df())

  expect_true(is.list(result))
  expect_true(is.data.frame(result$tables$summary))
  expect_equal(nrow(result$tables$summary), 7L)
  # The IV model lives in the meta slot and recovers the true effect (mu = 0.5).
  iv_coef <- result$meta$iv$coefficients
  effect_est <- iv_coef$estimate[iv_coef$term == "effect"]
  expect_equal(effect_est, 0.5, tolerance = 0.05)
})

test_that("exogeneity_tests aborts when a required column is missing", {
  local_options(artma.verbose = 1)
  df <- make_exogeneity_df(n = 30)
  expect_error(exogeneity_tests(df[, c("effect", "se")]))
})

test_that("exogeneity_tests handles mock data with too few significant p-uniform studies", {
  skip_if_not_installed("AER")
  local_options(artma.verbose = 1)

  # The package's own mock generator spreads effect/se uniformly at random, so
  # almost no study-level median clears the significance threshold p-uniform*
  # requires. That drives run_puniform_star() down its early-return path,
  # which used to omit the estimate_formatted/std_error_formatted columns and
  # crash build_exogeneity_summary() with "replacement has 4 rows, data has 6".
  df <- create_mock_df(nrow = 600, n_studies = 30)
  df$study_size <- unname(as.integer(table(df$study_id)[as.character(df$study_id)]))

  result <- expect_no_error(exogeneity_tests(df))

  expect_true(is.data.frame(result$tables$summary))
  expect_equal(nrow(result$tables$summary), 7L)
})
