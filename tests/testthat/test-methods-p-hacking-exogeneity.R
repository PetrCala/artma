box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options],
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
    artma.methods.p_hacking_tests.include_maive = FALSE,
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
})

test_that("p_hacking_tests aborts when a required column is missing", {
  local_options(artma.verbose = 1)
  # Missing t_stat and study_id.
  expect_error(p_hacking_tests(data.frame(effect = 1:3, se = rep(1, 3))))
})

test_that("p_hacking_tests prints a significance legend matching significance_mark thresholds", {
  local_caliper_only_options()

  output <- utils::capture.output(p_hacking_tests(make_p_hacking_df()), type = "message")

  expect_true(any(grepl("Significance marks: \\* p <= 0.1, \\*\\* p <= 0.05, \\*\\*\\* p <= 0.01", output)))
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
  skip_if_not_installed("ivmodel")
  local_options(artma.verbose = 1)

  result <- exogeneity_tests(make_exogeneity_df())

  expect_true(is.list(result))
  expect_true(is.data.frame(result$tables$summary))
  expect_equal(nrow(result$tables$summary), 6L)
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
