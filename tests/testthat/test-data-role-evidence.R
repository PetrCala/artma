box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_gte,
    expect_lte,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(
  artma / data / role_evidence[
    MIN_ROWS_FOR_EVIDENCE,
    coerce_numeric_column,
    profile_role_values,
    score_role_evidence,
    score_pair_consistency,
    score_triple_consistency,
    assign_core_roles
  ]
)


# A per-study coefficient counter like the idcoeff column that triggered the
# original mis-detection: restarts at 1 for every study.
make_counter <- function(n_studies = 12, k = 6) {
  rep(seq_len(k), times = n_studies)
}


# Tests for coerce_numeric_column
test_that("coerce_numeric_column passes numeric input through", {
  values <- c(1.5, 2.5, NA, 3.5)
  expect_equal(coerce_numeric_column(values), values)
})


test_that("coerce_numeric_column parses numeric strings (Excel text reads)", {
  values <- c("0.038", "1.268", "-0.086", NA)
  result <- coerce_numeric_column(values)
  expect_equal(result, c(0.038, 1.268, -0.086, NA))
})


test_that("coerce_numeric_column rejects genuinely textual columns", {
  expect_null(coerce_numeric_column(c("Smith (2001)", "Jones (2002)")))
  expect_null(coerce_numeric_column(c("a", "b", "1", "2")))
})


# Tests for profile_role_values
test_that("profile_role_values flags a per-study counter as identifier-like", {
  profile <- profile_role_values(make_counter())

  expect_true(profile$is_within_group_counter)
  expect_true(profile$is_id_like)
  expect_false(profile$is_sequential)
})


test_that("profile_role_values flags global sequences and years", {
  expect_true(profile_role_values(1:50)$is_sequential)
  expect_true(profile_role_values(seq(10, 500, by = 10))$is_arithmetic)

  withr::local_seed(42)
  years <- sample(1990:2015, 60, replace = TRUE)
  expect_true(profile_role_values(years)$is_year_like)
})


test_that("profile_role_values flags near-unique integer codes", {
  withr::local_seed(42)
  codes <- sample(10000:99999, 40)
  profile <- profile_role_values(codes)

  expect_true(profile$is_near_unique_integer)
  expect_true(profile$is_id_like)
})


test_that("profile_role_values does not flag continuous data as identifier-like", {
  withr::local_seed(42)
  profile <- profile_role_values(stats::rnorm(100, 0.3, 0.5))

  expect_false(profile$is_id_like)
  expect_true(profile$has_both_signs)
  expect_gte(profile$non_integer_share, 0.99)
})


test_that("profile_role_values does not flag a repeated study key as a counter", {
  # rep(1:10, each = 5) is a legitimate numeric study id, not a counter
  profile <- profile_role_values(rep(1:10, each = 5))
  expect_false(profile$is_within_group_counter)
  expect_false(profile$is_id_like)
})


# Tests for score_role_evidence
test_that("score_role_evidence rates continuous signed data high for effect", {
  withr::local_seed(42)
  score <- score_role_evidence(stats::rnorm(100, 0.2, 0.4), "effect")
  expect_gte(score, 0.8)
})


test_that("score_role_evidence gives identifier columns zero effect evidence", {
  expect_equal(score_role_evidence(make_counter(), "effect"), 0)
  expect_equal(score_role_evidence(1:80, "effect"), 0)
})


test_that("score_role_evidence returns NA when there is too little data", {
  expect_true(is.na(score_role_evidence(stats::rnorm(MIN_ROWS_FOR_EVIDENCE - 1), "effect")))
})


test_that("score_role_evidence gives non-numeric columns zero evidence", {
  labels <- paste("Study", LETTERS[1:20])
  expect_equal(score_role_evidence(labels, "effect"), 0)
  expect_equal(score_role_evidence(labels, "se"), 0)
})


test_that("score_role_evidence penalizes negative values for se", {
  withr::local_seed(42)
  real_se <- stats::runif(100, 0.05, 0.4)
  signed <- stats::rnorm(100, 0, 0.4)

  expect_gte(score_role_evidence(real_se, "se"), 0.9)
  expect_lte(score_role_evidence(signed, "se"), 0.3)
})


test_that("score_role_evidence rates plausible t-statistics high", {
  withr::local_seed(42)
  t_values <- stats::rnorm(100, 0.5, 2.5)
  huge <- stats::rnorm(100, 0, 5000)

  expect_gte(score_role_evidence(t_values, "t_stat"), 0.9)
  expect_lte(score_role_evidence(huge, "t_stat"), 0.5)
})


test_that("score_role_evidence rates repeated positive integers high for n_obs", {
  withr::local_seed(42)
  n_obs <- rep(sample(80:2000, 20), each = 5)

  expect_gte(score_role_evidence(n_obs, "n_obs"), 0.85)
  expect_equal(score_role_evidence(1:100, "n_obs"), 0)
})


# Tests for score_pair_consistency
test_that("score_pair_consistency rates a real (effect, se) pair high", {
  withr::local_seed(42)
  effect <- stats::rnorm(100, 0.3, 0.4)
  se <- stats::runif(100, 0.1, 0.5)

  expect_gte(score_pair_consistency(effect, se), 0.75)
})


test_that("score_pair_consistency rejects sample sizes posing as se", {
  withr::local_seed(42)
  effect <- stats::rnorm(100, 0.3, 0.4)
  n_obs <- sample(100:5000, 100, replace = TRUE)

  expect_lte(score_pair_consistency(effect, n_obs), 0.25)
})


test_that("score_pair_consistency rejects se columns with negative values", {
  withr::local_seed(42)
  effect <- stats::rnorm(100, 0.3, 0.4)
  signed <- stats::rnorm(100, 0, 0.4)

  expect_lte(score_pair_consistency(effect, signed), 0.25)
})


test_that("score_pair_consistency returns NA on insufficient complete cases", {
  expect_true(is.na(score_pair_consistency(stats::rnorm(5), stats::runif(5))))
})


# Tests for score_triple_consistency
test_that("score_triple_consistency detects exact and rounded ratios", {
  withr::local_seed(42)
  effect <- round(stats::rnorm(100, 0.3, 0.4), 3)
  se <- round(stats::runif(100, 0.1, 0.5), 3)
  t_exact <- effect / se
  t_rounded <- round(effect / se, 2)

  expect_gte(score_triple_consistency(effect, se, t_exact), 0.99)
  expect_gte(score_triple_consistency(effect, se, t_rounded), 0.9)
})


test_that("score_triple_consistency recognizes absolute t-statistics at a discount", {
  withr::local_seed(42)
  effect <- stats::rnorm(100, 0, 0.4)
  se <- stats::runif(100, 0.1, 0.5)
  t_abs <- abs(effect / se)

  score <- score_triple_consistency(effect, se, t_abs)
  expect_gte(score, 0.8)
  expect_lte(score, 0.9)
})


test_that("score_triple_consistency rejects unrelated columns", {
  withr::local_seed(42)
  effect <- stats::rnorm(100, 0.3, 0.4)
  se <- stats::runif(100, 0.1, 0.5)
  unrelated <- stats::rnorm(100, 2, 1)

  expect_lte(score_triple_consistency(effect, se, unrelated), 0.2)
})


# Tests for assign_core_roles
test_that("assign_core_roles lets a consistent triple beat a name-only match", {
  withr::local_seed(42)
  n <- 120
  effect <- round(stats::rnorm(n, 0.3, 0.5), 3)
  se <- round(stats::runif(n, 0.05, 0.4), 3)
  df <- data.frame(
    idcoeff = rep(1:6, times = 20),
    e = effect,
    se = se,
    t = round(effect / se, 4)
  )

  # Simulate the original failure: the counter's name scores highly for effect
  name_scores <- matrix(
    0,
    nrow = 3,
    ncol = 4,
    dimnames = list(c("effect", "se", "t_stat"), names(df))
  )
  name_scores["effect", "idcoeff"] <- 0.8
  name_scores["effect", "e"] <- 1.0
  name_scores["se", "se"] <- 1.0
  name_scores["t_stat", "t"] <- 1.0

  result <- assign_core_roles(df, name_scores, 0.7, 0.95)

  expect_equal(result$effect$column, "e")
  expect_equal(result$se$column, "se")
  expect_equal(result$t_stat$column, "t")
})


test_that("assign_core_roles declines an identifier even when the name matches", {
  withr::local_seed(42)
  df <- data.frame(
    coef = rep(1:6, times = 20),
    se = round(stats::runif(120, 0.05, 0.4), 3)
  )

  name_scores <- matrix(
    0,
    nrow = 3,
    ncol = 2,
    dimnames = list(c("effect", "se", "t_stat"), names(df))
  )
  name_scores["effect", "coef"] <- 1.0
  name_scores["se", "se"] <- 1.0

  result <- assign_core_roles(df, name_scores, 0.7, 0.95)

  expect_false("effect" %in% names(result))
  expect_equal(result$se$column, "se")
})


test_that("assign_core_roles maps name-free columns given conclusive data evidence", {
  withr::local_seed(42)
  n <- 150
  effect <- round(stats::rnorm(n, 0.1, 0.6), 3)
  se <- round(stats::runif(n, 0.05, 0.5), 3)
  df <- data.frame(
    x = effect,
    y = se,
    z = round(effect / se, 4)
  )

  name_scores <- matrix(
    0,
    nrow = 3,
    ncol = 3,
    dimnames = list(c("effect", "se", "t_stat"), names(df))
  )

  result <- assign_core_roles(df, name_scores, 0.7, 0.95)

  expect_equal(result$effect$column, "x")
  expect_equal(result$se$column, "y")
})


test_that("assign_core_roles returns nothing for tiny data frames", {
  df <- data.frame(effect = stats::rnorm(5), se = stats::runif(5))
  name_scores <- matrix(
    1,
    nrow = 3,
    ncol = 2,
    dimnames = list(c("effect", "se", "t_stat"), names(df))
  )

  expect_equal(assign_core_roles(df, name_scores, 0.7, 0.95), list())
})
