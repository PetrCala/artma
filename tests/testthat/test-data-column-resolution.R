box::use(
  artma / data / column_recognition[
    analyze_column_values,
    score_candidate_values,
    resolve_multiple_matches,
    recognize_columns,
    match_column_name,
    get_column_patterns
  ]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_false <- getFromNamespace("expect_false", "testthat")
expect_gte <- getFromNamespace("expect_gte", "testthat")
expect_lte <- getFromNamespace("expect_lte", "testthat")
expect_lt <- getFromNamespace("expect_lt", "testthat")


# Tests for analyze_column_values
test_that("analyze_column_values detects sequential integer patterns", {
  values <- 1:10
  analysis <- analyze_column_values(values)

  expect_true(analysis$is_sequential)
  expect_true(analysis$is_unique)
  expect_true(analysis$is_numeric)
  expect_equal(analysis$uniqueness_ratio, 1.0)
})


test_that("analyze_column_values detects non-sequential patterns", {
  values <- c(100, 150, 200, 120, 180)
  analysis <- analyze_column_values(values)

  expect_false(analysis$is_sequential)
  expect_true(analysis$is_numeric)
})


test_that("analyze_column_values handles repeated values", {
  values <- c(100, 100, 150, 150, 200, 200)
  analysis <- analyze_column_values(values)

  expect_false(analysis$is_unique)
  expect_equal(analysis$uniqueness_ratio, 0.5)
})


test_that("analyze_column_values computes statistical properties", {
  values <- c(100, 200, 300, 400, 500)
  analysis <- analyze_column_values(values)

  expect_equal(analysis$mean, 300)
  expect_equal(analysis$min, 100)
  expect_equal(analysis$max, 500)
  expect_true(analysis$variance > 0)
})


test_that("analyze_column_values handles NA values", {
  values <- c(100, NA, 200, NA, 300)
  analysis <- analyze_column_values(values)

  expect_equal(analysis$mean, 200)
  expect_equal(analysis$min, 100)
  expect_equal(analysis$max, 300)
})


test_that("analyze_column_values handles all-NA columns", {
  values <- c(NA, NA, NA)
  analysis <- analyze_column_values(values)

  expect_false(analysis$is_sequential)
  expect_false(analysis$is_unique)
  expect_equal(analysis$uniqueness_ratio, 0)
  expect_true(is.na(analysis$mean))
})


test_that("analyze_column_values handles character columns", {
  values <- c("Study A", "Study B", "Study C")
  analysis <- analyze_column_values(values)

  expect_false(analysis$is_numeric)
  expect_true(analysis$is_unique)
})


test_that("analyze_column_values detects sequential pattern starting from non-1", {
  values <- 5:14
  analysis <- analyze_column_values(values)

  # Should NOT be detected as sequential since we check for diff == 1
  expect_true(analysis$is_sequential)
})


# Tests for score_candidate_values
test_that("score_candidate_values penalizes sequential pattern for n_obs", {
  df <- data.frame(
    obs_n = 1:10,
    n_obs = c(100, 150, 200, 120, 180, 90, 110, 130, 160, 140)
  )

  # Sequential column should get penalty
  score_seq <- score_candidate_values(df, "obs_n", "n_obs", 1.0)

  # Non-sequential column should not get penalty
  score_nonseq <- score_candidate_values(df, "n_obs", "n_obs", 1.0)

  expect_lt(score_seq, score_nonseq)
  expect_lt(score_seq, 0.8) # Should have significant penalty
})


test_that("score_candidate_values penalizes high uniqueness for n_obs", {
  df <- data.frame(
    col_unique = 1:100, # All unique, like an ID
    col_data = sample(c(50, 100, 150, 200, 250), 100, replace = TRUE) # Repeated values
  )

  score_unique <- score_candidate_values(df, "col_unique", "n_obs", 1.0)
  score_repeated <- score_candidate_values(df, "col_data", "n_obs", 1.0)

  expect_lt(score_unique, score_repeated)
})


test_that("score_candidate_values rewards sequential pattern for obs_id", {
  df <- data.frame(
    obs_n = 1:10,
    other = c(100, 150, 200, 120, 180, 90, 110, 130, 160, 140)
  )

  # Sequential column should get bonus
  score_seq <- score_candidate_values(df, "obs_n", "obs_id", 1.0)

  # Non-sequential column should not get bonus
  score_nonseq <- score_candidate_values(df, "other", "obs_id", 1.0)

  expect_gte(score_seq, score_nonseq)
})


test_that("score_candidate_values penalizes non-unique values for obs_id", {
  df <- data.frame(
    col_unique = 1:10,
    col_repeated = rep(1:5, each = 2)
  )

  score_unique <- score_candidate_values(df, "col_unique", "obs_id", 1.0)
  score_repeated <- score_candidate_values(df, "col_repeated", "obs_id", 1.0)

  expect_gte(score_unique, score_repeated)
})


test_that("score_candidate_values handles effect columns appropriately", {
  df <- data.frame(
    effect_real = rnorm(100, mean = 0.5, sd = 0.2),
    effect_seq = 1:100
  )

  score_real <- score_candidate_values(df, "effect_real", "effect", 1.0)
  score_seq <- score_candidate_values(df, "effect_seq", "effect", 1.0)

  expect_gte(score_real, score_seq)
})


test_that("score_candidate_values penalizes zero variance columns", {
  df <- data.frame(
    col_constant = rep(100, 10),
    col_varying = c(100, 150, 200, 120, 180, 90, 110, 130, 160, 140)
  )

  score_constant <- score_candidate_values(df, "col_constant", "effect", 1.0)
  score_varying <- score_candidate_values(df, "col_varying", "effect", 1.0)

  expect_lt(score_constant, score_varying)
})


# Tests for resolve_multiple_matches
test_that("resolve_multiple_matches picks non-sequential for n_obs", {
  df <- data.frame(
    obs_n = 1:5,
    n_obs = c(100, 150, 200, 120, 180),
    study_id = paste("Study", LETTERS[1:5]),
    effect = rnorm(5),
    se = runif(5, 1, 3)
  )

  patterns <- get_column_patterns()
  matches <- list(
    obs_n = list(match = "n_obs", score = 1.0, method = "regex"),
    n_obs = list(match = "n_obs", score = 1.0, method = "regex")
  )

  withr::local_options(list("artma.verbose" = 1))
  best <- resolve_multiple_matches(df, c("obs_n", "n_obs"), "n_obs", matches)

  # Should pick n_obs over obs_n because obs_n is sequential
  expect_equal(best, "n_obs")
})


test_that("resolve_multiple_matches picks sequential for obs_id", {
  df <- data.frame(
    obs_n = 1:5,
    other_id = c(101, 102, 105, 103, 104),
    study_id = paste("Study", LETTERS[1:5]),
    effect = rnorm(5),
    se = runif(5, 1, 3)
  )

  patterns <- get_column_patterns()
  matches <- list(
    obs_n = list(match = "obs_id", score = 1.0, method = "regex"),
    other_id = list(match = "obs_id", score = 0.9, method = "keyword")
  )

  withr::local_options(list("artma.verbose" = 1))
  best <- resolve_multiple_matches(df, c("obs_n", "other_id"), "obs_id", matches)

  # Should pick obs_n because it's sequential and unique
  expect_equal(best, "obs_n")
})


test_that("resolve_multiple_matches returns single candidate unchanged", {
  df <- data.frame(n_obs = c(100, 150, 200))
  matches <- list(n_obs = list(match = "n_obs", score = 1.0, method = "regex"))

  withr::local_options(list("artma.verbose" = 1))
  best <- resolve_multiple_matches(df, "n_obs", "n_obs", matches)

  expect_equal(best, "n_obs")
})


# Integration tests with recognize_columns
test_that("recognize_columns correctly resolves obs_n vs n_obs conflict", {
  # This is the main test case from the issue
  df <- data.frame(
    obs_n = 1:10,
    study_id = 1:10,
    study_name = paste("Study", LETTERS[1:10]),
    effect = rnorm(10, 10, 2),
    se = runif(10, 1, 3),
    t_stat = rnorm(10, 3, 1),
    n_obs = sample(100:500, 10),
    reg_df = sample(50:200, 10),
    study_size = rep(1, 10)
  )

  withr::local_options(list("artma.verbose" = 1))
  mapping <- recognize_columns(df, min_confidence = 0.7)

  # Should map n_obs standard column to n_obs data column, not obs_n
  expect_equal(mapping$n_obs, "n_obs")

  # obs_n may or may not be mapped to obs_id depending on what's already used
  # The key test is that n_obs is correctly identified
  if ("obs_id" %in% names(mapping)) {
    # If obs_id is mapped, it should be a sequential column
    obs_id_col <- mapping$obs_id
    analysis <- analyze_column_values(df[[obs_id_col]])
    expect_true(analysis$is_sequential || analysis$is_unique)
  }
})


test_that("recognize_columns handles case with only sequential column", {
  # Edge case: only obs_n exists, no n_obs
  df <- data.frame(
    obs_n = 1:10,
    study_name = paste("Study", LETTERS[1:10]),
    effect = rnorm(10, 10, 2),
    se = runif(10, 1, 3)
  )

  withr::local_options(list("artma.verbose" = 1))
  mapping <- recognize_columns(df, min_confidence = 0.7)

  # Should still map to n_obs despite being sequential (it's the only candidate)
  expect_equal(mapping$n_obs, "obs_n")
})


test_that("recognize_columns handles case with multiple effect-like columns", {
  df <- data.frame(
    study_id = paste("Study", LETTERS[1:10]),
    effect_id = 1:10, # Sequential, should be rejected
    effect = rnorm(10, 0.5, 0.2), # Real effect sizes
    se = runif(10, 0.1, 0.3),
    n_obs = sample(50:200, 10)
  )

  withr::local_options(list("artma.verbose" = 1))
  mapping <- recognize_columns(df, min_confidence = 0.7)

  # Should pick the non-sequential effect column
  expect_equal(mapping$effect, "effect")
})


test_that("recognize_columns prioritizes better semantic match", {
  df <- data.frame(
    row_number = 1:10,
    sample_size = sample(100:500, 10),
    study_id = paste("Study", LETTERS[1:10]),
    estimate = rnorm(10, 0.5, 0.2),
    se = runif(10, 0.1, 0.3)
  )

  withr::local_options(list("artma.verbose" = 1))
  mapping <- recognize_columns(df, min_confidence = 0.7)

  # Should prefer sample_size for n_obs even if row_number matches the pattern
  expect_equal(mapping$n_obs, "sample_size")
})


test_that("value-based resolution works with realistic meta-analysis data", {
  # Simulate realistic data where column names are ambiguous
  df <- data.frame(
    obs_n = 1:50,
    study_id = rep(1:10, each = 5),
    study_name = rep(paste("Study", LETTERS[1:10]), each = 5),
    effect = rnorm(50, 0.3, 0.15),
    se = runif(50, 0.05, 0.2),
    n_obs = sample(50:300, 50, replace = TRUE),
    precision = 1 / runif(50, 0.05, 0.2)^2
  )

  withr::local_options(list("artma.verbose" = 1))
  mapping <- recognize_columns(df, min_confidence = 0.7)

  # Verify correct mappings; study_id maps to one of the study columns
  expect_true(mapping$study_id %in% c("study_id", "study_name"))
  expect_equal(mapping$effect, "effect")
  expect_equal(mapping$se, "se")
  expect_equal(mapping$n_obs, "n_obs") # Not obs_n
})


test_that("value-based resolution handles edge case with same scores", {
  # Create a case where both columns have very similar properties
  set.seed(42)
  df <- data.frame(
    n_obs_1 = sample(50:200, 20),
    n_obs_2 = sample(50:200, 20),
    study_id = paste("Study", 1:20),
    effect = rnorm(20),
    se = runif(20, 0.1, 0.3)
  )

  # Both n_obs_1 and n_obs_2 match the n_obs pattern
  withr::local_options(list("artma.verbose" = 1))
  mapping <- recognize_columns(df, min_confidence = 0.7)

  # Should pick one of them (whichever has slightly better properties)
  expect_true(mapping$n_obs %in% c("n_obs_1", "n_obs_2"))
})


test_that("analyze_column_values handles empty data frame columns", {
  values <- numeric(0)
  analysis <- analyze_column_values(values)

  expect_false(analysis$is_sequential)
  expect_false(analysis$is_unique)
  expect_equal(analysis$uniqueness_ratio, 0)
})


test_that("score_candidate_values handles columns with NA values", {
  df <- data.frame(
    col_with_na = c(100, NA, 200, NA, 300),
    n_obs = c(150, 160, 170, 180, 190)
  )

  # Should not crash on NA values
  score <- score_candidate_values(df, "col_with_na", "n_obs", 1.0)

  expect_true(is.numeric(score))
  expect_gte(score, 0)
  expect_lte(score, 1)
})


test_that("value-based resolution prefers exact name match when values are similar", {
  df <- data.frame(
    observations = sample(50:200, 20),
    n_obs = sample(50:200, 20),
    study_id = paste("Study", 1:20),
    effect = rnorm(20),
    se = runif(20, 0.1, 0.3)
  )

  withr::local_options(list("artma.verbose" = 1))
  mapping <- recognize_columns(df, min_confidence = 0.7)

  # When both have similar value properties, exact name match should win
  expect_equal(mapping$n_obs, "n_obs")
})
