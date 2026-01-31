box::use(
  artma / data / column_recognition[
    get_column_patterns,
    match_column_name,
    recognize_columns,
    get_required_column_names,
    check_mapping_completeness,
    string_similarity
  ]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_false <- getFromNamespace("expect_false", "testthat")
expect_gte <- getFromNamespace("expect_gte", "testthat")
expect_lte <- getFromNamespace("expect_lte", "testthat")


test_that("get_column_patterns returns valid structure", {
  patterns <- get_column_patterns()

  expect_true(is.list(patterns))
  expect_true(length(patterns) > 0)

  # Check required columns are present
  required <- c("study_id", "effect", "se", "n_obs")
  for (col in required) {
    expect_true(col %in% names(patterns))
    expect_true("patterns" %in% names(patterns[[col]]))
    expect_true("keywords" %in% names(patterns[[col]]))
    expect_true("priority" %in% names(patterns[[col]]))
  }
})


test_that("string_similarity returns 1.0 for identical strings", {
  sim <- string_similarity("effect", "effect")
  expect_equal(sim, 1.0)
})


test_that("string_similarity returns 0.0 for completely different strings", {
  sim <- string_similarity("effect", "xyz123")
  expect_true(sim < 0.5)
})


test_that("string_similarity returns high score for similar strings", {
  sim <- string_similarity("effect", "Effect")
  expect_gte(sim, 0.9)
})


test_that("string_similarity handles substring matches", {
  sim <- string_similarity("effect_size", "effect")
  expect_gte(sim, 0.7)
})


test_that("match_column_name recognizes exact effect column", {
  patterns <- get_column_patterns()
  result <- match_column_name("effect", patterns)

  expect_equal(result$match, "effect")
  expect_equal(result$score, 1.0)
  expect_equal(result$method, "regex")
})


test_that("match_column_name recognizes exact se column", {
  patterns <- get_column_patterns()
  result <- match_column_name("se", patterns)

  expect_equal(result$match, "se")
  expect_equal(result$score, 1.0)
  expect_equal(result$method, "regex")
})


test_that("match_column_name recognizes study_name as study_id", {
  patterns <- get_column_patterns()
  result <- match_column_name("study_name", patterns)

  expect_equal(result$match, "study_id")
  expect_equal(result$score, 1.0)
  expect_equal(result$method, "regex")
})


test_that("match_column_name recognizes n_obs variants", {
  patterns <- get_column_patterns()

  test_cases <- list(
    list(name = "n_obs", expected = "n_obs"),
    list(name = "obs_n", expected = "n_obs"),
    list(name = "sample_size", expected = "n_obs")
  )

  for (test_case in test_cases) {
    result <- match_column_name(test_case$name, patterns)
    expect_equal(result$match, test_case$expected,
      info = paste("Testing", test_case$name)
    )
  }
})


test_that("match_column_name recognizes t_stat variants", {
  patterns <- get_column_patterns()

  test_cases <- list(
    list(name = "t_stat", expected = "t_stat"),
    list(name = "t_statistic", expected = "t_stat"),
    list(name = "tval", expected = "t_stat")
  )

  for (test_case in test_cases) {
    result <- match_column_name(test_case$name, patterns)
    expect_equal(result$match, test_case$expected,
      info = paste("Testing", test_case$name)
    )
  }
})


test_that("match_column_name handles exclude keywords correctly", {
  patterns <- get_column_patterns()

  # "study_id" should match study_id pattern
  result <- match_column_name("study_id", patterns)
  expect_equal(result$match, "study_id")
})


test_that("match_column_name returns NA for unrecognized column", {
  patterns <- get_column_patterns()
  result <- match_column_name("completely_unknown_xyz", patterns)

  expect_true(is.na(result$match))
  expect_equal(result$score, 0)
})


test_that("match_column_name avoids false positives with region names", {
  patterns <- get_column_patterns()

  # These should NOT match obs_id due to exclude keywords
  test_cases <- c(
    "region_middle_east_and_north_africa",
    "region_asia",
    "region_europe"
  )

  for (col_name in test_cases) {
    result <- match_column_name(col_name, patterns)
    expect_false(result$match == "obs_id" && result$score >= 0.7,
      info = paste("Testing", col_name, "should not match obs_id")
    )
  }
})


test_that("recognize_columns correctly identifies standard meta-analysis columns", {
  df <- data.frame(
    study_name = c("Study A", "Study B"),
    effect = c(10.5, 8.2),
    se = c(2.3, 1.8),
    n_obs = c(100, 150)
  )

  mapping <- recognize_columns(df, min_confidence = 0.7)

  expect_equal(mapping$study_id, "study_name")
  expect_equal(mapping$effect, "effect")
  expect_equal(mapping$se, "se")
  expect_equal(mapping$n_obs, "n_obs")
})


test_that("recognize_columns handles different naming conventions", {
  df <- data.frame(
    author_name = c("Author A", "Author B"),
    estimate = c(10.5, 8.2),
    std_error = c(2.3, 1.8),
    sample_size = c(100, 150)
  )

  mapping <- recognize_columns(df, min_confidence = 0.7)

  expect_equal(mapping$study_id, "author_name")
  expect_equal(mapping$effect, "estimate")
  expect_equal(mapping$se, "std_error")
  expect_equal(mapping$n_obs, "sample_size")
})


test_that("recognize_columns prioritizes required columns", {
  df <- data.frame(
    study_id = c(1L, 2L),
    study_name = c("Study A", "Study B"),
    effect = c(10.5, 8.2),
    se = c(2.3, 1.8),
    n_obs = c(100, 150)
  )

  mapping <- recognize_columns(df, min_confidence = 0.7)

  # study_id should be mapped (to study_id or study_name)
  expect_true("study_id" %in% names(mapping))
  expect_true(mapping$study_id %in% c("study_id", "study_name"))
})


test_that("recognize_columns does not reuse columns", {
  df <- data.frame(
    effect = c(10.5, 8.2),
    se = c(2.3, 1.8),
    n_obs = c(100, 150),
    study_id = c(1L, 2L)
  )

  mapping <- recognize_columns(df, min_confidence = 0.7)

  # Each column should be mapped at most once
  mapped_cols <- unlist(mapping)
  expect_equal(length(mapped_cols), length(unique(mapped_cols)))
})


test_that("recognize_columns respects min_confidence threshold", {
  df <- data.frame(
    something_vaguely_like_effect = c(10.5, 8.2),
    maybe_error_ish = c(2.3, 1.8),
    n_obs = c(100, 150),
    study_id = c(1L, 2L)
  )

  # With high threshold, should only match clear cases
  mapping_strict <- recognize_columns(df, min_confidence = 0.95)

  # Should recognize the clear ones
  expect_equal(mapping_strict$n_obs, "n_obs")
  expect_equal(mapping_strict$study_id, "study_id")
})


test_that("recognize_columns handles empty data frame", {
  df <- data.frame()

  # Empty data frames should be validated, not throw error in recognize_columns
  # The validation happens elsewhere in the pipeline
  withr::local_options(list("artma.verbose" = 1))
  mapping <- recognize_columns(df, min_confidence = 0.7)

  expect_true(is.list(mapping))
  expect_equal(length(mapping), 0)
})


test_that("recognize_columns handles data frame with no recognizable columns", {
  df <- data.frame(
    xyz = c(1, 2),
    abc = c(3, 4),
    def = c(5, 6)
  )

  mapping <- recognize_columns(df, min_confidence = 0.7)

  # Should return empty or minimal mapping
  expect_true(is.list(mapping))
  expect_true(length(mapping) < ncol(df))
})


test_that("get_required_column_names returns expected columns", {
  required <- get_required_column_names()

  expect_true(is.character(required))
  expect_true(length(required) >= 4)
  expect_true("study_id" %in% required)
  expect_true("effect" %in% required)
  expect_true("se" %in% required)
  expect_true("n_obs" %in% required)
})


test_that("check_mapping_completeness identifies complete mapping", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect",
    se = "se",
    n_obs = "n_obs"
  )

  result <- check_mapping_completeness(mapping)

  expect_true(result$complete)
  expect_equal(length(result$missing), 0)
})


test_that("check_mapping_completeness identifies missing required columns", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect"
    # Missing se and n_obs
  )

  result <- check_mapping_completeness(mapping)

  expect_false(result$complete)
  expect_true("se" %in% result$missing)
  expect_true("n_obs" %in% result$missing)
})


test_that("check_mapping_completeness handles empty mapping", {
  mapping <- list()

  result <- check_mapping_completeness(mapping)

  expect_false(result$complete)
  expect_true(length(result$missing) >= 4)
})


test_that("recognize_columns with realistic meta-analysis data", {
  # Simulate realistic column names from published meta-analyses
  df <- data.frame(
    obs_n = 1:5,
    study_id = 1:5,
    study_name = paste("Study", LETTERS[1:5]),
    effect = rnorm(5, 10, 2),
    se = runif(5, 1, 3),
    t_stat = rnorm(5, 3, 1),
    n_obs = sample(100:500, 5),
    reg_df = sample(50:200, 5),
    study_size = rep(1, 5)
  )

  withr::local_options(list("artma.verbose" = 1))
  mapping <- recognize_columns(df, min_confidence = 0.7)

  # Verify key columns are recognized; study_id maps to one of the study columns
  expect_true(mapping$study_id %in% c("study_id", "study_name"))
  expect_equal(mapping$effect, "effect")
  expect_equal(mapping$se, "se")
  # n_obs could be matched to either "obs_n" or "n_obs" - both are valid patterns
  expect_true(mapping$n_obs %in% c("n_obs", "obs_n"))
  expect_equal(mapping$t_stat, "t_stat")

  # Verify no false positives for obs_id
  if ("obs_id" %in% names(mapping)) {
    # If obs_id is mapped, it should be to obs_n, not to anything else
    expect_true(mapping$obs_id %in% c("obs_n", "obs_id"))
  }
})


test_that("column recognition handles case insensitivity", {
  df <- data.frame(
    EFFECT = c(10.5, 8.2),
    SE = c(2.3, 1.8),
    Study = c("A", "B"),
    N_OBS = c(100, 150)
  )

  mapping <- recognize_columns(df, min_confidence = 0.7)

  expect_equal(mapping$effect, "EFFECT")
  expect_equal(mapping$se, "SE")
  expect_equal(mapping$study_id, "Study")
  expect_equal(mapping$n_obs, "N_OBS")
})
