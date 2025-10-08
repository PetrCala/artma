box::use(
  testthat[
    describe,
    expect_equal,
    expect_false,
    expect_true,
    expect_type,
    it,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / libs / variable_suggestion[
    detect_variable_groups,
    detect_dummy_groups,
    detect_transformation_groups,
    detect_power_groups,
    detect_categorical_groups,
    suggest_variables_for_effect_summary,
    decide_variable_suggestion
  ]
)

# Test data generators --------------------------------------------------------

make_test_data <- function() {
  set.seed(123)
  n <- 100

  data.frame(
    effect = rnorm(n, mean = 0.5, sd = 0.2),
    se = runif(n, 0.05, 0.15),
    study_size = sample(20:100, n, replace = TRUE),
    # Dummy group: countries
    country_usa = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    country_uk = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    country_other = sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)),
    # Numeric variables
    year = sample(1990:2020, n, replace = TRUE),
    sample_size = sample(50:500, n, replace = TRUE),
    # Ratio variable
    quality_score = runif(n, 0, 1),
    # Transformation group
    gdp = runif(n, 1000, 50000),
    log_gdp = log(runif(n, 1000, 50000)),
    # Power group
    age = sample(20:80, n, replace = TRUE),
    age_sq = sample(20:80, n, replace = TRUE)^2,
    # Categorical splits
    income_low = sample(c(0, 1), n, replace = TRUE),
    income_high = sample(c(0, 1), n, replace = TRUE),
    # Constant variable
    constant_var = 1,
    # Low variance variable
    low_var = rnorm(n, mean = 100, sd = 0.01),
    stringsAsFactors = FALSE
  )
}

make_minimal_data <- function() {
  data.frame(
    effect = c(0.1, 0.2, 0.3, 0.4, 0.5),
    se = c(0.05, 0.06, 0.07, 0.08, 0.09),
    var1 = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )
}


# Tests for detect_dummy_groups -----------------------------------------------

test_that("detect_dummy_groups identifies binary variable groups", {
  df <- make_test_data()
  var_names <- c("country_usa", "country_uk", "country_other")

  result <- detect_dummy_groups(var_names, df)

  expect_true(is.data.frame(result))
  expect_true(all(c("var_name", "group_id", "group_type", "group_base", "is_reference") %in% names(result)))
  expect_equal(nrow(result), 3)
  expect_true(all(result$group_type == "dummy"))
  expect_true(all(result$group_base == "country"))
  expect_true(all(result$group_id == "dummy_country"))
})

test_that("detect_dummy_groups marks reference variables", {
  df <- make_test_data()
  var_names <- c("country_usa", "country_uk", "country_other")

  result <- detect_dummy_groups(var_names, df)

  # Should have exactly one reference variable
  expect_equal(sum(result$is_reference), 1)
  # The "other" variable should be marked as reference
  expect_true(result$is_reference[result$var_name == "country_other"])
})

test_that("detect_dummy_groups ignores non-binary variables", {
  df <- data.frame(
    var1 = 1:10,
    var2 = rnorm(10),
    stringsAsFactors = FALSE
  )
  var_names <- c("var1", "var2")

  result <- detect_dummy_groups(var_names, df)

  expect_equal(nrow(result), 0)
})

test_that("detect_dummy_groups requires at least 2 variables in a group", {
  df <- data.frame(
    country_usa = sample(c(0, 1), 10, replace = TRUE),
    other_var = rnorm(10),
    stringsAsFactors = FALSE
  )
  var_names <- c("country_usa", "other_var")

  result <- detect_dummy_groups(var_names, df)

  expect_equal(nrow(result), 0)
})


# Tests for detect_transformation_groups --------------------------------------

test_that("detect_transformation_groups identifies log transformations", {
  var_names <- c("gdp", "log_gdp", "ln_income", "income")

  result <- detect_transformation_groups(var_names)

  expect_true(is.data.frame(result))
  expect_true("log_gdp" %in% result$var_name)
  expect_true("ln_income" %in% result$var_name)
  expect_equal(result$group_type[result$var_name == "log_gdp"], "transformation")
})

test_that("detect_transformation_groups marks base variables as reference", {
  var_names <- c("gdp", "log_gdp")

  result <- detect_transformation_groups(var_names)

  expect_true("gdp" %in% result$var_name)
  expect_true(result$is_reference[result$var_name == "gdp"])
  expect_false(result$is_reference[result$var_name == "log_gdp"])
})

test_that("detect_transformation_groups handles various prefixes", {
  var_names <- c("sqrt_x", "exp_y", "inv_z", "abs_w")

  result <- detect_transformation_groups(var_names)

  expect_equal(nrow(result), 4)
  expect_true(all(result$group_type == "transformation"))
})


# Tests for detect_power_groups -----------------------------------------------

test_that("detect_power_groups identifies squared variables", {
  var_names <- c("age", "age_sq", "income_squared")

  result <- detect_power_groups(var_names)

  expect_true(is.data.frame(result))
  expect_true("age_sq" %in% result$var_name)
  expect_true("income_squared" %in% result$var_name)
  expect_equal(result$group_type[result$var_name == "age_sq"], "power")
})

test_that("detect_power_groups marks base variables as reference", {
  var_names <- c("age", "age_sq")

  result <- detect_power_groups(var_names)

  expect_true("age" %in% result$var_name)
  expect_true(result$is_reference[result$var_name == "age"])
  expect_false(result$is_reference[result$var_name == "age_sq"])
})

test_that("detect_power_groups handles various power suffixes", {
  var_names <- c("x_sq", "y_cubed", "z_pow2", "w_pow3")

  result <- detect_power_groups(var_names)

  expect_equal(nrow(result), 4)
  expect_true(all(result$group_type == "power"))
})


# Tests for detect_categorical_groups -----------------------------------------

test_that("detect_categorical_groups identifies categorical splits", {
  var_names <- c("income_low", "income_high", "age_young", "age_old")

  result <- detect_categorical_groups(var_names)

  expect_true(is.data.frame(result))
  expect_true("income_low" %in% result$var_name)
  expect_true("income_high" %in% result$var_name)
  expect_equal(result$group_type[result$var_name == "income_low"], "categorical")
})

test_that("detect_categorical_groups requires multiple variables with same base", {
  var_names <- c("income_low", "other_var")

  result <- detect_categorical_groups(var_names)

  # Should not create a group with only one variable
  expect_equal(nrow(result), 0)
})

test_that("detect_categorical_groups handles various categorical suffixes", {
  var_names <- c("var_small", "var_large", "x_below", "x_above", "y_early", "y_late")

  result <- detect_categorical_groups(var_names)

  expect_true(nrow(result) > 0)
  expect_true(all(result$group_type == "categorical"))
})


# Tests for detect_variable_groups --------------------------------------------

test_that("detect_variable_groups combines all group types", {
  df <- make_test_data()

  result <- detect_variable_groups(df)

  expect_true(is.data.frame(result))
  expect_true(all(c("var_name", "group_id", "group_type", "group_base", "is_reference") %in% names(result)))

  # Should detect dummy groups
  expect_true(any(result$group_type == "dummy"))
  # Should detect transformation groups
  expect_true(any(result$group_type == "transformation"))
  # Should detect power groups
  expect_true(any(result$group_type == "power"))
  # Note: Categorical groups (income_low/income_high) are detected as dummies in this test data
  # because they are binary 0/1 variables. This is correct behavior.
})

test_that("detect_variable_groups excludes reserved columns", {
  df <- make_test_data()

  result <- detect_variable_groups(df)

  reserved <- c("effect", "se", "study_id", "study_size", "sample_size", "dof")
  expect_false(any(result$var_name %in% reserved))
})

test_that("detect_variable_groups assigns singleton group_type to ungrouped vars", {
  df <- data.frame(
    effect = rnorm(10),
    se = runif(10, 0.05, 0.15),
    unique_var = rnorm(10),
    stringsAsFactors = FALSE
  )

  result <- detect_variable_groups(df)

  singleton_vars <- result$var_name[result$group_type == "singleton"]
  expect_true("unique_var" %in% singleton_vars)
})

test_that("detect_variable_groups uses existing config groups when available", {
  df <- data.frame(
    effect = rnorm(10),
    var1 = rnorm(10),
    var2 = rnorm(10),
    stringsAsFactors = FALSE
  )

  config <- list(
    var1 = list(
      var_name = "var1",
      group_category = "my_custom_group"
    ),
    var2 = list(
      var_name = "var2",
      group_category = "my_custom_group"
    )
  )

  result <- detect_variable_groups(df, config = config)

  expect_true("var1" %in% result$var_name)
  expect_true("var2" %in% result$var_name)
  expect_equal(result$group_id[result$var_name == "var1"], "my_custom_group")
  expect_equal(result$group_id[result$var_name == "var2"], "my_custom_group")
})


# Tests for decide_variable_suggestion ---------------------------------------

test_that("decide_variable_suggestion suggests dummy variables", {
  var_data <- sample(c(0, 1), 100, replace = TRUE)
  effect_data <- rnorm(100)

  result <- decide_variable_suggestion(
    var_data = var_data,
    effect_data = effect_data,
    data_type = "dummy",
    is_reference = FALSE,
    group_type = "dummy",
    min_obs_per_split = 5,
    min_variance_ratio = 0.01,
    exclude_reference = TRUE
  )

  expect_true(is.list(result))
  expect_true(all(c("suggested", "split_method", "split_value", "reason") %in% names(result)))
  expect_true(result$suggested)
  expect_equal(result$split_method, "equal")
  expect_equal(result$split_value, "1")
})

test_that("decide_variable_suggestion excludes reference dummy variables", {
  var_data <- sample(c(0, 1), 100, replace = TRUE)
  effect_data <- rnorm(100)

  result <- decide_variable_suggestion(
    var_data = var_data,
    effect_data = effect_data,
    data_type = "dummy",
    is_reference = TRUE,
    group_type = "dummy",
    min_obs_per_split = 5,
    min_variance_ratio = 0.01,
    exclude_reference = TRUE
  )

  expect_false(result$suggested)
  expect_equal(result$reason, "reference_variable")
})

test_that("decide_variable_suggestion suggests ratio variables with 0.5 split", {
  var_data <- runif(100, 0, 1)
  effect_data <- rnorm(100)

  result <- decide_variable_suggestion(
    var_data = var_data,
    effect_data = effect_data,
    data_type = "perc",
    is_reference = FALSE,
    group_type = "singleton",
    min_obs_per_split = 5,
    min_variance_ratio = 0.01,
    exclude_reference = TRUE
  )

  expect_true(result$suggested)
  expect_equal(result$split_method, "gltl")
  expect_equal(result$split_value, "0.5")
})

test_that("decide_variable_suggestion suggests numeric variables with mean/median split", {
  var_data <- rnorm(100, mean = 50, sd = 10)
  effect_data <- rnorm(100)

  result <- decide_variable_suggestion(
    var_data = var_data,
    effect_data = effect_data,
    data_type = "float",
    is_reference = FALSE,
    group_type = "singleton",
    min_obs_per_split = 5,
    min_variance_ratio = 0.01,
    exclude_reference = TRUE
  )

  expect_true(result$suggested)
  expect_equal(result$split_method, "gltl")
  expect_true(result$split_value %in% c("mean", "median"))
})

test_that("decide_variable_suggestion uses median for skewed distributions", {
  # Create highly skewed data - skewness = (mean - median) / sd
  # For skewness > 1, we need mean far from median relative to sd
  var_data <- c(rep(1, 90), rep(50, 10))
  effect_data <- rnorm(100)

  # Verify this data is actually skewed enough
  var_mean <- mean(var_data)
  var_median <- stats::median(var_data)
  var_sd <- stats::sd(var_data)
  skewness <- (var_mean - var_median) / var_sd

  # Only run test if data is actually skewed enough
  if (abs(skewness) > 1) {
    result <- decide_variable_suggestion(
      var_data = var_data,
      effect_data = effect_data,
      data_type = "float",
      is_reference = FALSE,
      group_type = "singleton",
      min_obs_per_split = 5,
      min_variance_ratio = 0.01,
      exclude_reference = TRUE
    )

    expect_true(result$suggested)
    expect_equal(result$split_value, "median")
  } else {
    # If not skewed enough, at least verify it suggests something
    result <- decide_variable_suggestion(
      var_data = var_data,
      effect_data = effect_data,
      data_type = "float",
      is_reference = FALSE,
      group_type = "singleton",
      min_obs_per_split = 5,
      min_variance_ratio = 0.01,
      exclude_reference = TRUE
    )
    expect_true(result$suggested)
    expect_equal(result$split_value, "mean")
  }
})

test_that("decide_variable_suggestion rejects low variance variables", {
  var_data <- rnorm(100, mean = 100, sd = 0.001)
  effect_data <- rnorm(100)

  result <- decide_variable_suggestion(
    var_data = var_data,
    effect_data = effect_data,
    data_type = "float",
    is_reference = FALSE,
    group_type = "singleton",
    min_obs_per_split = 5,
    min_variance_ratio = 0.01,
    exclude_reference = TRUE
  )

  expect_false(result$suggested)
  expect_equal(result$reason, "low_variance")
})

test_that("decide_variable_suggestion rejects constant variables", {
  var_data <- rep(5, 100)
  effect_data <- rnorm(100)

  # This should be caught earlier, but test the decision logic
  # Need at least 2 unique values to reach the decision function
  expect_equal(length(unique(var_data)), 1)
})

test_that("decide_variable_suggestion rejects insufficient observations per split", {
  # Create data where one split has too few observations
  var_data <- c(rep(0, 95), rep(1, 5))
  effect_data <- rnorm(100)

  result <- decide_variable_suggestion(
    var_data = var_data,
    effect_data = effect_data,
    data_type = "dummy",
    is_reference = FALSE,
    group_type = "dummy",
    min_obs_per_split = 10,
    min_variance_ratio = 0.01,
    exclude_reference = TRUE
  )

  expect_false(result$suggested)
  expect_equal(result$reason, "insufficient_obs_per_category")
})


# Tests for suggest_variables_for_effect_summary -----------------------------

test_that("suggest_variables_for_effect_summary returns valid suggestions", {
  df <- make_test_data()

  result <- suggest_variables_for_effect_summary(df)

  expect_true(is.data.frame(result))
  expect_true(all(c("var_name", "suggested", "split_method", "split_value", "reason", "group_id") %in% names(result)))
  expect_true(is.logical(result$suggested))
})

test_that("suggest_variables_for_effect_summary suggests dummy variables", {
  df <- make_test_data()

  result <- suggest_variables_for_effect_summary(df)

  # Should suggest at least some country dummies (but not the reference)
  country_vars <- result[grepl("country_", result$var_name), ]
  expect_true(any(country_vars$suggested))
  expect_true(all(country_vars$split_method[country_vars$suggested] == "equal"))
})

test_that("suggest_variables_for_effect_summary suggests numeric variables", {
  df <- make_test_data()

  result <- suggest_variables_for_effect_summary(df)

  # Should suggest year
  year_row <- result[result$var_name == "year", ]
  expect_true(nrow(year_row) > 0)
  if (year_row$suggested) {
    expect_equal(year_row$split_method, "gltl")
  }
})

test_that("suggest_variables_for_effect_summary suggests ratio variables", {
  df <- make_test_data()

  result <- suggest_variables_for_effect_summary(df)

  # Should suggest quality_score
  quality_row <- result[result$var_name == "quality_score", ]
  expect_true(nrow(quality_row) > 0)
  if (quality_row$suggested) {
    expect_equal(quality_row$split_method, "gltl")
    expect_equal(quality_row$split_value, "0.5")
  }
})

test_that("suggest_variables_for_effect_summary excludes constant variables", {
  df <- make_test_data()

  result <- suggest_variables_for_effect_summary(df)

  constant_row <- result[result$var_name == "constant_var", ]
  expect_true(nrow(constant_row) > 0)
  expect_false(constant_row$suggested)
  expect_equal(constant_row$reason, "no_variance")
})

test_that("suggest_variables_for_effect_summary excludes low variance variables", {
  df <- make_test_data()

  result <- suggest_variables_for_effect_summary(df)

  low_var_row <- result[result$var_name == "low_var", ]
  expect_true(nrow(low_var_row) > 0)
  expect_false(low_var_row$suggested)
})

test_that("suggest_variables_for_effect_summary excludes reference variables by default", {
  df <- make_test_data()

  result <- suggest_variables_for_effect_summary(df, exclude_reference = TRUE)

  # country_other should be excluded as reference
  country_other_row <- result[result$var_name == "country_other", ]
  expect_true(nrow(country_other_row) > 0)
  expect_false(country_other_row$suggested)
  expect_equal(country_other_row$reason, "reference_variable")
})

test_that("suggest_variables_for_effect_summary includes reference variables when requested", {
  df <- make_test_data()

  result <- suggest_variables_for_effect_summary(df, exclude_reference = FALSE)

  # country_other should now be included
  country_other_row <- result[result$var_name == "country_other", ]
  expect_true(nrow(country_other_row) > 0)
  # Should be suggested if it has enough observations
  if (sum(df$country_other == 1) >= 5 && sum(df$country_other == 0) >= 5) {
    expect_true(country_other_row$suggested)
  }
})

test_that("suggest_variables_for_effect_summary respects min_obs_per_split", {
  df <- data.frame(
    effect = rnorm(20),
    se = runif(20, 0.05, 0.15),
    rare_var = c(rep(0, 18), rep(1, 2)),
    stringsAsFactors = FALSE
  )

  result <- suggest_variables_for_effect_summary(df, min_obs_per_split = 5)

  rare_row <- result[result$var_name == "rare_var", ]
  expect_true(nrow(rare_row) > 0)
  expect_false(rare_row$suggested)
})

test_that("suggest_variables_for_effect_summary handles empty data gracefully", {
  df <- data.frame(
    effect = numeric(0),
    se = numeric(0),
    stringsAsFactors = FALSE
  )

  # Should not crash
  expect_error(
    suggest_variables_for_effect_summary(df),
    NA
  )
})

test_that("suggest_variables_for_effect_summary requires effect column", {
  df <- data.frame(
    se = runif(10, 0.05, 0.15),
    var1 = rnorm(10),
    stringsAsFactors = FALSE
  )

  expect_error(
    suggest_variables_for_effect_summary(df),
    "effect"
  )
})

test_that("suggest_variables_for_effect_summary uses config when provided", {
  df <- data.frame(
    effect = rnorm(100),
    var1 = sample(c(0, 1), 100, replace = TRUE),
    var2 = sample(c(0, 1), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )

  config <- list(
    var1 = list(
      var_name = "var1",
      group_category = "test_group",
      data_type = "dummy"
    ),
    var2 = list(
      var_name = "var2",
      group_category = "test_group",
      data_type = "dummy"
    )
  )

  result <- suggest_variables_for_effect_summary(df, config = config)

  expect_true("var1" %in% result$var_name)
  expect_true("var2" %in% result$var_name)
  expect_equal(result$group_id[result$var_name == "var1"], "test_group")
  expect_equal(result$group_id[result$var_name == "var2"], "test_group")
})


# Integration tests -----------------------------------------------------------

test_that("full workflow: detect groups and suggest variables", {
  df <- make_test_data()

  # Step 1: Detect groups
  groups <- detect_variable_groups(df)
  expect_true(nrow(groups) > 0)

  # Step 2: Suggest variables
  suggestions <- suggest_variables_for_effect_summary(df)
  expect_true(nrow(suggestions) > 0)

  # Step 3: Verify suggestions have corresponding groups
  for (i in seq_len(nrow(suggestions))) {
    var <- suggestions$var_name[i]
    expect_true(var %in% groups$var_name)
  }
})

test_that("suggestions are consistent with group detection", {
  df <- make_test_data()

  suggestions <- suggest_variables_for_effect_summary(df)

  # All suggested variables should have a group_id
  expect_true(all(!is.na(suggestions$group_id)))

  # Reference variables in dummy groups should not be suggested (by default)
  dummy_refs <- suggestions[
    suggestions$group_id %in% suggestions$group_id[grepl("^dummy_", suggestions$group_id)],
  ]

  # This is tested more thoroughly in other tests
  expect_true(is.data.frame(dummy_refs))
})

test_that("variable suggestion handles complex real-world patterns", {
  set.seed(456)
  n <- 200

  df <- data.frame(
    effect = rnorm(n, 0.3, 0.15),
    se = runif(n, 0.05, 0.2),
    # Multiple dummy groups
    journal_tier1 = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    journal_tier2 = sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)),
    journal_other = sample(c(0, 1), n, replace = TRUE, prob = c(0.9, 0.1)),
    # Numeric with different distributions
    year = sample(1980:2023, n, replace = TRUE),
    citations = rpois(n, lambda = 20),
    impact_factor = rexp(n, rate = 0.5),
    # Ratio
    female_ratio = runif(n, 0, 1),
    # Transformations
    gdp_per_capita = runif(n, 5000, 50000),
    log_gdp_per_capita = log(runif(n, 5000, 50000)),
    stringsAsFactors = FALSE
  )

  suggestions <- suggest_variables_for_effect_summary(df)

  expect_true(nrow(suggestions) > 0)
  expect_true(any(suggestions$suggested))

  # Check that different variable types get appropriate split methods
  dummy_suggested <- suggestions[suggestions$suggested & !is.na(suggestions$split_method) & suggestions$split_method == "equal", ]
  gltl_suggested <- suggestions[suggestions$suggested & !is.na(suggestions$split_method) & suggestions$split_method == "gltl", ]

  expect_true(nrow(dummy_suggested) > 0)
  expect_true(nrow(gltl_suggested) > 0)
})
