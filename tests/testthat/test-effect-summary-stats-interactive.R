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
  artma / libs / effect_summary_stats_interactive[
    update_config_with_selections
  ]
)

# Test data generators --------------------------------------------------------

make_test_config <- function() {
  list(
    effect = list(
      var_name = "effect",
      var_name_verbose = "Effect Size",
      data_type = "float",
      effect_sum_stats = NA,
      equal = NA,
      gltl = NA
    ),
    year = list(
      var_name = "year",
      var_name_verbose = "Publication Year",
      data_type = "int",
      effect_sum_stats = NA,
      equal = NA,
      gltl = NA
    ),
    quality = list(
      var_name = "quality",
      var_name_verbose = "Study Quality",
      data_type = "perc",
      effect_sum_stats = NA,
      equal = NA,
      gltl = NA
    ),
    published = list(
      var_name = "published",
      var_name_verbose = "Published",
      data_type = "dummy",
      effect_sum_stats = NA,
      equal = NA,
      gltl = NA
    )
  )
}

make_test_data <- function() {
  set.seed(123)
  n <- 50

  data.frame(
    effect = rnorm(n, 0.5, 0.2),
    se = runif(n, 0.05, 0.15),
    study_size = sample(20:100, n, replace = TRUE),
    year = sample(1990:2020, n, replace = TRUE),
    quality = runif(n, 0, 1),
    published = sample(c(0, 1), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}


# Tests for update_config_with_selections -------------------------------------

test_that("update_config_with_selections sets equal split correctly", {
  config <- make_test_config()

  var_configs <- list(
    published = list(
      var_name = "published",
      split_method = "equal",
      split_value = "1"
    )
  )

  result <- update_config_with_selections(config, var_configs)

  expect_true(result$published$effect_sum_stats)
  expect_equal(result$published$equal, "1")
  expect_true(is.na(result$published$gltl))
})

test_that("update_config_with_selections sets gltl split correctly", {
  config <- make_test_config()

  var_configs <- list(
    year = list(
      var_name = "year",
      split_method = "gltl",
      split_value = "mean"
    )
  )

  result <- update_config_with_selections(config, var_configs)

  expect_true(result$year$effect_sum_stats)
  expect_equal(result$year$gltl, "mean")
  expect_true(is.na(result$year$equal))
})

test_that("update_config_with_selections handles multiple variables", {
  config <- make_test_config()

  var_configs <- list(
    year = list(
      var_name = "year",
      split_method = "gltl",
      split_value = "median"
    ),
    published = list(
      var_name = "published",
      split_method = "equal",
      split_value = "1"
    ),
    quality = list(
      var_name = "quality",
      split_method = "gltl",
      split_value = "0.5"
    )
  )

  result <- update_config_with_selections(config, var_configs)

  # Check all three variables configured
  expect_true(result$year$effect_sum_stats)
  expect_equal(result$year$gltl, "median")

  expect_true(result$published$effect_sum_stats)
  expect_equal(result$published$equal, "1")

  expect_true(result$quality$effect_sum_stats)
  expect_equal(result$quality$gltl, "0.5")
})

test_that("update_config_with_selections handles empty var_configs", {
  config <- make_test_config()
  var_configs <- list()

  result <- update_config_with_selections(config, var_configs)

  # Config should be unchanged
  expect_equal(result, config)
})

test_that("update_config_with_selections skips missing variables", {
  config <- make_test_config()

  var_configs <- list(
    nonexistent = list(
      var_name = "nonexistent",
      split_method = "equal",
      split_value = "1"
    ),
    year = list(
      var_name = "year",
      split_method = "gltl",
      split_value = "mean"
    )
  )

  result <- update_config_with_selections(config, var_configs)

  # year should be configured
  expect_true(result$year$effect_sum_stats)
  # nonexistent should not cause errors
  expect_false("nonexistent" %in% names(result))
})

test_that("update_config_with_selections preserves unmodified config entries", {
  config <- make_test_config()

  var_configs <- list(
    year = list(
      var_name = "year",
      split_method = "gltl",
      split_value = "mean"
    )
  )

  result <- update_config_with_selections(config, var_configs)

  # Other variables should remain unchanged
  expect_true(is.na(result$published$effect_sum_stats))
  expect_true(is.na(result$quality$effect_sum_stats))
  expect_true(is.na(result$effect$effect_sum_stats))
})


# Note: prompt_equal_value and prompt_gltl_value are not exported
# and are difficult to test as they use readline(). They are
# tested indirectly through the integration workflow.


# Integration tests -----------------------------------------------------------

test_that("full workflow: auto suggestions converted to config updates", {
  df <- make_test_data()
  config <- make_test_config()

  # Simulate automatic suggestions
  var_configs <- list(
    year = list(
      var_name = "year",
      split_method = "gltl",
      split_value = "mean"
    ),
    published = list(
      var_name = "published",
      split_method = "equal",
      split_value = "1"
    )
  )

  updated_config <- update_config_with_selections(config, var_configs)

  # Verify config is properly updated
  expect_true(updated_config$year$effect_sum_stats)
  expect_equal(updated_config$year$gltl, "mean")

  expect_true(updated_config$published$effect_sum_stats)
  expect_equal(updated_config$published$equal, "1")

  # Verify other variables unchanged
  expect_true(is.na(updated_config$quality$effect_sum_stats))
})

test_that("workflow handles gltl with numeric threshold", {
  config <- make_test_config()

  var_configs <- list(
    quality = list(
      var_name = "quality",
      split_method = "gltl",
      split_value = "0.75"
    )
  )

  updated_config <- update_config_with_selections(config, var_configs)

  expect_true(updated_config$quality$effect_sum_stats)
  expect_equal(updated_config$quality$gltl, "0.75")
})

test_that("workflow handles equal with numeric value", {
  config <- make_test_config()

  var_configs <- list(
    year = list(
      var_name = "year",
      split_method = "equal",
      split_value = "2010"
    )
  )

  updated_config <- update_config_with_selections(config, var_configs)

  expect_true(updated_config$year$effect_sum_stats)
  expect_equal(updated_config$year$equal, "2010")
})

test_that("config updates properly clear opposite split method", {
  config <- make_test_config()

  # First set gltl
  var_configs1 <- list(
    year = list(
      var_name = "year",
      split_method = "gltl",
      split_value = "mean"
    )
  )
  config1 <- update_config_with_selections(config, var_configs1)

  expect_equal(config1$year$gltl, "mean")
  expect_true(is.na(config1$year$equal))

  # Now set equal (should clear gltl)
  var_configs2 <- list(
    year = list(
      var_name = "year",
      split_method = "equal",
      split_value = "2010"
    )
  )
  config2 <- update_config_with_selections(config1, var_configs2)

  expect_equal(config2$year$equal, "2010")
  expect_true(is.na(config2$year$gltl))
})


# Edge cases ------------------------------------------------------------------

test_that("update_config_with_selections handles NA values correctly", {
  config <- make_test_config()

  # Explicitly set some values to NA
  config$year$equal <- "some_value"
  config$year$gltl <- "other_value"

  var_configs <- list(
    year = list(
      var_name = "year",
      split_method = "equal",
      split_value = "2020"
    )
  )

  result <- update_config_with_selections(config, var_configs)

  # equal should be set, gltl should be NA
  expect_equal(result$year$equal, "2020")
  expect_true(is.na(result$year$gltl))
})

test_that("update_config_with_selections with character split values", {
  config <- make_test_config()

  var_configs <- list(
    year = list(
      var_name = "year",
      split_method = "gltl",
      split_value = "median" # Character, not numeric
    )
  )

  result <- update_config_with_selections(config, var_configs)

  expect_equal(result$year$gltl, "median")
})

test_that("var_configs with extra fields doesn't break update", {
  config <- make_test_config()

  var_configs <- list(
    year = list(
      var_name = "year",
      split_method = "gltl",
      split_value = "mean",
      extra_field = "should be ignored",
      another_field = 123
    )
  )

  # Should not error
  result <- update_config_with_selections(config, var_configs)

  expect_true(result$year$effect_sum_stats)
  expect_equal(result$year$gltl, "mean")
})
