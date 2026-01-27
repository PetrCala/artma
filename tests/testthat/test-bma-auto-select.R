box::use(
  testthat[
    describe,
    expect_equal,
    expect_gt,
    expect_true,
    expect_type,
    test_that
  ],
  withr[local_options]
)

#' Tests for BMA automatic variable selection
#'
#' These tests verify that the BMA method correctly uses the variable
#' suggestion logic to automatically detect suitable moderator variables.

# Test data generator ---------------------------------------------------------

make_bma_test_data <- function() {
  set.seed(456)
  n <- 100

  data.frame(
    effect = rnorm(n, 0.3, 0.15),
    se = runif(n, 0.05, 0.2),
    # Numeric variables with good variance
    year = sample(1990:2020, n, replace = TRUE),
    sample_size = sample(50:500, n, replace = TRUE),
    # Binary variables (dummy group)
    country_usa = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    country_uk = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    country_other = sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)),
    # Ratio variable
    quality_score = runif(n, 0, 1),
    # Low variance (should be excluded)
    constant_like = rnorm(n, 100, 0.01),
    stringsAsFactors = FALSE
  )
}

make_bma_test_config <- function() {
  list(
    effect = list(
      var_name = "effect",
      var_name_verbose = "Effect Size",
      data_type = "float",
      bma = NA
    ),
    se = list(
      var_name = "se",
      var_name_verbose = "Standard Error",
      data_type = "float",
      bma = NA
    ),
    year = list(
      var_name = "year",
      var_name_verbose = "Publication Year",
      data_type = "int",
      group_category = "singleton_year",
      bma = NA
    ),
    sample_size = list(
      var_name = "sample_size",
      var_name_verbose = "Sample Size",
      data_type = "int",
      group_category = "singleton_sample_size",
      bma = NA
    ),
    country_usa = list(
      var_name = "country_usa",
      var_name_verbose = "USA",
      data_type = "dummy",
      group_category = "dummy_country",
      bma = NA
    ),
    country_uk = list(
      var_name = "country_uk",
      var_name_verbose = "UK",
      data_type = "dummy",
      group_category = "dummy_country",
      bma = NA
    ),
    country_other = list(
      var_name = "country_other",
      var_name_verbose = "Other Country",
      data_type = "dummy",
      group_category = "dummy_country",
      bma = NA
    ),
    quality_score = list(
      var_name = "quality_score",
      var_name_verbose = "Quality Score",
      data_type = "perc",
      group_category = "singleton_quality_score",
      bma = NA
    ),
    constant_like = list(
      var_name = "constant_like",
      var_name_verbose = "Constant Like",
      data_type = "float",
      group_category = "singleton_constant_like",
      bma = NA
    )
  )
}


# Integration with variable suggestion ----------------------------------------

test_that("auto_select_bma_variables uses variable suggestion logic", {
  # We can't actually call auto_select_bma_variables interactively,
  # but we can verify the underlying suggestion logic works for BMA

  box::use(
    artma / variable / suggestion[suggest_variables_for_effect_summary]
  )

  df <- make_bma_test_data()
  config <- make_bma_test_config()

  # Get suggestions (what auto_select_bma_variables would use)
  suggestions <- suggest_variables_for_effect_summary(
    df,
    config = config,
    min_obs_per_split = 5,
    min_variance_ratio = 0.01,
    exclude_reference = TRUE
  )

  expect_true(is.data.frame(suggestions))
  expect_true("suggested" %in% colnames(suggestions))
  expect_true("var_name" %in% colnames(suggestions))
  expect_true("reason" %in% colnames(suggestions))

  # Should have some suggestions
  suggested_vars <- suggestions[suggestions$suggested, ]
  expect_gt(nrow(suggested_vars), 0)

  # Should not suggest reserved variables
  expect_false("effect" %in% suggested_vars$var_name)
  expect_false("se" %in% suggested_vars$var_name)

  # Should not suggest low variance variable
  expect_false("constant_like" %in% suggested_vars$var_name)
})

test_that("BMA suggestions exclude reference variables correctly", {
  box::use(
    artma / variable / suggestion[suggest_variables_for_effect_summary],
    artma / variable / detection[detect_variable_groups]
  )

  df <- make_bma_test_data()
  config <- make_bma_test_config()

  # First detect groups to understand reference assignment
  groups <- detect_variable_groups(df, config = config)

  # Find which country variable is marked as reference
  country_group <- groups[groups$group_type == "dummy" & grepl("country", groups$var_name), ]
  if (nrow(country_group) > 0) {
    reference_var <- country_group$var_name[country_group$is_reference]

    # Get suggestions with reference exclusion
    suggestions <- suggest_variables_for_effect_summary(
      df,
      config = config,
      exclude_reference = TRUE
    )

    suggested_names <- suggestions$var_name[suggestions$suggested]

    # Reference variable should not be suggested
    if (length(reference_var) > 0) {
      expect_false(reference_var %in% suggested_names)
    }

    # At least one non-reference country variable should be suggested
    non_ref_countries <- country_group$var_name[!country_group$is_reference]
    expect_true(any(non_ref_countries %in% suggested_names))
  }
})

test_that("BMA suggestions include appropriate variable types", {
  box::use(
    artma / variable / suggestion[suggest_variables_for_effect_summary]
  )

  df <- make_bma_test_data()
  config <- make_bma_test_config()

  suggestions <- suggest_variables_for_effect_summary(df, config = config)
  suggested_vars <- suggestions[suggestions$suggested, ]

  # Should have some suggestions
  expect_gt(nrow(suggested_vars), 0)

  # Should suggest binary variables (can be used as dummies in BMA)
  country_vars <- c("country_usa", "country_uk", "country_other")
  expect_true(any(country_vars %in% suggested_vars$var_name))

  # Should suggest ratio variables (good variance relative to mean)
  expect_true("quality_score" %in% suggested_vars$var_name)

  # Note: year and sample_size may or may not be suggested depending on
  # their coefficient of variation. This is correct behavior - the algorithm
  # filters based on informative variance.
})

test_that("BMA variable suggestion reasons are appropriate", {
  box::use(
    artma / variable / suggestion[suggest_variables_for_effect_summary]
  )

  df <- make_bma_test_data()
  config <- make_bma_test_config()

  suggestions <- suggest_variables_for_effect_summary(df, config = config)
  suggested_vars <- suggestions[suggestions$suggested, ]

  # Check that reasons are informative
  expect_true(all(nzchar(suggested_vars$reason)))
  expect_true(all(!is.na(suggested_vars$reason)))

  # Common reasons should appear
  possible_reasons <- c(
    "informative_numeric",
    "informative_dummy",
    "ratio_variable",
    "dummy_sufficient_obs"
  )
  expect_true(any(suggested_vars$reason %in% possible_reasons))
})


# Variable filtering for BMA --------------------------------------------------

test_that("suggestions filter out inappropriate variables for BMA", {
  box::use(
    artma / variable / suggestion[suggest_variables_for_effect_summary]
  )

  # Create data with problematic variables
  set.seed(789)
  n <- 50

  df <- data.frame(
    effect = rnorm(n, 0.3, 0.1),
    se = runif(n, 0.05, 0.15),
    # Good variable with high relative variance
    score = rnorm(n, 5, 2),
    # Constant variable (bad for BMA)
    constant = rep(1, n),
    # Very low variance (bad for BMA)
    low_var = rnorm(n, 100, 0.001),
    # Insufficient observations per split
    rare = c(rep(0, 48), rep(1, 2)),
    stringsAsFactors = FALSE
  )

  config <- list(
    effect = list(var_name = "effect", data_type = "float", bma = NA),
    se = list(var_name = "se", data_type = "float", bma = NA),
    score = list(var_name = "score", data_type = "float", bma = NA),
    constant = list(var_name = "constant", data_type = "int", bma = NA),
    low_var = list(var_name = "low_var", data_type = "float", bma = NA),
    rare = list(var_name = "rare", data_type = "dummy", bma = NA)
  )

  suggestions <- suggest_variables_for_effect_summary(df, config = config)

  # score should be suggested (good variable with high CV)
  score_row <- suggestions[suggestions$var_name == "score", ]
  expect_true(score_row$suggested)

  # constant should not be suggested (no variance)
  constant_row <- suggestions[suggestions$var_name == "constant", ]
  expect_false(constant_row$suggested)
  expect_equal(constant_row$reason, "no_variance")

  # low_var should not be suggested (very low variance)
  low_var_row <- suggestions[suggestions$var_name == "low_var", ]
  expect_false(low_var_row$suggested)
  expect_equal(low_var_row$reason, "low_variance")

  # rare should not be suggested (insufficient observations)
  rare_row <- suggestions[suggestions$var_name == "rare", ]
  expect_false(rare_row$suggested)
})


# Dummy variable trap avoidance -----------------------------------------------

test_that("reference exclusion prevents dummy variable trap", {
  box::use(
    artma / variable / suggestion[suggest_variables_for_effect_summary],
    artma / variable / detection[detect_variable_groups]
  )

  df <- make_bma_test_data()
  config <- make_bma_test_config()

  # Detect groups first
  groups <- detect_variable_groups(df, config = config)

  # Get dummy groups
  dummy_groups <- unique(groups$group_id[groups$group_type == "dummy"])

  if (length(dummy_groups) > 0) {
    # Get suggestions with reference exclusion
    suggestions <- suggest_variables_for_effect_summary(
      df,
      config = config,
      exclude_reference = TRUE
    )

    suggested_names <- suggestions$var_name[suggestions$suggested]

    # For each dummy group, check that not all variables are suggested
    for (group_id in dummy_groups) {
      group_vars <- groups$var_name[groups$group_id == group_id]
      suggested_in_group <- sum(group_vars %in% suggested_names)

      # Should suggest fewer variables than total in group (to avoid trap)
      expect_true(suggested_in_group < length(group_vars))

      # At least one should be suggested (unless all filtered for other reasons)
      if (any(group_vars %in% suggestions$var_name)) {
        expect_gt(suggested_in_group, 0)
      }
    }
  }
})


# Workflow compatibility ------------------------------------------------------

test_that("BMA suggestions are compatible with BMA data preparation", {
  box::use(
    artma / variable / suggestion[suggest_variables_for_effect_summary],
    artma / econometric / bma[get_bma_data]
  )

  df <- make_bma_test_data()
  config <- make_bma_test_config()

  # Get suggestions
  suggestions <- suggest_variables_for_effect_summary(df, config = config)
  suggested_names <- suggestions$var_name[suggestions$suggested]

  if (length(suggested_names) > 0) {
    # Prepare BMA data structure (as BMA method would)
    all_vars <- c("effect", suggested_names)

    bma_var_list <- data.frame(
      var_name = all_vars,
      bma = rep(TRUE, length(all_vars)),
      to_log_for_bma = rep(FALSE, length(all_vars)),
      bma_reference_var = rep(FALSE, length(all_vars)),
      stringsAsFactors = FALSE
    )

    # This should not error
    expect_error(
      {
        bma_data <- get_bma_data(
          df,
          bma_var_list,
          variable_info = all_vars,
          scale_data = TRUE,
          from_vector = TRUE,
          include_reference_groups = FALSE
        )
      },
      NA
    )
  }
})


# Edge cases ------------------------------------------------------------------

test_that("BMA auto-select handles empty dataset gracefully", {
  box::use(
    artma / variable / suggestion[suggest_variables_for_effect_summary]
  )

  df <- data.frame(
    effect = numeric(0),
    se = numeric(0),
    var1 = numeric(0),
    stringsAsFactors = FALSE
  )

  config <- list(
    effect = list(var_name = "effect", bma = NA),
    se = list(var_name = "se", bma = NA),
    var1 = list(var_name = "var1", bma = NA)
  )

  # Should not error
  expect_error(
    {
      suggestions <- suggest_variables_for_effect_summary(df, config = config)
    },
    NA
  )
})

test_that("BMA auto-select handles dataset with only reserved columns", {
  box::use(
    artma / variable / suggestion[suggest_variables_for_effect_summary]
  )

  df <- data.frame(
    effect = rnorm(50, 0.3, 0.1),
    se = runif(50, 0.05, 0.15),
    study_size = sample(20:100, 50, replace = TRUE),
    stringsAsFactors = FALSE
  )

  config <- list(
    effect = list(var_name = "effect", bma = NA),
    se = list(var_name = "se", bma = NA),
    study_size = list(var_name = "study_size", bma = NA)
  )

  suggestions <- suggest_variables_for_effect_summary(df, config = config)

  # Reserved columns should not be suggested
  suggested_names <- suggestions$var_name[suggestions$suggested]
  expect_false("effect" %in% suggested_names)
  expect_false("se" %in% suggested_names)
})
