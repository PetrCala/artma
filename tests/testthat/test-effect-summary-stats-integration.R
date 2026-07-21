box::use(
  testthat[
    expect_equal,
    expect_no_error,
    expect_true,
    test_that
  ],
  withr[local_options]
)

#' Integration tests for effect summary stats with interactive selection
#'
#' These tests verify that all module imports work correctly and that
#' the integration between effect_summary_stats and the interactive
#' module functions properly.

# Test box imports ------------------------------------------------------------

# The interactive module's readline-driven exports are not called by any other
# test (they prompt), so an existence check is their only unit coverage. Every
# other import these smoke tests once verified (data_config, variable
# suggestion/detection, utils, validation) is exercised by real callers in this
# and neighbouring files, so their loadability is covered transitively.
test_that("interactive effect-summary module exports its readline entry points", {
  box::use(
    artma / interactive / effect_summary_stats[
      prompt_effect_summary_var_selection,
      auto_select_effect_summary_vars
    ]
  )

  expect_true(is.function(prompt_effect_summary_var_selection))
  expect_true(is.function(auto_select_effect_summary_vars))
})


# Workflow integration tests --------------------------------------------------

test_that("effect_summary_stats defaults to the All Data row in non-interactive mode", {
  box::use(
    artma / methods / effect_summary_stats[effect_summary_stats]
  )

  # Create minimal data
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    study_size = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  # Set empty config and non-interactive mode
  local_options(
    "artma.data.columns" = list(
      effect = list(
        var_name = "effect",
        effect_sum_stats = NA,
        equal = NA,
        gltl = NA
      ),
      study_size = list(
        var_name = "study_size",
        effect_sum_stats = NA,
        equal = NA,
        gltl = NA
      )
    ),
    "artma.verbose" = 1
  )

  # Should default the effect column in, producing the "All Data" row
  result <- effect_summary_stats(df)$tables$summary

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$`Var Name`, "All Data")
})

test_that("effect_summary_stats processes configured variables correctly", {
  box::use(
    artma / methods / effect_summary_stats[effect_summary_stats]
  )

  df <- data.frame(
    effect = c(0.1, 0.2, 0.3, 0.4, 0.5),
    study_size = c(10, 20, 30, 40, 50),
    year = c(2010, 2011, 2012, 2013, 2014),
    stringsAsFactors = FALSE
  )

  # Configure year for effect summary stats
  local_options(
    "artma.data.columns" = list(
      effect = list(
        var_name = "effect",
        var_name_verbose = "Effect",
        data_type = "float",
        effect_sum_stats = NA,
        equal = NA,
        gltl = NA
      ),
      study_size = list(
        var_name = "study_size",
        var_name_verbose = "Study Size",
        data_type = "int",
        effect_sum_stats = NA,
        equal = NA,
        gltl = NA
      ),
      year = list(
        var_name = "year",
        var_name_verbose = "Year",
        data_type = "int",
        effect_sum_stats = TRUE,
        equal = NA_character_,
        gltl = "2012"
      )
    ),
    "artma.methods.effect_summary_stats.conf_level" = 0.95,
    "artma.methods.effect_summary_stats.formal_output" = FALSE,
    "artma.output.number_of_decimals" = 3,
    "artma.verbose" = 1
  )

  result <- effect_summary_stats(df)$tables$summary

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("Var Name" %in% colnames(result))

  # Should have "All Data" row and splits for year
  expect_true(any(grepl("All Data", result$`Var Name`)))
  expect_true(any(grepl("Year", result$`Var Name`)))
})


# Value-level assertions ------------------------------------------------------

test_that("effect_summary_stats computes the All Data row exactly", {
  box::use(
    artma / methods / effect_summary_stats[effect_summary_stats]
  )

  # Fixed dataset with hand-computable summary statistics.
  #   mean   = 0.32   median = 0.30   min = 0.10   max = 0.50
  #   sd     = 0.164 (rounded)        obs = 5
  #   z_0.95 = qnorm(0.975) = 1.959964
  #   se     = round(sd, 3) / sqrt(5); ci = mean +/- z * se -> [0.176, 0.464]
  #   weighted mean with w = 1 / study_size^2 -> 0.165
  df <- data.frame(
    effect = c(0.10, 0.24, 0.30, 0.46, 0.50),
    study_size = c(10, 20, 30, 40, 50),
    stringsAsFactors = FALSE
  )

  local_options(
    "artma.data.columns" = list(
      effect = list(var_name = "effect", effect_sum_stats = NA, equal = NA, gltl = NA),
      study_size = list(
        var_name = "study_size",
        var_name_verbose = "Study Size",
        data_type = "int",
        effect_sum_stats = TRUE,
        equal = NA,
        gltl = NA
      )
    ),
    "artma.methods.effect_summary_stats.conf_level" = 0.95,
    "artma.methods.effect_summary_stats.formal_output" = FALSE,
    "artma.output.number_of_decimals" = 3,
    "artma.verbose" = 1
  )

  result <- effect_summary_stats(df)$tables$summary
  all_data <- result[result$`Var Name` == "All Data", , drop = FALSE]

  expect_equal(nrow(all_data), 1L)
  expect_equal(all_data$Mean, 0.32)
  expect_equal(all_data$Median, 0.30)
  expect_equal(all_data$Min, 0.10)
  expect_equal(all_data$Max, 0.50)
  expect_equal(all_data$SD, 0.164)
  expect_equal(all_data$Obs, 5L)
  expect_equal(all_data$`CI lower`, 0.176)
  expect_equal(all_data$`CI upper`, 0.464)
  expect_equal(all_data$`Weighted Mean`, 0.165)
})

test_that("effect_summary_stats confidence interval widens with the confidence level", {
  box::use(
    artma / methods / effect_summary_stats[effect_summary_stats]
  )

  df <- data.frame(
    effect = c(0.10, 0.24, 0.30, 0.46, 0.50),
    study_size = c(10, 20, 30, 40, 50),
    stringsAsFactors = FALSE
  )

  config <- list(
    effect = list(var_name = "effect", effect_sum_stats = NA, equal = NA, gltl = NA),
    study_size = list(
      var_name = "study_size",
      var_name_verbose = "Study Size",
      data_type = "int",
      effect_sum_stats = TRUE,
      equal = NA,
      gltl = NA
    )
  )

  run_at <- function(conf_level) {
    local_options(
      "artma.data.columns" = config,
      "artma.methods.effect_summary_stats.conf_level" = conf_level,
      "artma.output.number_of_decimals" = 3,
      "artma.verbose" = 1
    )
    res <- effect_summary_stats(df)$tables$summary
    row <- res[res$`Var Name` == "All Data", , drop = FALSE]
    row$`CI upper` - row$`CI lower`
  }

  expect_true(run_at(0.99) > run_at(0.95))
  expect_true(run_at(0.95) > run_at(0.80))
})

# Config update integration ---------------------------------------------------

test_that("update_config_with_selections integrates with effect_summary_stats", {
  box::use(
    artma / interactive / effect_summary_stats[
      update_config_with_selections
    ],
    artma / methods / effect_summary_stats[effect_summary_stats]
  )

  df <- data.frame(
    effect = rnorm(20, 0.3, 0.1),
    study_size = sample(20:100, 20, replace = TRUE),
    published = sample(c(0, 1), 20, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Start with empty config
  config <- list(
    effect = list(
      var_name = "effect",
      effect_sum_stats = NA,
      equal = NA,
      gltl = NA
    ),
    study_size = list(
      var_name = "study_size",
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

  # Simulate user selection
  var_configs <- list(
    published = list(
      var_name = "published",
      split_method = "equal",
      split_value = "1"
    )
  )

  # Update config
  updated_config <- update_config_with_selections(config, var_configs)

  # Set config and run effect_summary_stats
  local_options(
    "artma.data.columns" = updated_config,
    "artma.methods.effect_summary_stats.conf_level" = 0.95,
    "artma.output.number_of_decimals" = 3,
    "artma.verbose" = 1
  )

  result <- effect_summary_stats(df)$tables$summary

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true(any(grepl("Published", result$`Var Name`)))
})


# Error handling --------------------------------------------------------------

test_that("effect_summary_stats handles missing data gracefully", {
  box::use(
    artma / methods / effect_summary_stats[effect_summary_stats]
  )

  df <- data.frame(
    effect = c(0.1, NA, 0.3),
    study_size = c(10, 20, 30),
    year = c(2010, 2011, 2012),
    stringsAsFactors = FALSE
  )

  local_options(
    "artma.data.columns" = list(
      effect = list(var_name = "effect", effect_sum_stats = NA, equal = NA, gltl = NA),
      study_size = list(var_name = "study_size", effect_sum_stats = NA, equal = NA, gltl = NA),
      year = list(
        var_name = "year",
        var_name_verbose = "Year",
        effect_sum_stats = TRUE,
        equal = NA,
        gltl = "2011"
      )
    ),
    "artma.verbose" = 1
  )

  # Should handle NA values without error
  expect_no_error({
    result <- effect_summary_stats(df)$tables$summary
  })
})

test_that("effect_summary_stats handles non-numeric variables correctly", {
  box::use(
    artma / methods / effect_summary_stats[effect_summary_stats]
  )

  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    study_size = c(10, 20, 30),
    category = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  local_options(
    "artma.data.columns" = list(
      effect = list(var_name = "effect", effect_sum_stats = NA, equal = NA, gltl = NA),
      study_size = list(var_name = "study_size", effect_sum_stats = NA, equal = NA, gltl = NA),
      category = list(
        var_name = "category",
        var_name_verbose = "Category",
        data_type = "category",
        effect_sum_stats = TRUE,
        equal = "A",
        gltl = NA
      )
    ),
    "artma.verbose" = 1
  )

  # Should warn about non-numeric variable
  result <- effect_summary_stats(df)$tables$summary

  expect_true(is.data.frame(result))
  # Category should be skipped, only "All Data" should appear
  expect_equal(nrow(result), 1)
})


# Realistic workflow test -----------------------------------------------------

test_that("full workflow: suggestion -> config -> analysis works end-to-end", {
  box::use(
    artma / variable / suggestion[suggest_variables_for_effect_summary],
    artma / interactive / effect_summary_stats[update_config_with_selections],
    artma / methods / effect_summary_stats[effect_summary_stats]
  )

  set.seed(123)
  n <- 50
  df <- data.frame(
    effect = rnorm(n, 0.3, 0.15),
    study_size = sample(20:100, n, replace = TRUE),
    year = sample(2000:2020, n, replace = TRUE),
    published = sample(c(0, 1), n, replace = TRUE),
    quality = runif(n, 0, 1),
    stringsAsFactors = FALSE
  )

  # Create base config
  config <- list(
    effect = list(var_name = "effect", effect_sum_stats = NA, equal = NA, gltl = NA),
    study_size = list(var_name = "study_size", effect_sum_stats = NA, equal = NA, gltl = NA),
    year = list(
      var_name = "year",
      var_name_verbose = "Year",
      data_type = "int",
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
    ),
    quality = list(
      var_name = "quality",
      var_name_verbose = "Quality",
      data_type = "perc",
      effect_sum_stats = NA,
      equal = NA,
      gltl = NA
    )
  )

  # Step 1: Get automatic suggestions
  suggestions <- suggest_variables_for_effect_summary(df, config)
  expect_true(is.data.frame(suggestions))
  suggested_vars <- suggestions[suggestions$suggested, ]

  # Step 2: Convert to config format
  if (nrow(suggested_vars) > 0) {
    var_configs <- list()
    for (i in seq_len(min(2, nrow(suggested_vars)))) {
      var <- suggested_vars[i, ]
      var_configs[[var$var_name]] <- list(
        var_name = var$var_name,
        split_method = var$split_method,
        split_value = var$split_value
      )
    }

    # Step 3: Update config
    updated_config <- update_config_with_selections(config, var_configs)

    # Step 4: Run analysis with updated config
    local_options(
      "artma.data.columns" = updated_config,
      "artma.verbose" = 1
    )

    result <- effect_summary_stats(df)$tables$summary

    # Verify we got results
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("All Data" %in% result$`Var Name`)
  }
})
