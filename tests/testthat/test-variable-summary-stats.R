box::use(
  testthat[
    expect_equal,
    expect_identical,
    expect_named,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / methods / variable_summary_stats[variable_summary_stats]
)

make_config_entry <- function(name, verbose, data_type) {
  list(
    var_name = name,
    var_name_verbose = verbose,
    data_type = data_type,
    variable_summary = TRUE
  )
}

test_that("Obs counts non-missing observations, not the count of ones for dummy variables", {
  local_options(
    "artma.data.columns" = list(
      effect = make_config_entry("effect", "Effect", "float"),
      is_rct = make_config_entry("is_rct", "Is RCT", "int")
    ),
    "artma.methods.variable_summary_stats.use_verbose_names" = TRUE,
    "artma.output.number_of_decimals" = 3,
    "artma.verbose" = 1
  )

  df <- data.frame(
    effect = c(0.1, 0.2, 0.3, NA),
    is_rct = c(1, 0, 0, NA)
  )

  method_result <- variable_summary_stats(df)
  result <- method_result$tables$summary

  expect_named(result, c(
    "Var Name", "Var Class", "Mean", "Median",
    "Min", "Max", "SD", "Obs", "Missing obs"
  ))
  expect_identical(result$`Var Name`, c("Effect", "Is RCT"))
  # 3 non-missing rows for each variable, even though `is_rct` has only one 1.
  expect_equal(result$Obs, c("3", "3"))
  expect_equal(result$`Missing obs`, c("25%", "25%"))

  # The estimates slot reports the same run as numbers, with missingness as a
  # proportion rather than the display table's percentage string.
  estimates <- method_result$estimates
  expect_identical(unique(estimates$model), c("Effect", "Is RCT"))
  expect_equal(estimates$estimate[estimates$term == "missing_share"], c(0.25, 0.25))
  expect_equal(estimates$estimate[estimates$term == "mean"], c(0.2, 1 / 3))
  expect_equal(unique(estimates$n_obs), 3L)
})
