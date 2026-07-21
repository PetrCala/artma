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

  result <- variable_summary_stats(df)$tables$summary

  expect_named(result, c(
    "Var Name", "Var Class", "Mean", "Median",
    "Min", "Max", "SD", "Obs", "Missing obs"
  ))
  expect_identical(result$`Var Name`, c("Effect", "Is RCT"))
  # 3 non-missing rows for each variable, even though `is_rct` has only one 1.
  expect_equal(result$Obs, c("3", "3"))
})
