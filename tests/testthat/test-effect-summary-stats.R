box::use(
  testthat[
    expect_equal,
    expect_identical,
    expect_named,
    test_that
  ],
  withr[local_options],
  artma / methods / effect_summary_stats[effect_summary_stats]
)

make_config_entry <- function(name, verbose, data_type, equal = NA_real_, gltl = NA_real_) {
  list(
    var_name = name,
    var_name_verbose = verbose,
    data_type = data_type,
    effect_sum_stats = TRUE,
    effect_summary_stats = TRUE,
    equal = equal,
    gltl = gltl,
    gtlt = gltl
  )
}

test_that("effect summary stats computes segmented summaries", {
  local_options(
    "artma.data.config" = list(
      group = make_config_entry("group", "Group", "int", equal = 1),
      score = make_config_entry("score", "Score", "float", gltl = "median")
    ),
    "artma.methods.effect_summary_stats.conf_level" = 0.95,
    "artma.methods.effect_summary_stats.formal_output" = FALSE,
    "artma.output.number_of_decimals" = 3,
    "artma.verbose" = 1
  )

  df <- data.frame(
    effect = c(0.1, 0.2, 0.3, 0.4, NA),
    study_size = c(10, 10, 20, 20, 30),
    group = c(1, 1, 2, 2, 1),
    score = c(1, 2, 3, 4, 5)
  )

  result <- effect_summary_stats(df)

  expect_named(result, c(
    "Var Name", "Var Class", "Mean", "CI lower", "CI upper",
    "Weighted Mean", "WM CI lower", "WM CI upper",
    "Median", "Min", "Max", "SD", "Obs"
  ))
  expect_equal(result$`Var Name`, c(
    "All Data",
    "Group = 1",
    "Score >= 2.5",
    "Score < 2.5"
  ))
  expect_equal(result$Mean, c(0.25, 0.15, 0.35, 0.15))
  expect_equal(result$`CI lower`, c(0.124, 0.052, 0.252, 0.052))
  expect_equal(result$`CI upper`, c(0.376, 0.248, 0.448, 0.248))
  expect_equal(result$`Weighted Mean`, c(0.19, 0.15, 0.35, 0.15))
  expect_equal(result$`WM CI lower`, c(0.098, 0.081, 0.281, 0.081))
  expect_equal(result$`WM CI upper`, c(0.282, 0.219, 0.419, 0.219))
  expect_equal(result$Median, c(0.25, 0.15, 0.35, 0.15))
  expect_equal(result$Min, c(0.1, 0.1, 0.3, 0.1))
  expect_equal(result$Max, c(0.4, 0.2, 0.4, 0.2))
  expect_equal(result$SD, c(0.129, 0.071, 0.071, 0.071))
  expect_identical(result$Obs, c(4L, 2L, 2L, 2L))
})

test_that("formal output hides presentation columns", {
  local_options(
    "artma.data.config" = list(
      group = make_config_entry("group", "Group", "int", equal = 1)
    ),
    "artma.methods.effect_summary_stats.conf_level" = 0.9,
    "artma.methods.effect_summary_stats.formal_output" = TRUE,
    "artma.output.number_of_decimals" = 2,
    "artma.verbose" = 1
  )

  df <- data.frame(
    effect = c(0.2, 0.2, 0.3),
    study_size = c(5, 5, 10),
    group = c(1, 1, 0)
  )

  result <- effect_summary_stats(df)

  expect_named(result, c(
    "Var Name", "Mean", "CI lower", "CI upper",
    "Weighted Mean", "WM CI lower", "WM CI upper", "Obs"
  ))
  expect_identical(result$`Var Name`, c("All Data", "Group = 1"))
})
