box::use(
  testthat[
    describe,
    expect_equal,
    expect_identical,
    expect_named,
    expect_null,
    expect_s3_class,
    expect_true,
    it,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / methods / funnel_plot[funnel_plot]
)


create_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  data.frame(
    effect = rnorm(n, mean = 0.5, sd = 0.3),
    precision = runif(n, min = 5, max = 50),
    study_id = rep(1:10, each = n / 10),
    stringsAsFactors = FALSE
  )
}


test_that("funnel_plot creates a plot with required columns", {
  local_options(
    "artma.methods.funnel_plot.effect_proximity" = 0.2,
    "artma.methods.funnel_plot.maximum_precision" = 0.2,
    "artma.methods.funnel_plot.precision_to_log" = FALSE,
    "artma.methods.funnel_plot.use_study_medians" = FALSE,
    "artma.methods.funnel_plot.add_zero" = TRUE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- create_test_data()
  result <- funnel_plot(df)

  expect_s3_class(result, "artma_funnel_plot")
  expect_named(result, c("plot", "n_points", "n_outliers_removed", "used_study_medians"))
  expect_true(ggplot2::is.ggplot(result$plot))
  expect_identical(result$used_study_medians, FALSE)
})


test_that("funnel_plot respects use_study_medians option", {
  local_options(
    "artma.methods.funnel_plot.effect_proximity" = 1,
    "artma.methods.funnel_plot.maximum_precision" = 1,
    "artma.methods.funnel_plot.precision_to_log" = FALSE,
    "artma.methods.funnel_plot.use_study_medians" = TRUE,
    "artma.methods.funnel_plot.add_zero" = TRUE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- create_test_data(n = 100)
  result <- funnel_plot(df)

  expect_identical(result$used_study_medians, TRUE)
  expect_equal(result$n_points, 10)
})


test_that("funnel_plot filters outliers correctly", {
  local_options(
    "artma.methods.funnel_plot.effect_proximity" = 0.1,
    "artma.methods.funnel_plot.maximum_precision" = 0.1,
    "artma.methods.funnel_plot.precision_to_log" = FALSE,
    "artma.methods.funnel_plot.use_study_medians" = FALSE,
    "artma.methods.funnel_plot.add_zero" = TRUE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- data.frame(
    effect = c(0, 0, 0, 10),
    precision = c(10, 10, 10, 100),
    study_id = 1:4
  )

  result <- funnel_plot(df)

  expect_true(result$n_outliers_removed > 0)
  expect_true(result$n_points < nrow(df))
})


test_that("funnel_plot with no outlier filtering keeps all points", {
  local_options(
    "artma.methods.funnel_plot.effect_proximity" = 1,
    "artma.methods.funnel_plot.maximum_precision" = 1,
    "artma.methods.funnel_plot.precision_to_log" = FALSE,
    "artma.methods.funnel_plot.use_study_medians" = FALSE,
    "artma.methods.funnel_plot.add_zero" = TRUE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- create_test_data(n = 50)
  result <- funnel_plot(df)

  expect_equal(result$n_outliers_removed, 0)
  expect_equal(result$n_points, 50)
})


test_that("funnel_plot handles different themes", {
  themes <- c("blue", "yellow", "green", "red", "purple")

  for (theme in themes) {
    local_options(
      "artma.methods.funnel_plot.effect_proximity" = 1,
      "artma.methods.funnel_plot.maximum_precision" = 1,
      "artma.methods.funnel_plot.precision_to_log" = FALSE,
      "artma.methods.funnel_plot.use_study_medians" = FALSE,
      "artma.methods.funnel_plot.add_zero" = TRUE,
      "artma.visualization.theme" = theme,
      "artma.visualization.export_graphics" = FALSE,
      "artma.verbose" = 1
    )

    df <- create_test_data(n = 20)
    result <- funnel_plot(df)

    expect_true(ggplot2::is.ggplot(result$plot))
  }
})


test_that("funnel_plot handles precision_to_log option", {
  local_options(
    "artma.methods.funnel_plot.effect_proximity" = 1,
    "artma.methods.funnel_plot.maximum_precision" = 1,
    "artma.methods.funnel_plot.precision_to_log" = TRUE,
    "artma.methods.funnel_plot.use_study_medians" = FALSE,
    "artma.methods.funnel_plot.add_zero" = TRUE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- create_test_data(n = 20)
  result <- funnel_plot(df)

  expect_true(ggplot2::is.ggplot(result$plot))
  expect_s3_class(result, "artma_funnel_plot")
})


test_that("funnel_plot returns empty result when all data filtered", {
  local_options(
    "artma.methods.funnel_plot.effect_proximity" = 0,
    "artma.methods.funnel_plot.maximum_precision" = 0,
    "artma.methods.funnel_plot.precision_to_log" = FALSE,
    "artma.methods.funnel_plot.use_study_medians" = FALSE,
    "artma.methods.funnel_plot.add_zero" = TRUE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- data.frame(
    effect = c(10, -10),
    precision = c(100, 100),
    study_id = 1:2
  )

  result <- funnel_plot(df)

  expect_null(result$plot)
  expect_equal(result$n_points, 0)
})
