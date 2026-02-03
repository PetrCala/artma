box::use(
  testthat[
    expect_equal,
    expect_identical,
    expect_named,
    expect_null,
    expect_s3_class,
    expect_true,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / methods / t_stat_histogram[t_stat_histogram]
)


create_test_data <- function(n = 200, seed = 42) {
  set.seed(seed)
  data.frame(
    t_stat = rnorm(n, mean = 0.5, sd = 2),
    stringsAsFactors = FALSE
  )
}


test_that("t_stat_histogram creates both plots with defaults", {
  local_options(
    "artma.methods.t_stat_histogram.lower_cutoff" = -120,
    "artma.methods.t_stat_histogram.upper_cutoff" = 120,
    "artma.methods.t_stat_histogram.critical_values" = 1.96,
    "artma.methods.t_stat_histogram.n_bins" = 80L,
    "artma.methods.t_stat_histogram.show_mean_line" = TRUE,
    "artma.methods.t_stat_histogram.show_density_curve" = TRUE,
    "artma.methods.t_stat_histogram.min_tick_distance" = 0.5,
    "artma.methods.t_stat_histogram.close_up_enabled" = TRUE,
    "artma.methods.t_stat_histogram.close_up_lower" = -10,
    "artma.methods.t_stat_histogram.close_up_upper" = 10,
    "artma.methods.t_stat_histogram.close_up_min_tick_distance" = 0.3,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- create_test_data()
  result <- t_stat_histogram(df)

  expect_s3_class(result, "artma_t_stat_histogram")
  expect_named(result, c(
    "plot_main", "plot_close_up", "n_observations",
    "n_outliers_main", "n_outliers_close_up",
    "mean_t_stat", "close_up_enabled"
  ))
  expect_true(ggplot2::is.ggplot(result$plot_main))
  expect_true(ggplot2::is.ggplot(result$plot_close_up))
  expect_identical(result$close_up_enabled, TRUE)
  expect_equal(result$n_observations, 200)
})


test_that("t_stat_histogram works without close-up", {
  local_options(
    "artma.methods.t_stat_histogram.lower_cutoff" = -120,
    "artma.methods.t_stat_histogram.upper_cutoff" = 120,
    "artma.methods.t_stat_histogram.critical_values" = 1.96,
    "artma.methods.t_stat_histogram.n_bins" = 80L,
    "artma.methods.t_stat_histogram.show_mean_line" = TRUE,
    "artma.methods.t_stat_histogram.show_density_curve" = TRUE,
    "artma.methods.t_stat_histogram.min_tick_distance" = 0.5,
    "artma.methods.t_stat_histogram.close_up_enabled" = FALSE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- create_test_data()
  result <- t_stat_histogram(df)

  expect_true(ggplot2::is.ggplot(result$plot_main))
  expect_null(result$plot_close_up)
  expect_identical(result$close_up_enabled, FALSE)
})


test_that("t_stat_histogram handles all themes", {
  themes <- c("blue", "yellow", "green", "red", "purple")

  for (theme in themes) {
    local_options(
      "artma.methods.t_stat_histogram.lower_cutoff" = -50,
      "artma.methods.t_stat_histogram.upper_cutoff" = 50,
      "artma.methods.t_stat_histogram.critical_values" = 1.96,
      "artma.methods.t_stat_histogram.n_bins" = 40L,
      "artma.methods.t_stat_histogram.show_mean_line" = TRUE,
      "artma.methods.t_stat_histogram.show_density_curve" = TRUE,
      "artma.methods.t_stat_histogram.min_tick_distance" = 0.5,
      "artma.methods.t_stat_histogram.close_up_enabled" = FALSE,
      "artma.visualization.theme" = theme,
      "artma.visualization.export_graphics" = FALSE,
      "artma.verbose" = 1
    )

    df <- create_test_data(n = 50)
    result <- t_stat_histogram(df)

    expect_true(ggplot2::is.ggplot(result$plot_main))
  }
})


test_that("t_stat_histogram filters outliers by cutoff", {
  local_options(
    "artma.methods.t_stat_histogram.lower_cutoff" = -5,
    "artma.methods.t_stat_histogram.upper_cutoff" = 5,
    "artma.methods.t_stat_histogram.critical_values" = 1.96,
    "artma.methods.t_stat_histogram.n_bins" = 40L,
    "artma.methods.t_stat_histogram.show_mean_line" = FALSE,
    "artma.methods.t_stat_histogram.show_density_curve" = FALSE,
    "artma.methods.t_stat_histogram.min_tick_distance" = 0.3,
    "artma.methods.t_stat_histogram.close_up_enabled" = FALSE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- data.frame(t_stat = c(-100, -2, 0, 1, 3, 100))
  result <- t_stat_histogram(df)

  expect_equal(result$n_outliers_main, 2L)
  expect_equal(result$n_observations, 6)
})


test_that("t_stat_histogram supports multiple critical values", {
  local_options(
    "artma.methods.t_stat_histogram.lower_cutoff" = -10,
    "artma.methods.t_stat_histogram.upper_cutoff" = 10,
    "artma.methods.t_stat_histogram.critical_values" = c(1.645, 1.96, 2.58),
    "artma.methods.t_stat_histogram.n_bins" = 40L,
    "artma.methods.t_stat_histogram.show_mean_line" = TRUE,
    "artma.methods.t_stat_histogram.show_density_curve" = FALSE,
    "artma.methods.t_stat_histogram.min_tick_distance" = 0.3,
    "artma.methods.t_stat_histogram.close_up_enabled" = FALSE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- create_test_data(n = 100)
  result <- t_stat_histogram(df)

  expect_true(ggplot2::is.ggplot(result$plot_main))
})


test_that("t_stat_histogram works without mean line and density", {
  local_options(
    "artma.methods.t_stat_histogram.lower_cutoff" = -120,
    "artma.methods.t_stat_histogram.upper_cutoff" = 120,
    "artma.methods.t_stat_histogram.critical_values" = 1.96,
    "artma.methods.t_stat_histogram.n_bins" = 80L,
    "artma.methods.t_stat_histogram.show_mean_line" = FALSE,
    "artma.methods.t_stat_histogram.show_density_curve" = FALSE,
    "artma.methods.t_stat_histogram.min_tick_distance" = 0.5,
    "artma.methods.t_stat_histogram.close_up_enabled" = FALSE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- create_test_data(n = 50)
  result <- t_stat_histogram(df)

  expect_true(ggplot2::is.ggplot(result$plot_main))
  expect_s3_class(result, "artma_t_stat_histogram")
})


test_that("t_stat_histogram reports correct mean", {
  local_options(
    "artma.methods.t_stat_histogram.lower_cutoff" = -120,
    "artma.methods.t_stat_histogram.upper_cutoff" = 120,
    "artma.methods.t_stat_histogram.critical_values" = 1.96,
    "artma.methods.t_stat_histogram.n_bins" = 40L,
    "artma.methods.t_stat_histogram.show_mean_line" = TRUE,
    "artma.methods.t_stat_histogram.show_density_curve" = FALSE,
    "artma.methods.t_stat_histogram.min_tick_distance" = 0.5,
    "artma.methods.t_stat_histogram.close_up_enabled" = FALSE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )

  df <- data.frame(t_stat = c(-2, 0, 2, 4))
  result <- t_stat_histogram(df)

  expect_equal(result$mean_t_stat, 1)
})
