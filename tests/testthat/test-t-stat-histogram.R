box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_named,
    expect_null,
    expect_s3_class,
    expect_setequal,
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


# Set the t_stat_histogram + visualization options a test needs, overriding only
# the fields that matter to it. Defaults draw a single main plot (close-up
# disabled) over a wide cutoff, the blue theme, and no graphics export. Restores
# the previous options when the calling test exits.
local_hist_options <- function(..., .env = parent.frame()) {
  defaults <- list(
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
  local_options(utils::modifyList(defaults, list(...)), .local_envir = .env)
}


test_that("t_stat_histogram creates both plots with defaults", {
  local_hist_options(
    "artma.methods.t_stat_histogram.n_bins" = 80L,
    "artma.methods.t_stat_histogram.show_density_curve" = TRUE,
    "artma.methods.t_stat_histogram.close_up_enabled" = TRUE,
    "artma.methods.t_stat_histogram.close_up_lower" = -10,
    "artma.methods.t_stat_histogram.close_up_upper" = 10,
    "artma.methods.t_stat_histogram.close_up_min_tick_distance" = 0.3
  )

  df <- create_test_data()
  result <- t_stat_histogram(df)

  expect_s3_class(result, "artma_t_stat_histogram")
  expect_named(result, c("tables", "estimates", "plots", "meta"))
  expect_named(result$plots, c("plot_main", "plot_close_up"))
  expect_named(result$meta, c(
    "n_observations", "n_outliers_main", "n_outliers_close_up",
    "mean_t_stat", "close_up_enabled"
  ))
  expect_true(ggplot2::is_ggplot(result$plots$plot_main))
  expect_true(ggplot2::is_ggplot(result$plots$plot_close_up))
  expect_identical(result$meta$close_up_enabled, TRUE)
  expect_equal(result$meta$n_observations, 200)
})


test_that("t_stat_histogram works without close-up", {
  local_hist_options(
    "artma.methods.t_stat_histogram.n_bins" = 80L,
    "artma.methods.t_stat_histogram.show_density_curve" = TRUE
  )

  df <- create_test_data()
  result <- t_stat_histogram(df)

  expect_true(ggplot2::is_ggplot(result$plots$plot_main))
  expect_null(result$plots$plot_close_up)
  expect_identical(result$meta$close_up_enabled, FALSE)
})


test_that("t_stat_histogram handles all themes", {
  themes <- c("blue", "yellow", "green", "red", "purple")

  for (theme in themes) {
    local_hist_options(
      "artma.methods.t_stat_histogram.lower_cutoff" = -50,
      "artma.methods.t_stat_histogram.upper_cutoff" = 50,
      "artma.methods.t_stat_histogram.show_density_curve" = TRUE,
      "artma.visualization.theme" = theme
    )

    df <- create_test_data(n = 50)
    result <- t_stat_histogram(df)

    expect_true(ggplot2::is_ggplot(result$plots$plot_main))
    # Theme choice must not change which observations are plotted.
    expect_equal(result$meta$n_observations, 50)
  }
})


test_that("t_stat_histogram filters outliers by cutoff", {
  local_hist_options(
    "artma.methods.t_stat_histogram.lower_cutoff" = -5,
    "artma.methods.t_stat_histogram.upper_cutoff" = 5,
    "artma.methods.t_stat_histogram.show_mean_line" = FALSE,
    "artma.methods.t_stat_histogram.min_tick_distance" = 0.3
  )

  df <- data.frame(t_stat = c(-100, -2, 0, 1, 3, 100))
  result <- t_stat_histogram(df)

  expect_equal(result$meta$n_outliers_main, 2L)
  expect_equal(result$meta$n_observations, 6)
})


test_that("t_stat_histogram supports multiple critical values", {
  local_hist_options(
    "artma.methods.t_stat_histogram.lower_cutoff" = -10,
    "artma.methods.t_stat_histogram.upper_cutoff" = 10,
    "artma.methods.t_stat_histogram.critical_values" = c(1.645, 1.96, 2.58),
    "artma.methods.t_stat_histogram.min_tick_distance" = 0.3
  )

  df <- create_test_data(n = 100)
  result <- t_stat_histogram(df)

  expect_true(ggplot2::is_ggplot(result$plots$plot_main))
  # All 100 observations fall inside the cutoff, so none are dropped.
  expect_equal(result$meta$n_observations, 100)
  expect_equal(result$meta$n_outliers_main, 0L)
})


test_that("t_stat_histogram works without mean line and density", {
  local_hist_options(
    "artma.methods.t_stat_histogram.n_bins" = 80L,
    "artma.methods.t_stat_histogram.show_mean_line" = FALSE
  )

  df <- create_test_data(n = 50)
  result <- t_stat_histogram(df)

  expect_true(ggplot2::is_ggplot(result$plots$plot_main))
  expect_s3_class(result, "artma_t_stat_histogram")
  expect_equal(result$meta$n_observations, 50)
})


test_that("t_stat_histogram reports correct mean", {
  local_hist_options()

  df <- data.frame(t_stat = c(-2, 0, 2, 4))
  result <- t_stat_histogram(df)

  expect_equal(result$meta$mean_t_stat, 1)
})


test_that("t_stat_histogram writes both PNG files when export and close-up are enabled", {
  dir <- withr::local_tempdir()
  local_hist_options(
    "artma.methods.t_stat_histogram.close_up_enabled" = TRUE,
    "artma.methods.t_stat_histogram.close_up_lower" = -10,
    "artma.methods.t_stat_histogram.close_up_upper" = 10,
    "artma.methods.t_stat_histogram.close_up_min_tick_distance" = 0.3,
    "artma.visualization.export_graphics" = TRUE,
    "artma.visualization.export_path" = dir,
    "artma.output.save_results" = FALSE
  )

  df <- create_test_data()
  t_stat_histogram(df)

  expect_setequal(
    list.files(dir),
    c("t_stat_histogram_full_range.png", "t_stat_histogram_close_up.png")
  )
})


test_that("t_stat_histogram writes only the full-range PNG when close-up is disabled", {
  dir <- withr::local_tempdir()
  local_hist_options(
    "artma.methods.t_stat_histogram.close_up_enabled" = FALSE,
    "artma.visualization.export_graphics" = TRUE,
    "artma.visualization.export_path" = dir,
    "artma.output.save_results" = FALSE
  )

  df <- create_test_data()
  t_stat_histogram(df)

  expect_setequal(list.files(dir), "t_stat_histogram_full_range.png")
})


# Same regression as the funnel plot: tick labels were HTML color spans that
# nothing rendered, so the axis printed the markup verbatim.
test_that("t_stat_histogram renders plain x-axis labels, not HTML markup", {
  local_hist_options()

  plot <- t_stat_histogram(create_test_data())$plots$plot_main
  labels <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x$get_labels()

  expect_true(length(labels) > 0)
  expect_false(any(grepl("<|span|style=", labels)))
})


test_that("t_stat_histogram keeps its ticks apart and keeps the critical values", {
  local_hist_options("artma.methods.t_stat_histogram.min_tick_distance" = 0.5)

  plot <- t_stat_histogram(create_test_data())$plots$plot_main
  built <- ggplot2::ggplot_build(plot)
  breaks <- built$layout$panel_params[[1]]$x$get_breaks()
  breaks <- sort(breaks[!is.na(breaks)])

  expect_true(all(diff(breaks) >= 0.5))
  # The critical values are why the plot exists; they must survive thinning.
  expect_true(all(c(-1.96, 1.96) %in% round(breaks, 2)))

  # One colour for every tick label: the per-tick colouring that stood in for a
  # legend is gone, and with it the vectorised element_text() warning.
  expect_equal(length(built$plot$theme$axis.text.x$colour), 1L)
})


test_that("t_stat_histogram shades the significant region and names the mean", {
  local_hist_options()

  plot <- t_stat_histogram(create_test_data())$plots$plot_main

  band_layers <- Filter(
    function(layer) is.data.frame(layer$data) && "band" %in% names(layer$data),
    plot$layers
  )
  expect_equal(length(band_layers), 1L)
  # One band per tail, bounded by the loosest critical value.
  expect_equal(nrow(band_layers[[1]]$data), 2L)

  mean_labels <- unlist(lapply(plot$layers, function(layer) {
    data <- layer$data
    if (is.data.frame(data) && "label" %in% names(data)) unique(data$label) else NULL
  }))
  expect_true("Mean t-statistic" %in% mean_labels)
})
