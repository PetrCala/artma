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


# Set the funnel_plot + visualization options a test needs, overriding only the
# fields that matter to it. The defaults disable filtering (proximity/precision
# = 1), use raw points, the blue theme, and no graphics export. Restores the
# previous options when the calling test exits.
local_funnel_options <- function(..., .env = parent.frame()) {
  defaults <- list(
    "artma.methods.funnel_plot.effect_proximity" = 1,
    "artma.methods.funnel_plot.maximum_precision" = 1,
    "artma.methods.funnel_plot.precision_to_log" = FALSE,
    "artma.methods.funnel_plot.use_study_medians" = FALSE,
    "artma.methods.funnel_plot.add_zero" = TRUE,
    "artma.visualization.theme" = "blue",
    "artma.visualization.export_graphics" = FALSE,
    "artma.verbose" = 1
  )
  local_options(utils::modifyList(defaults, list(...)), .local_envir = .env)
}


test_that("funnel_plot creates a plot with required columns", {
  local_funnel_options(
    "artma.methods.funnel_plot.effect_proximity" = 0.2,
    "artma.methods.funnel_plot.maximum_precision" = 0.2
  )

  df <- create_test_data()
  result <- funnel_plot(df)

  expect_s3_class(result, "artma_funnel_plot")
  expect_named(result, c("tables", "estimates", "plots", "meta"))
  expect_named(result$plots, "funnel_plot")
  expect_named(
    result$meta,
    c("n_points", "n_outliers_removed", "pct_filtered", "used_study_medians"),
    ignore.order = TRUE
  )
  expect_true(ggplot2::is_ggplot(result$plots$funnel_plot))
  expect_identical(result$meta$used_study_medians, FALSE)
})


test_that("funnel_plot respects use_study_medians option", {
  local_funnel_options(
    "artma.methods.funnel_plot.use_study_medians" = TRUE
  )

  df <- create_test_data(n = 100)
  result <- funnel_plot(df)

  expect_identical(result$meta$used_study_medians, TRUE)
  expect_equal(result$meta$n_points, 10)
})


test_that("funnel_plot filters outliers correctly", {
  local_funnel_options(
    "artma.methods.funnel_plot.effect_proximity" = 0.1,
    "artma.methods.funnel_plot.maximum_precision" = 0.1
  )

  df <- data.frame(
    effect = c(0, 0, 0, 10),
    precision = c(10, 10, 10, 100),
    study_id = 1:4
  )

  result <- funnel_plot(df)

  expect_true(result$meta$n_outliers_removed > 0)
  expect_true(result$meta$n_points < nrow(df))
  expect_true(result$meta$pct_filtered > 0)
})


test_that("funnel_plot reports filtered observation count in the plot subtitle", {
  local_funnel_options(
    "artma.methods.funnel_plot.effect_proximity" = 0.1,
    "artma.methods.funnel_plot.maximum_precision" = 0.1
  )

  df <- data.frame(
    effect = c(0, 0, 0, 0, 10),
    precision = c(1, 1, 1, 1, 100),
    study_id = 1:5
  )

  result <- funnel_plot(df)

  expect_true(result$meta$n_outliers_removed > 0)
  expect_true(result$meta$n_points > 0)
  expect_equal(
    result$meta$pct_filtered,
    round(100 * result$meta$n_outliers_removed / nrow(df))
  )
  expect_true(grepl("filtered as outliers", result$plots$funnel_plot$labels$subtitle))
})


test_that("funnel_plot with no outlier filtering keeps all points", {
  local_funnel_options()

  df <- create_test_data(n = 50)
  result <- funnel_plot(df)

  expect_equal(result$meta$n_outliers_removed, 0)
  expect_equal(result$meta$n_points, 50)
  expect_equal(result$meta$pct_filtered, 0)
  expect_null(result$plots$funnel_plot$labels$subtitle)
})


test_that("funnel_plot handles different themes", {
  themes <- c("blue", "yellow", "green", "red", "purple")

  for (theme in themes) {
    local_funnel_options("artma.visualization.theme" = theme)

    df <- create_test_data(n = 20)
    result <- funnel_plot(df)

    expect_true(ggplot2::is_ggplot(result$plots$funnel_plot))
    # Theme choice must not drop points from the plotted data.
    expect_equal(result$meta$n_points, 20)
  }
})


test_that("funnel_plot handles precision_to_log option", {
  local_funnel_options("artma.methods.funnel_plot.precision_to_log" = TRUE)

  df <- create_test_data(n = 20)
  result <- funnel_plot(df)

  expect_true(ggplot2::is_ggplot(result$plots$funnel_plot))
  expect_s3_class(result, "artma_funnel_plot")
  # Log-transforming precision keeps every point (no filtering configured).
  expect_equal(result$meta$n_points, 20)
})


test_that("funnel_plot writes exactly one PNG file when export is enabled", {
  dir <- withr::local_tempdir()
  local_funnel_options(
    "artma.visualization.export_graphics" = TRUE,
    "artma.visualization.export_path" = dir,
    "artma.output.save_results" = FALSE
  )

  df <- create_test_data()
  funnel_plot(df)

  expect_setequal(list.files(dir), "funnel_plot_effect_precision.png")
})


test_that("funnel_plot returns empty result when all data filtered", {
  local_funnel_options(
    "artma.methods.funnel_plot.effect_proximity" = 0,
    "artma.methods.funnel_plot.maximum_precision" = 0
  )

  df <- data.frame(
    effect = c(10, -10),
    precision = c(100, 100),
    study_id = 1:2
  )

  result <- funnel_plot(df)

  expect_null(result$plots$funnel_plot)
  expect_equal(result$meta$n_points, 0)
})


# The x-axis labels used to be HTML color spans rendered by ggtext. After
# ggtext was dropped the markup was still emitted and printed literally, so the
# axis read `<span style='color:black'>50</span>`. Assert on the built plot,
# which is what the user actually sees.
test_that("funnel_plot renders plain x-axis labels, not HTML markup", {
  local_funnel_options()

  df <- create_test_data()
  plot <- funnel_plot(df)$plots$funnel_plot
  labels <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x$get_labels()

  expect_true(length(labels) > 0)
  expect_false(any(grepl("<|span|style=", labels)))
})


# Collect the mapped reference-line labels from every layer that carries them.
layer_labels <- function(plot) {
  unlist(lapply(plot$layers, function(layer) {
    data <- layer$data
    if (is.data.frame(data) && "label" %in% names(data)) unique(data$label) else NULL
  }))
}


test_that("funnel_plot names its reference lines in a legend", {
  local_funnel_options()

  df <- create_test_data()
  plot <- funnel_plot(df)$plots$funnel_plot
  built <- ggplot2::ggplot_build(plot)
  breaks <- built$layout$panel_params[[1]]$x$get_breaks()
  breaks <- breaks[!is.na(breaks)]

  # The mean still earns its own tick, it is just no longer tinted to say so.
  mean_effect <- mean(df$effect)
  expect_equal(breaks[which.min(abs(breaks - mean_effect))], mean_effect)

  expect_true("Mean effect" %in% layer_labels(plot))

  # One colour for every tick label: the vectorised per-tick colouring is gone,
  # and with it the "Vectorized input to element_text()" warning it raised.
  expect_equal(length(built$plot$theme$axis.text.x$colour), 1L)
})


test_that("funnel_plot draws pseudo-confidence contours when precision is 1/SE", {
  local_funnel_options("artma.calc.precision_type" = "1/SE")

  plot <- funnel_plot(create_test_data())$plots$funnel_plot
  contour_layers <- Filter(
    function(layer) is.data.frame(layer$data) && "side" %in% names(layer$data),
    plot$layers
  )

  expect_equal(length(contour_layers), 1L)
  expect_setequal(unique(contour_layers[[1]]$data$side), c("lower", "upper"))
})


test_that("funnel_plot omits the contours when precision is not 1/SE", {
  # Under the DoF precision type precision is sqrt(reg_dof), which carries no
  # standard-error relationship, so mean +/- 1.96 * SE is undefined.
  local_funnel_options("artma.calc.precision_type" = "DoF")

  plot <- funnel_plot(create_test_data())$plots$funnel_plot
  contour_layers <- Filter(
    function(layer) is.data.frame(layer$data) && "side" %in% names(layer$data),
    plot$layers
  )

  expect_equal(length(contour_layers), 0L)
})


test_that("funnel_plot drops the contours when show_contours is off", {
  local_funnel_options(
    "artma.calc.precision_type" = "1/SE",
    "artma.methods.funnel_plot.show_contours" = FALSE
  )

  plot <- funnel_plot(create_test_data())$plots$funnel_plot
  contour_layers <- Filter(
    function(layer) is.data.frame(layer$data) && "side" %in% names(layer$data),
    plot$layers
  )

  expect_equal(length(contour_layers), 0L)
})


# A wide range used to pin the exact data minimum, maximum and mean on top of a
# fixed-interval grid, so labels such as 350 and 369.54 overlapped.
test_that("funnel_plot keeps x-axis ticks far enough apart to be readable", {
  local_funnel_options()

  set.seed(7)
  df <- data.frame(
    effect = c(runif(200, 1, 369.54), 1, 369.54),
    precision = runif(202, 5, 50)
  )

  plot <- funnel_plot(df)$plots$funnel_plot
  breaks <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x$get_breaks()
  breaks <- sort(breaks[!is.na(breaks)])

  range_size <- max(df$effect) - min(df$effect)
  expect_true(all(diff(breaks) >= range_size / 14))
  expect_true(length(breaks) <= 14)
})
