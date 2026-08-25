box::use(
  testthat[expect_equal, expect_false, expect_setequal, expect_true, test_that]
)

box::use(
  artma / methods / box_plot[box_plot]
)


test_that("box_plot auto-selects study_label over study_id for grouping", {
  df <- data.frame(
    study_id = c(1L, 2L, 1L, 3L),
    study_label = c("Albeigh (2008)", "Baker (2009)", "Albeigh (2008)", "Clark (2010)"),
    effect = c(0.2, 0.3, 0.1, 0.4),
    se = c(0.1, 0.1, 0.2, 0.1),
    n_obs = c(100, 120, 100, 140),
    stringsAsFactors = FALSE
  )

  withr::local_options(list(
    "artma.visualization.export_graphics" = FALSE,
    "artma.output.save_results" = FALSE,
    "artma.verbose" = 1,
    "artma.data.columns" = list()
  ))

  result <- box_plot(df)

  expect_true(is.list(result))
  expect_equal(result$meta$factor_by, "study_label")
})

test_that("box_plot builds a plot even when the data has duplicate column names", {
  df <- data.frame(
    study_id = c(1L, 2L, 1L, 3L),
    study_label = c("Albeigh (2008)", "Baker (2009)", "Albeigh (2008)", "Clark (2010)"),
    effect = c(0.2, 0.3, 0.1, 0.4),
    se = c(0.1, 0.1, 0.2, 0.1),
    n_obs = c(100, 120, 100, 140),
    stringsAsFactors = FALSE
  )
  # A stray duplicate column used to reach ggplot2 and abort the whole plot with
  # "`data` must be uniquely named but has duplicate columns".
  df$extra <- df$se
  names(df)[names(df) == "extra"] <- "se"

  withr::local_options(list(
    "artma.visualization.export_graphics" = FALSE,
    "artma.output.save_results" = FALSE,
    "artma.verbose" = 1,
    "artma.data.columns" = list()
  ))

  result <- box_plot(df)

  expect_equal(result$meta$factor_by, "study_label")
  expect_true(inherits(result$plots[[1]], "ggplot"))
  expect_true(is.data.frame(ggplot2::ggplot_build(result$plots[[1]])$data[[1]]))
})

test_that("box_plot writes exactly one PNG file when export is enabled", {
  df <- data.frame(
    study_id = c(1L, 2L, 1L, 3L),
    study_label = c("Albeigh (2008)", "Baker (2009)", "Albeigh (2008)", "Clark (2010)"),
    effect = c(0.2, 0.3, 0.1, 0.4),
    se = c(0.1, 0.1, 0.2, 0.1),
    n_obs = c(100, 120, 100, 140),
    stringsAsFactors = FALSE
  )
  dir <- withr::local_tempdir()

  withr::local_options(list(
    "artma.visualization.export_graphics" = TRUE,
    "artma.visualization.export_path" = dir,
    "artma.output.save_results" = FALSE,
    "artma.verbose" = 1,
    "artma.data.columns" = list()
  ))

  box_plot(df)

  expect_setequal(list.files(dir), "box_plot_study_label.png")
})


local_box_plot_options <- function(.env = parent.frame()) {
  withr::local_options(
    list(
      "artma.visualization.export_graphics" = FALSE,
      "artma.output.save_results" = FALSE,
      "artma.verbose" = 1,
      "artma.data.columns" = list()
    ),
    .local_envir = .env
  )
}


test_that("box_plot orders groups by median rather than alphabetically", {
  # Study labels carry no order of their own, so an alphabetical axis scatters
  # the distribution at random and hides which studies sit high or low.
  df <- data.frame(
    study_label = rep(c("Aaron (2001)", "Baker (2002)", "Clark (2003)"), each = 3),
    effect = c(0.9, 1.0, 1.1, -0.5, -0.4, -0.3, 0.2, 0.3, 0.4),
    stringsAsFactors = FALSE
  )
  local_box_plot_options()

  plot <- box_plot(df)$plots[[1]]
  levels_in_order <- levels(plot$data$.factor)

  # Ascending by median; coord_flip() then puts the largest at the top.
  expect_equal(levels_in_order, c("Baker (2002)", "Clark (2003)", "Aaron (2001)"))
})


test_that("box_plot does not label the axis 'Effect of effect'", {
  df <- data.frame(
    study_label = rep(c("A (2001)", "B (2002)"), each = 3),
    effect = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
    stringsAsFactors = FALSE
  )
  local_box_plot_options()

  plot <- box_plot(df)$plots[[1]]
  expect_equal(plot$labels$y, "Effect")
  expect_equal(plot$labels$x, "Study label")
})


test_that("box_plot marks zero when the effects straddle it", {
  straddling <- data.frame(
    study_label = rep(c("A (2001)", "B (2002)"), each = 3),
    effect = c(-0.3, -0.2, -0.1, 0.1, 0.2, 0.3),
    stringsAsFactors = FALSE
  )
  local_box_plot_options()

  hline_intercepts <- function(plot) {
    unlist(lapply(plot$layers, function(layer) {
      if (inherits(layer$geom, "GeomHline")) layer$data$yintercept else NULL
    }))
  }

  expect_true(0 %in% hline_intercepts(box_plot(straddling)$plots[[1]]))

  one_sided <- straddling
  one_sided$effect <- abs(one_sided$effect) + 1
  expect_false(0 %in% hline_intercepts(box_plot(one_sided)$plots[[1]]))
})
