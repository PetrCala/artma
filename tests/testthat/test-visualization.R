box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_message,
    expect_null,
    expect_true,
    test_that
  ],
  withr[local_options, local_tempdir],
  artma / visualization / colors[
    VALID_THEMES,
    validate_theme,
    get_colors,
    get_background,
    get_vline_color
  ],
  artma / visualization / theme[get_theme],
  artma / visualization / export[
    ensure_export_dir,
    build_export_filename,
    save_plot,
    save_plot_html
  ],
  artma / visualization / options[
    get_visualization_options,
    set_visualization_option,
    get_valid_themes
  ]
)

# colors --------------------------------------------------------------------

test_that("validate_theme accepts known themes and rejects unknown ones", {
  expect_true(validate_theme("blue"))
  expect_error(validate_theme("chartreuse"))
  expect_error(validate_theme(c("blue", "red")))
})

test_that("get_colors returns the exact box_plot palette", {
  expect_equal(
    get_colors("blue", "box_plot"),
    list(outlier = "#005CAB", fill = "#e6f3ff", border = "#0d4ed1")
  )
})

test_that("get_colors returns a single hex for the funnel palette", {
  expect_equal(get_colors("red", "funnel_plot"), "#FF0000")
})

test_that("get_colors resolves submethods", {
  expect_equal(get_colors("blue", "t_stat_histogram", "density"), "#013091")
})

test_that("get_colors aborts on unknown method or submethod", {
  expect_error(get_colors("blue", "no_such_method"))
  expect_error(get_colors("blue", "t_stat_histogram", "no_such_submethod"))
})

test_that("get_background and get_vline_color return theme hex codes", {
  expect_equal(get_background("yellow"), "#FFFFD1")
  expect_equal(get_vline_color("blue"), "#D10D0D")
})

test_that("VALID_THEMES lists the five supported themes", {
  expect_equal(VALID_THEMES, c("blue", "yellow", "green", "red", "purple"))
})

# theme ---------------------------------------------------------------------

test_that("get_theme returns a ggplot2 theme object", {
  th <- get_theme("green")
  expect_true(inherits(th, "theme"))
  expect_true(inherits(th, "gg"))
})

test_that("get_theme rejects invalid themes", {
  expect_error(get_theme("mauve"))
})

# export --------------------------------------------------------------------

test_that("build_export_filename follows the documented naming scheme", {
  expect_equal(build_export_filename("box_plot", "country"), "box_plot_country.png")
  expect_equal(build_export_filename("box_plot", "country", index = 2), "box_plot_country_2.png")
  expect_equal(build_export_filename("funnel", "year", extension = "pdf"), "funnel_year.pdf")
})

test_that("ensure_export_dir creates the directory", {
  base <- local_tempdir()
  target <- file.path(base, "nested", "graphics")
  expect_false(dir.exists(target))
  ensure_export_dir(target)
  expect_true(dir.exists(target))
})

test_that("save_plot writes a ggplot to disk and returns the path", {
  local_options(artma.verbose = 1)
  dir <- local_tempdir()
  path <- file.path(dir, "plot.png")
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  returned <- save_plot(plot, path, width = 200, height = 200)

  expect_equal(returned, path)
  expect_true(file.exists(path))
  expect_true(file.info(path)$size > 0)
})

test_that("save_plot rejects non-ggplot input", {
  dir <- local_tempdir()
  expect_error(save_plot("not a plot", file.path(dir, "x.png")))
})

test_that("save_plot does not write HTML when export_html is off", {
  local_options(artma.verbose = 1, artma.visualization.export_html = FALSE)
  dir <- local_tempdir()
  path <- file.path(dir, "plot.png")
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  save_plot(plot, path, width = 200, height = 200)

  expect_false(file.exists(file.path(dir, "plot.html")))
})

test_that("save_plot additionally writes an HTML widget when export_html is on", {
  testthat::skip_if_not_installed("plotly")
  testthat::skip_if_not_installed("htmlwidgets")
  # plotly::ggplotly() and htmlwidgets::saveWidget() are mocked so this test
  # stays deterministic across plotly/ggplot2 version combinations.
  testthat::local_mocked_bindings(ggplotly = function(p, ...) list(fake_widget = TRUE), .package = "plotly")
  testthat::local_mocked_bindings(
    saveWidget = function(widget, file, ...) writeLines("<html></html>", file),
    .package = "htmlwidgets"
  )
  local_options(artma.verbose = 1, artma.visualization.export_html = TRUE)
  dir <- local_tempdir()
  path <- file.path(dir, "plot.png")
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  save_plot(plot, path, width = 200, height = 200)

  html_path <- file.path(dir, "plot.html")
  expect_true(file.exists(html_path))
  expect_true(file.info(html_path)$size > 0)
})

test_that("save_plot_html writes a standalone HTML file and returns its path", {
  testthat::skip_if_not_installed("plotly")
  testthat::skip_if_not_installed("htmlwidgets")
  testthat::local_mocked_bindings(ggplotly = function(p, ...) list(fake_widget = TRUE), .package = "plotly")
  testthat::local_mocked_bindings(
    saveWidget = function(widget, file, ...) writeLines("<html></html>", file),
    .package = "htmlwidgets"
  )
  dir <- local_tempdir()
  path <- file.path(dir, "widget.html")
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  returned <- save_plot_html(plot, path)

  expect_equal(returned, path)
  expect_true(file.exists(path))
})

test_that("save_plot_html skips with a warning when plotly or htmlwidgets are missing", {
  local_options(artma.verbose = 3)
  local_pretend_packages_absent("plotly")
  dir <- local_tempdir()
  path <- file.path(dir, "widget.html")
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  expect_message(
    result <- save_plot_html(plot, path),
    "plotly"
  )
  expect_null(result)
  expect_false(file.exists(path))
})

# options -------------------------------------------------------------------

test_that("get_visualization_options returns defaults", {
  local_options(
    artma.visualization.theme = NULL,
    artma.visualization.export_graphics = NULL,
    artma.visualization.export_html = NULL,
    artma.visualization.export_path = NULL,
    artma.visualization.graph_scale = NULL,
    artma.output.save_results = FALSE
  )

  vis <- get_visualization_options()
  expect_equal(vis$theme, "blue")
  expect_true(vis$export_graphics)
  expect_false(vis$export_html)
  expect_equal(vis$export_path, "graphics")
  expect_equal(vis$graph_scale, 2)
})

test_that("set_visualization_option updates a theme and reports the previous value", {
  local_options(
    artma.visualization.theme = "blue",
    artma.output.save_results = FALSE
  )

  previous <- set_visualization_option(theme = "red")
  expect_equal(previous$theme, "blue")
  expect_equal(getOption("artma.visualization.theme"), "red")
})

test_that("set_visualization_option updates export_html and reports the previous value", {
  local_options(
    artma.visualization.export_html = FALSE,
    artma.output.save_results = FALSE
  )

  previous <- set_visualization_option(export_html = TRUE)
  expect_false(previous$export_html)
  expect_true(getOption("artma.visualization.export_html"))
})

test_that("set_visualization_option rejects a non-logical export_html", {
  local_options(artma.output.save_results = FALSE)
  expect_error(set_visualization_option(export_html = "yes"))
})

test_that("set_visualization_option rejects an invalid theme", {
  local_options(artma.output.save_results = FALSE)
  expect_error(set_visualization_option(theme = "octarine"))
})

test_that("get_valid_themes returns the theme vector", {
  expect_equal(get_valid_themes(), c("blue", "yellow", "green", "red", "purple"))
})
