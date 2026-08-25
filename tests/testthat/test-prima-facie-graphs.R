box::use(
  testthat[expect_equal, expect_setequal, expect_true, test_that],
  withr[local_options, local_tempdir]
)

box::use(
  artma / methods / prima_facie_graphs[prima_facie_graphs]
)

create_test_data <- function(n = 60, seed = 1) {
  set.seed(seed)
  data.frame(
    effect = rnorm(n, mean = 0.3, sd = 0.2),
    region_north = rep(c(1, 0), each = n / 2),
    region_south = rep(c(0, 1), each = n / 2),
    stringsAsFactors = FALSE
  )
}

test_that("prima_facie_graphs writes one PNG file per detected group when export is enabled", {
  dir <- local_tempdir()

  local_options(list(
    "artma.visualization.export_graphics" = TRUE,
    "artma.visualization.export_path" = dir,
    "artma.output.save_results" = FALSE,
    "artma.verbose" = 1,
    "artma.data.columns" = list()
  ))

  df <- create_test_data()
  prima_facie_graphs(df)

  expect_setequal(list.files(dir), "prima_facie_region.png")
})


test_that("prima_facie_graphs overlays group histograms instead of stacking them", {
  # Stacking densities adds quantities that each integrate to one, so the bars
  # climb to a total that means nothing and no group's own distribution can be
  # read off the chart.
  local_options(list(
    "artma.visualization.export_graphics" = FALSE,
    "artma.output.save_results" = FALSE,
    "artma.verbose" = 1,
    "artma.data.columns" = list(),
    "artma.methods.prima_facie_graphs.type" = "histogram"
  ))

  plot <- prima_facie_graphs(create_test_data())$plots[[1]]
  histogram_layers <- Filter(
    function(layer) inherits(layer$geom, "GeomBar"),
    plot$layers
  )

  expect_equal(length(histogram_layers), 1L)
  expect_true(inherits(histogram_layers[[1]]$position, "PositionIdentity"))
})


test_that("prima_facie_graphs keeps its legend outside the panel", {
  # An inside legend covers data whenever the mode falls under it.
  local_options(list(
    "artma.visualization.export_graphics" = FALSE,
    "artma.output.save_results" = FALSE,
    "artma.verbose" = 1,
    "artma.data.columns" = list()
  ))

  plot <- prima_facie_graphs(create_test_data())$plots[[1]]
  expect_equal(plot$theme$legend.position, "bottom")
})
