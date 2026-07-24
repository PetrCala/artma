box::use(
  testthat[expect_setequal, test_that],
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
