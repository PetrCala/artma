box::use(
  artma / methods / box_plot[box_plot]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")


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
    "artma.data.config" = list()
  ))

  result <- box_plot(df)

  expect_true(is.list(result))
  expect_equal(result$factor_by, "study_label")
})

