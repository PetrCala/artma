box::use(
  testthat[
    expect_gt,
    expect_true,
    skip_on_cran,
    test_that
  ],
  testing / mocks / index[MOCKS],
  withr[local_options, local_tempdir]
)

# End-to-end: run two cheap methods on a tiny fixture dataset with the
# output.report option enabled, and assert a non-trivial report.html lands in
# the output directory. Exercises the wiring in R/artma.R that renders the
# report at the end of a run.
test_that("artma() writes report.html when output.report is enabled", {
  skip_on_cran()

  work <- local_tempdir()
  data_path <- file.path(work, "meta.csv")
  utils::write.csv(
    data.frame(
      study_id = rep(1:5, each = 4),
      effect = stats::rnorm(20, 0.4, 0.2),
      se = stats::runif(20, 0.05, 0.2),
      n_obs = sample(50:400, 20, replace = TRUE)
    ),
    data_path,
    row.names = FALSE
  )

  options_dir <- file.path(work, "options")
  dir.create(options_dir)
  output_dir <- file.path(work, "out")
  dir.create(output_dir)

  artma::options.create(
    options_file_name = "report_it.yaml",
    options_dir = options_dir,
    user_input = list(
      data = list(source_path = data_path),
      general = list(parallel = FALSE),
      output = list(dir = output_dir, save_results = TRUE, report = TRUE)
    )
  )

  local_options(artma.verbose = 1)

  result <- artma::artma(
    methods = c("funnel_plot", "effect_summary_stats"),
    options = "report_it.yaml",
    options_dir = options_dir
  )

  report_path <- file.path(output_dir, "report.html")
  expect_true(file.exists(report_path))

  contents <- paste(readLines(report_path, warn = FALSE), collapse = "\n")
  expect_gt(nchar(contents), 1000)
  expect_true(grepl("Effect Summary Stats", contents) || grepl("Funnel Plot", contents))
  # Plots the run wrote must be embedded, never linked.
  expect_true(grepl("data:image/png;base64,", contents, fixed = TRUE))
})

# Golden end-to-end: a realistic method set on mock data must produce a report
# with one section per method. `linear_tests` is the important one: it records
# partial skips as a named list, and the renderer used to abort on that shape,
# which only surfaced as a warning in the middle of a long run (issue #414).
test_that("artma() renders one report section per method for a real method set", {
  skip_on_cran()

  work <- local_tempdir()
  data_path <- file.path(work, "mock.csv")
  MOCKS$create_mock_df(with_file_creation = TRUE, file_path = data_path, seed = 414)

  options_dir <- file.path(work, "options")
  dir.create(options_dir)
  output_dir <- file.path(work, "out")
  dir.create(output_dir)

  artma::options.create(
    options_file_name = "golden_report.yaml",
    options_dir = options_dir,
    user_input = list(
      data = list(source_path = data_path),
      general = list(parallel = FALSE),
      output = list(dir = output_dir, save_results = TRUE, report = TRUE)
    )
  )

  local_options(artma.verbose = 1)

  methods <- c("effect_summary_stats", "linear_tests", "funnel_plot")
  suppressWarnings(artma::artma(
    methods = methods,
    options = "golden_report.yaml",
    options_dir = options_dir
  ))

  report_path <- file.path(output_dir, "report.html")
  expect_true(file.exists(report_path))

  contents <- paste(readLines(report_path, warn = FALSE), collapse = "\n")
  expect_gt(nchar(contents), 1000)

  for (method_name in methods) {
    anchor <- paste0("id=\"section-", gsub("_", "-", method_name), "\"")
    expect_true(grepl(anchor, contents, fixed = TRUE))
  }

  # linear_tests ran, so its section must carry its results rather than a
  # blanket "this method was skipped" body.
  linear_section <- sub(
    ".*id=\"section-linear-tests\"", "", contents
  )
  linear_section <- sub("id=\"section-.*", "", linear_section)
  expect_true(!grepl("This method was skipped", linear_section, fixed = TRUE))
})
