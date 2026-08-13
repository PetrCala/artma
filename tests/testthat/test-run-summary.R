box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_null,
    expect_true,
    test_that
  ],
  withr[local_options, local_tempdir],
  artma / output / run_summary[render_run_summary, summarise_run]
)

# Build a results list shaped like the one invoke_runtime_methods() returns.
make_results <- function(output_files = list(),
                         requested = c("funnel_plot", "linear_tests", "bma"),
                         ran = c("funnel_plot", "linear_tests")) {
  results <- stats::setNames(
    lapply(ran, function(name) list(tables = list(), plots = list(), meta = list())),
    ran
  )
  attr(results, "skipped_methods") <- c(bma = "missing suggested package: BMS")
  attr(results, "run_info") <- list(
    methods_requested = requested,
    seed = 4242L,
    output_files = output_files
  )
  results
}

# summarise_run ---------------------------------------------------------------

test_that("summarise_run reports what ran, what was skipped and what failed", {
  results <- make_results()
  attr(results, "failed_methods") <- c(maive = "singular first stage")

  run <- summarise_run(results)

  expect_equal(run$ran, c("funnel_plot", "linear_tests"))
  expect_equal(run$requested, c("funnel_plot", "linear_tests", "bma"))
  expect_equal(unname(run$skipped["bma"]), "missing suggested package: BMS")
  expect_equal(unname(run$failed["maive"]), "singular first stage")
})

test_that("summarise_run leaves the assembled ma_table out of the methods that ran", {
  results <- make_results(ran = c("bma", "fma"))
  results[["ma_table"]] <- list(tables = list(), plots = list(), meta = list())

  expect_equal(summarise_run(results)$ran, c("bma", "fma"))
})

test_that("summarise_run counts the tables and plots the run wrote", {
  out <- local_tempdir()
  dir.create(file.path(out, "tables"))
  dir.create(file.path(out, "graphics"))
  local_options(artma.visualization.export_path = "graphics")

  results <- make_results(
    output_files = list(funnel_plot = file.path(out, "graphics", "funnel_plot.png"))
  )

  run <- summarise_run(
    results,
    output_dir = out,
    run_files = c(
      file.path(out, "tables", "funnel_plot.csv"),
      file.path(out, "tables", "linear_tests.csv"),
      file.path(out, "report.html")
    )
  )

  expect_equal(run$tables, 2L)
  expect_equal(run$graphics, 1L)
  expect_equal(run$other, 1L)
  expect_equal(run$output_dir, out)
})

test_that("summarise_run reports no output directory when results are not saved", {
  run <- summarise_run(make_results())

  expect_null(run$output_dir)
  expect_equal(run$tables, 0L)
  expect_equal(run$graphics, 0L)
})

# render_run_summary ----------------------------------------------------------

test_that("render_run_summary prints on a clean run", {
  local_options(artma.verbose = 3)
  results <- make_results(requested = c("funnel_plot", "linear_tests"))
  attr(results, "skipped_methods") <- NULL

  msgs <- paste(testthat::capture_messages(render_run_summary(results)), collapse = "")

  expect_true(grepl("Run summary", msgs, fixed = TRUE))
  expect_true(grepl("Ran 2 of 2 requested methods", msgs, fixed = TRUE))
})

test_that("render_run_summary names the skipped and failed methods with their reasons", {
  local_options(artma.verbose = 3)
  results <- make_results()
  attr(results, "failed_methods") <- c(maive = "singular first stage")

  msgs <- paste(testthat::capture_messages(render_run_summary(results)), collapse = "")

  expect_true(grepl("Skipped", msgs, fixed = TRUE))
  expect_true(grepl("missing suggested package: BMS", msgs, fixed = TRUE))
  expect_true(grepl("Failed", msgs, fixed = TRUE))
  expect_true(grepl("singular first stage", msgs, fixed = TRUE))
})

test_that("render_run_summary reports the file counts and the output directory", {
  out <- local_tempdir()
  dir.create(file.path(out, "tables"))
  dir.create(file.path(out, "graphics"))
  local_options(artma.verbose = 3, cli.width = 400, artma.visualization.export_path = "graphics")

  msgs <- paste(
    testthat::capture_messages(render_run_summary(
      make_results(output_files = list(funnel_plot = file.path(out, "graphics", "funnel_plot.png"))),
      output_dir = out,
      run_files = c(
        file.path(out, "tables", "funnel_plot.csv"),
        file.path(out, "tables", "linear_tests.csv")
      )
    )),
    collapse = ""
  )

  expect_true(grepl("Wrote 2 tables and 1 plot to", msgs, fixed = TRUE))
  expect_true(grepl(basename(out), msgs, fixed = TRUE))
})

test_that("render_run_summary leaves out the categories the run did not write", {
  out <- local_tempdir()
  dir.create(file.path(out, "tables"))
  local_options(artma.verbose = 3, cli.width = 400)

  msgs <- paste(
    testthat::capture_messages(render_run_summary(
      make_results(),
      output_dir = out,
      run_files = file.path(out, "tables", "funnel_plot.csv")
    )),
    collapse = ""
  )

  written_line <- grep("Wrote", strsplit(msgs, "\n", fixed = TRUE)[[1]], value = TRUE)

  expect_true(grepl("Wrote 1 table to", written_line, fixed = TRUE))
  expect_false(grepl("plot", written_line, fixed = TRUE))
})

test_that("render_run_summary counts files outside the tables and graphics directories", {
  out <- local_tempdir()
  local_options(artma.verbose = 3, cli.width = 400)

  msgs <- paste(
    testthat::capture_messages(render_run_summary(
      make_results(),
      output_dir = out,
      run_files = file.path(out, "report.html")
    )),
    collapse = ""
  )

  expect_true(grepl("Wrote 1 other file to", msgs, fixed = TRUE))
})

test_that("render_run_summary says nothing was written when the run wrote no files", {
  out <- local_tempdir()
  local_options(artma.verbose = 3, cli.width = 400)

  msgs <- paste(
    testthat::capture_messages(render_run_summary(make_results(), output_dir = out)),
    collapse = ""
  )

  expect_true(grepl("Nothing was written to", msgs, fixed = TRUE))
  expect_false(grepl("Wrote", msgs, fixed = TRUE))
})

test_that("render_run_summary stays silent below info verbosity", {
  local_options(artma.verbose = 2)

  msgs <- testthat::capture_messages(render_run_summary(make_results()))

  expect_equal(length(msgs), 0L)
})

test_that("render_run_summary returns the summary it rendered, invisibly", {
  local_options(artma.verbose = 2)

  run <- render_run_summary(make_results())

  expect_false(is.null(run$ran))
})
