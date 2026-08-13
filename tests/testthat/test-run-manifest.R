box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_null,
    expect_setequal,
    expect_true,
    skip_if_not_installed,
    skip_on_cran,
    test_that
  ],
  withr[local_options, local_tempdir],
  artma / output / run_manifest[
    build_run_manifest,
    classify_run_files,
    manifest_files,
    manifest_plot_index,
    read_run_manifest,
    write_run_manifest
  ]
)

# Build a results list shaped like the one invoke_runtime_methods() returns.
make_results <- function(output_files = list(), methods = c("funnel_plot", "bma")) {
  results <- list(funnel_plot = list(tables = list(), plots = list(), meta = list()))
  attr(results, "skipped_methods") <- c(bma = "missing suggested package: BMS")
  attr(results, "run_info") <- list(
    methods_requested = methods,
    seed = 4242L,
    output_files = output_files
  )
  results
}

# classify_run_files --------------------------------------------------------

test_that("classify_run_files splits recorded paths by where they were written", {
  out <- local_tempdir()
  dir.create(file.path(out, "tables"))
  dir.create(file.path(out, "graphics"))
  local_options(artma.visualization.export_path = "graphics")

  files <- c(
    file.path(out, "tables", "bma.csv"),
    file.path(out, "graphics", "funnel_plot.png"),
    file.path(out, "report.html")
  )
  classified <- classify_run_files(files, out)

  expect_equal(basename(classified$tables), "bma.csv")
  expect_equal(basename(classified$graphics), "funnel_plot.png")
  expect_equal(basename(classified$other), "report.html")
})

test_that("classify_run_files de-duplicates repeated recordings", {
  out <- local_tempdir()
  path <- file.path(out, "tables", "bma.csv")
  classified <- classify_run_files(c(path, path), out)
  expect_equal(length(classified$tables), 1L)
})

# build_run_manifest --------------------------------------------------------

test_that("build_run_manifest records the run's identity", {
  out <- local_tempdir()
  data_path <- file.path(out, "meta.csv")
  utils::write.csv(data.frame(effect = 1, se = 1), data_path, row.names = FALSE)

  local_options(
    artma.data.source_path = data_path,
    artma.temp.file_name = "scenario.yaml",
    artma.temp.dir_name = out
  )

  manifest <- build_run_manifest(make_results(), output_dir = out)

  expect_equal(manifest$options_file, "scenario.yaml")
  expect_equal(manifest$options_dir, out)
  expect_equal(manifest$data_source_path, data_path)
  expect_true(nzchar(manifest$data_source_mtime))
  expect_equal(manifest$seed, 4242)
  expect_equal(as.character(manifest$methods_requested), c("funnel_plot", "bma"))
  expect_equal(as.character(manifest$methods_run), "funnel_plot")
  expect_equal(manifest$methods_skipped$bma, "missing suggested package: BMS")
  expect_equal(length(manifest$methods_failed), 0L)
  expect_true(nzchar(manifest$timestamp))
})

test_that("build_run_manifest lists only the files it was handed", {
  out <- local_tempdir()
  dir.create(file.path(out, "graphics"))
  local_options(artma.visualization.export_path = "graphics")

  png <- file.path(out, "graphics", "stem_funnel.png")
  file.create(png)
  # A file from an earlier run, sitting in the same directory.
  file.create(file.path(out, "graphics", "stale.png"))

  manifest <- build_run_manifest(
    make_results(output_files = list(nonlinear_tests = png)),
    output_dir = out,
    run_files = file.path(out, "tables", "bma.csv")
  )

  expect_equal(basename(manifest$files$graphics), "stem_funnel.png")
  expect_equal(basename(manifest$files$tables), "bma.csv")
  expect_equal(basename(manifest$method_files$nonlinear_tests), "stem_funnel.png")
})

# read/write ----------------------------------------------------------------

test_that("write_run_manifest round-trips through run.json", {
  skip_if_not_installed("jsonlite")

  out <- local_tempdir()
  local_options(artma.temp.file_name = "scenario.yaml")

  path <- write_run_manifest(make_results(), output_dir = out)
  expect_equal(path, file.path(out, "run.json"))
  expect_true(file.exists(path))

  manifest <- read_run_manifest(out)
  expect_equal(manifest$options_file, "scenario.yaml")
  expect_equal(manifest$methods_run, "funnel_plot")
  expect_equal(manifest$methods_skipped$bma, "missing suggested package: BMS")
})

test_that("write_run_manifest overwrites the previous run's manifest", {
  skip_if_not_installed("jsonlite")

  out <- local_tempdir()
  write_run_manifest(make_results(methods = c("funnel_plot", "bma")), output_dir = out)
  write_run_manifest(
    structure(list(), run_info = list(methods_requested = "fma", seed = 1L)), # nolint: undesirable_function_linter.
    output_dir = out
  )

  manifest <- read_run_manifest(out)
  expect_equal(manifest$methods_requested, "fma")
  expect_equal(length(manifest$methods_run), 0L)
})

test_that("read_run_manifest returns NULL when there is no manifest", {
  expect_null(read_run_manifest(local_tempdir()))
  expect_null(read_run_manifest(NULL))
})

# manifest_plot_index -------------------------------------------------------

test_that("manifest_plot_index keeps only PNGs that are still on disk", {
  out <- local_tempdir()
  png <- file.path(out, "funnel_plot.png")
  file.create(png)

  index <- manifest_plot_index(list(method_files = list(
    funnel_plot = c(png, file.path(out, "funnel_plot.html")),
    bma = file.path(out, "deleted.png")
  )))

  expect_equal(names(index), "funnel_plot")
  expect_equal(index$funnel_plot, png)
  expect_equal(length(manifest_plot_index(NULL)), 0L)
})

# End to end ----------------------------------------------------------------

# A run must leave a record of itself that lists what it produced, and only
# what it produced: the second run below writes into the directory the first
# one filled, and must not claim the first run's files or methods.
test_that("artma() writes a run.json describing exactly that run", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")

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
    options_file_name = "manifest_it.yaml",
    options_dir = options_dir,
    user_input = list(
      data = list(source_path = data_path),
      general = list(parallel = FALSE, seed = 123),
      output = list(dir = output_dir, save_results = TRUE)
    )
  )

  local_options(artma.verbose = 1)

  run <- function(methods) {
    artma::artma(methods = methods, options = "manifest_it.yaml", options_dir = options_dir)
    read_run_manifest(output_dir)
  }

  first <- run(c("funnel_plot", "effect_summary_stats"))

  expect_setequal(first$methods_requested, c("funnel_plot", "effect_summary_stats"))
  expect_true("funnel_plot" %in% first$methods_run)
  expect_equal(first$seed, 123)
  expect_true(nzchar(first$artma_version))

  first_files <- manifest_files(first)
  expect_true(length(first_files) > 0L)
  expect_true(all(file.exists(first_files)))
  expect_true(any(grepl("funnel", basename(first_files))))
  # Every listed file is one this run wrote into its own output directory.
  expect_true(all(startsWith(first_files, normalizePath(output_dir))))

  second <- run("effect_summary_stats")

  expect_equal(second$methods_requested, "effect_summary_stats")
  expect_equal(second$methods_run, "effect_summary_stats")

  second_files <- manifest_files(second)
  expect_true(length(second_files) > 0L)
  # The first run's funnel plot is still on disk, but it is not this run's.
  expect_true(any(grepl("funnel", basename(first_files))))
  expect_false(any(grepl("funnel", basename(second_files))))
})
