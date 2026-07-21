box::use(
  testthat[test_that, expect_equal, expect_false, expect_true, expect_identical],
  testing / fixtures / index[FIXTURES]
)

# Caching is on by default, so every component of the cache signature needs a
# test that proves a change to it produces a miss. A stale hit is a correctness
# bug, not a performance one.

cache_test_options <- function() {
  list(
    "artma.cache.use_cache" = TRUE,
    "artma.cache.max_age" = 3600,
    "artma.verbose" = 1
  )
}

# A registered method whose runs are counted and whose received cache signature
# is recorded, wired to an isolated in-memory cache.
make_counted_method <- function(stage = "cache_signature_demo") {
  box::use(artma / modules / runtime_methods[register_runtime_method])

  state <- new.env(parent = emptyenv())
  state$runs <- 0L
  state$signature <- NULL

  impl <- function(df, bma_result = NULL, cache_signature = NULL) {
    state$runs <- state$runs + 1L
    state$signature <- cache_signature
    list(rows = nrow(df))
  }

  state$run <- register_runtime_method(
    impl,
    stage = stage,
    cache = memoise::cache_memory()
  )

  state
}

test_that("identical inputs reuse the cached method result", {
  FIXTURES$local_cli_silence()
  withr::local_options(cache_test_options())

  method <- make_counted_method()
  df <- data.frame(effect = c(0.1, 0.2), se = c(0.01, 0.02))

  testthat::capture_messages(first <- method$run(df = df))
  testthat::capture_messages(second <- method$run(df = df))

  expect_equal(method$runs, 1L)
  expect_identical(first, second)
})

test_that("a changed data frame misses the cache", {
  FIXTURES$local_cli_silence()
  withr::local_options(cache_test_options())

  method <- make_counted_method()

  testthat::capture_messages(method$run(df = data.frame(effect = 0.1, se = 0.01)))
  testthat::capture_messages(method$run(df = data.frame(effect = 0.9, se = 0.01)))

  expect_equal(method$runs, 2L)
})

test_that("a changed artma option misses the cache", {
  FIXTURES$local_cli_silence()
  withr::local_options(cache_test_options())

  method <- make_counted_method()
  df <- data.frame(effect = 0.1, se = 0.01)

  withr::with_options(
    list("artma.output.number_of_decimals" = 3),
    testthat::capture_messages(method$run(df = df))
  )
  expect_equal(method$runs, 1L)

  # An option the method never reads directly still changes the key: the
  # signature covers the whole artma.* group, deliberately erring towards
  # recomputation.
  withr::with_options(
    list("artma.output.number_of_decimals" = 5),
    testthat::capture_messages(method$run(df = df))
  )
  expect_equal(method$runs, 2L)
})

test_that("a changed upstream dependency result misses the cache", {
  FIXTURES$local_cli_silence()
  withr::local_options(cache_test_options())

  method <- make_counted_method()
  df <- data.frame(effect = 0.1, se = 0.01)

  bma_first <- list(tables = list(coefficients = data.frame(term = "x", estimate = 1)))
  bma_second <- list(tables = list(coefficients = data.frame(term = "x", estimate = 2)))

  testthat::capture_messages(method$run(df = df, bma_result = bma_first))
  expect_equal(method$runs, 1L)

  testthat::capture_messages(method$run(df = df, bma_result = bma_first))
  expect_equal(method$runs, 1L)

  # A changed BMA result must not be served the estimate built on the old one.
  testthat::capture_messages(method$run(df = df, bma_result = bma_second))
  expect_equal(method$runs, 2L)
})

test_that("the method source hash is part of the cache signature", {
  FIXTURES$local_cli_silence()
  withr::local_options(cache_test_options())

  df <- data.frame(effect = 0.1, se = 0.01)

  # "bma" and "fma" name real method files, so their hashes are resolvable and
  # must differ from one another.
  bma_method <- make_counted_method(stage = "bma")
  fma_method <- make_counted_method(stage = "fma")

  testthat::capture_messages(bma_method$run(df = df))
  testthat::capture_messages(fma_method$run(df = df))

  expect_true(is.character(bma_method$signature$method_source))
  expect_false(is.na(bma_method$signature$method_source))
  expect_false(
    identical(bma_method$signature$method_source, fma_method$signature$method_source)
  )
})

test_that("method_source_hash tracks edits to the method file", {
  box::use(
    artma / libs / infrastructure / source_fingerprint[
      forget_source_fingerprints, method_source_hash
    ]
  )

  methods_dir <- withr::local_tempdir()
  method_file <- file.path(methods_dir, "demo.R")

  writeLines("demo <- function(df) nrow(df)", method_file)
  before <- method_source_hash("demo", methods_dir = methods_dir)

  writeLines("demo <- function(df) nrow(df) * 2L", method_file)
  forget_source_fingerprints()
  after <- method_source_hash("demo", methods_dir = methods_dir)

  expect_false(identical(before, after))
  expect_true(is.na(method_source_hash("no_such_method", methods_dir = methods_dir)))
})

test_that("package_source_fingerprint tracks edits anywhere in the source tree", {
  box::use(
    artma / libs / infrastructure / source_fingerprint[
      forget_source_fingerprints, package_source_fingerprint
    ]
  )

  root <- withr::local_tempdir()
  dir.create(file.path(root, "libs"), recursive = TRUE)
  writeLines("a <- 1", file.path(root, "top.R"))
  writeLines("helper <- function() 1", file.path(root, "libs", "helper.R"))

  before <- package_source_fingerprint(root)

  # A shared helper a method calls into, not the method itself.
  writeLines("helper <- function() 2", file.path(root, "libs", "helper.R"))
  forget_source_fingerprints()
  after <- package_source_fingerprint(root)

  expect_false(identical(before, after))
})

test_that("a cache hit whose recorded plot file vanished recomputes", {
  box::use(
    artma / libs / infrastructure / cache[cache_cli_runner],
    artma / libs / infrastructure / output_files[record_output_file]
  )

  FIXTURES$local_cli_silence()
  withr::local_options(cache_test_options())

  plot_dir <- withr::local_tempdir()
  plot_path <- file.path(plot_dir, "demo_plot.png")

  runs <- 0L
  impl <- function(df) {
    runs <<- runs + 1L
    writeLines("plot", plot_path)
    record_output_file(plot_path)
    "done"
  }

  cached <- cache_cli_runner(
    impl,
    stage = "plot_replay",
    key_builder = function(df) list(rows = nrow(df)),
    cache = memoise::cache_memory()
  )

  df <- data.frame(x = 1)

  testthat::capture_messages(cached(df))
  expect_equal(runs, 1L)
  expect_true(file.exists(plot_path))

  # A plain hit: the file is still there, nothing to redo.
  testthat::capture_messages(cached(df))
  expect_equal(runs, 1L)

  # The results directory was cleared between runs. Replaying the return value
  # alone would report success with the plot missing.
  file.remove(plot_path)
  testthat::capture_messages(cached(df))
  expect_equal(runs, 2L)
  expect_true(file.exists(plot_path))
})

test_that("use_cache is honoured at call time, not at wrap time", {
  box::use(artma / libs / infrastructure / cache[cache_cli_runner])

  FIXTURES$local_cli_silence()

  runs <- 0L
  impl <- function(x) {
    runs <<- runs + 1L
    x
  }

  # Wrapped while caching is off, as happens when a method module loads before
  # the options file is applied.
  cached <- withr::with_options(
    list("artma.cache.use_cache" = FALSE),
    cache_cli_runner(
      impl,
      stage = "call_time_gate",
      key_builder = function(x) list(value = x),
      cache = memoise::cache_memory()
    )
  )

  withr::with_options(list("artma.cache.use_cache" = FALSE), {
    testthat::capture_messages(cached(1))
    testthat::capture_messages(cached(1))
  })
  expect_equal(runs, 2L)

  withr::with_options(list("artma.cache.use_cache" = TRUE), {
    testthat::capture_messages(cached(1))
    testthat::capture_messages(cached(1))
  })
  expect_equal(runs, 3L)
})
