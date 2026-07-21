box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_length,
    expect_match,
    expect_null,
    expect_true,
    skip_if,
    test_that
  ]
)

can_fork <- function() {
  cores <- tryCatch(parallel::detectCores(), error = function(err) NA_integer_)
  !identical(.Platform$OS.type, "windows") && is.numeric(cores) && !is.na(cores) && cores >= 2L
}

test_that("group_methods_into_layers puts independent methods in one layer", {
  box::use(artma / modules / method_execution[group_methods_into_layers])

  layers <- group_methods_into_layers(c("a", "b", "c"), list())

  expect_length(layers, 1L)
  expect_equal(layers[[1L]], c("a", "b", "c"))
})

test_that("group_methods_into_layers separates dependents into later layers", {
  box::use(artma / modules / method_execution[group_methods_into_layers])

  layers <- group_methods_into_layers(
    c("bma", "funnel_plot", "best_practice_estimate"),
    list(best_practice_estimate = "bma")
  )

  expect_length(layers, 2L)
  expect_equal(layers[[1L]], c("bma", "funnel_plot"))
  expect_equal(layers[[2L]], "best_practice_estimate")
})

test_that("group_methods_into_layers chains transitive dependencies", {
  box::use(artma / modules / method_execution[group_methods_into_layers])

  layers <- group_methods_into_layers(
    c("a", "b", "c"),
    list(b = "a", c = "b")
  )

  expect_equal(layers, list("a", "b", "c"))
})

test_that("group_methods_into_layers ignores dependencies outside the requested set", {
  box::use(artma / modules / method_execution[group_methods_into_layers])

  layers <- group_methods_into_layers(c("fma"), list(fma = "bma"))

  expect_equal(layers, list("fma"))
})

test_that("group_methods_into_layers handles the empty case and aborts on cycles", {
  box::use(artma / modules / method_execution[group_methods_into_layers])

  expect_equal(group_methods_into_layers(character(), list()), list())
  expect_error(
    group_methods_into_layers(c("a", "b"), list(a = "b", b = "a")),
    "Cyclic"
  )
})

test_that("group_methods_into_layers flattens to a valid topological order", {
  box::use(
    artma / modules / method_execution[group_methods_into_layers],
    artma / modules / runtime_methods[topo_sort_methods]
  )

  names <- c("bma", "fma", "best_practice_estimate")
  deps <- list(best_practice_estimate = "bma", fma = "bma")

  flattened <- unlist(group_methods_into_layers(topo_sort_methods(names, deps), deps))

  expect_true(which(flattened == "bma") < which(flattened == "fma"))
  expect_true(which(flattened == "bma") < which(flattened == "best_practice_estimate"))
})

test_that("resolve_worker_count falls back to sequential execution", {
  box::use(artma / modules / method_execution[resolve_worker_count])

  withr::local_options(list(artma.general.parallel = TRUE))

  # A single task never forks.
  expect_equal(resolve_worker_count(1L, is_interactive = FALSE, os_type = "unix", n_cores = 8L), 1L)
  # Windows has no fork().
  expect_equal(resolve_worker_count(4L, is_interactive = FALSE, os_type = "windows", n_cores = 8L), 1L)
  # A single core leaves nothing to parallelise over.
  expect_equal(resolve_worker_count(4L, is_interactive = FALSE, os_type = "unix", n_cores = 1L), 1L)
  # An unknown core count is treated as unusable.
  expect_equal(
    resolve_worker_count(4L, is_interactive = FALSE, os_type = "unix", n_cores = NA_integer_),
    1L
  )
})

test_that("resolve_worker_count honours the artma.general.parallel flag", {
  box::use(artma / modules / method_execution[resolve_worker_count])

  withr::local_options(list(artma.general.parallel = FALSE))
  expect_equal(resolve_worker_count(4L, is_interactive = FALSE, os_type = "unix", n_cores = 8L), 1L)

  withr::local_options(list(artma.general.parallel = TRUE))
  expect_equal(resolve_worker_count(4L, is_interactive = FALSE, os_type = "unix", n_cores = 8L), 4L)
  expect_equal(resolve_worker_count(9L, is_interactive = FALSE, os_type = "unix", n_cores = 4L), 3L)
})

test_that("resolve_worker_count stays sequential when methods may still prompt", {
  box::use(artma / modules / method_execution[resolve_worker_count])

  withr::local_options(list(artma.general.parallel = TRUE, artma.autonomy.level = "balanced"))
  expect_equal(resolve_worker_count(4L, is_interactive = TRUE, os_type = "unix", n_cores = 8L), 1L)

  withr::local_options(list(artma.autonomy.level = "autonomous"))
  expect_equal(resolve_worker_count(4L, is_interactive = TRUE, os_type = "unix", n_cores = 8L), 4L)
})

test_that("with_captured_output captures output instead of printing it", {
  box::use(artma / modules / method_execution[with_captured_output])

  outcome <- testthat::expect_silent(
    with_captured_output({
      cat("to stdout\n")
      message("to stderr")
      "value"
    })
  )

  expect_equal(outcome$value, "value")
  expect_null(outcome$error)
  expect_true(any(grepl("to stdout", outcome$output, fixed = TRUE)))
  expect_true(any(grepl("to stderr", outcome$output, fixed = TRUE)))
})

test_that("with_captured_output keeps cli formatting intact", {
  box::use(artma / modules / method_execution[with_captured_output])

  withr::local_options(list(cli.num_colors = 256L))

  outcome <- with_captured_output(cli::cli_alert_success("all {.strong good}"))
  captured <- paste(outcome$output, collapse = "\n")

  expect_match(captured, "all")
  # ANSI escapes survive the capture, so replaying reproduces the styling.
  expect_true(grepl("\033[", captured, fixed = TRUE))
})

test_that("with_captured_output records errors and restores the sinks", {
  box::use(artma / modules / method_execution[with_captured_output])

  sinks_before <- sink.number()
  outcome <- with_captured_output(stop("boom"))

  expect_null(outcome$value)
  expect_equal(outcome$error, "boom")
  expect_equal(sink.number(), sinks_before)
})

test_that("build_rng_streams derives distinct, reproducible streams", {
  box::use(artma / modules / method_execution[build_rng_streams])

  streams <- build_rng_streams(c("a", "b"), seed = 42L)

  expect_equal(names(streams), c("a", "b"))
  expect_false(identical(streams$a, streams$b))
  expect_equal(streams, build_rng_streams(c("a", "b"), seed = 42L))
  expect_false(identical(streams, build_rng_streams(c("a", "b"), seed = 7L)))
})

test_that("build_rng_streams leaves the caller's RNG state untouched", {
  box::use(artma / modules / method_execution[build_rng_streams])

  set.seed(123)
  before_kind <- RNGkind()
  before_seed <- .Random.seed

  build_rng_streams(c("a", "b", "c"), seed = 1L)

  expect_equal(RNGkind(), before_kind)
  expect_equal(.Random.seed, before_seed)
})

test_that("execute_method_layer returns results in input order", {
  box::use(artma / modules / method_execution[execute_method_layer])

  outcomes <- execute_method_layer(c("c", "a", "b"), run_one = function(name) toupper(name))

  expect_equal(vapply(outcomes, function(o) o$value, character(1)), c("C", "A", "B"))
})

test_that("execute_method_layer isolates failures from the rest of the layer", {
  box::use(artma / modules / method_execution[execute_method_layer])

  run_one <- function(name) {
    if (identical(name, "b")) stop("b exploded")
    name
  }

  for (workers in c(1L, 2L)) {
    skip_if(workers > 1L && !can_fork(), "forking is unavailable")

    outcomes <- execute_method_layer(c("a", "b", "c"), run_one = run_one, workers = workers)

    expect_equal(outcomes[[1L]]$value, "a")
    expect_null(outcomes[[2L]]$value)
    expect_match(outcomes[[2L]]$error, "b exploded")
    expect_equal(outcomes[[3L]]$value, "c")
  }
})

test_that("execute_method_layer gives identical results in parallel and sequentially", {
  box::use(artma / modules / method_execution[build_rng_streams, execute_method_layer])

  skip_if(!can_fork(), "forking is unavailable")

  names <- c("a", "b", "c")
  streams <- build_rng_streams(names, seed = 99L)
  run_one <- function(name) list(name = name, draws = stats::runif(3))

  sequential <- execute_method_layer(names, run_one, streams = streams, workers = 1L)
  concurrent <- execute_method_layer(names, run_one, streams = streams, workers = 3L)

  expect_equal(
    lapply(sequential, function(o) o$value),
    lapply(concurrent, function(o) o$value)
  )
})

test_that("execute_method_layer captures output from forked workers", {
  box::use(artma / modules / method_execution[execute_method_layer])

  skip_if(!can_fork(), "forking is unavailable")

  outcomes <- testthat::expect_silent(
    execute_method_layer(
      c("a", "b"),
      run_one = function(name) {
        cli::cli_alert_info("running {name}")
        name
      },
      workers = 2L
    )
  )

  expect_true(any(grepl("running a", outcomes[[1L]]$output, fixed = TRUE)))
  expect_true(any(grepl("running b", outcomes[[2L]]$output, fixed = TRUE)))
})
