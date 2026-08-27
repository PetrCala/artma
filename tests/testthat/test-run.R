box::use(
  testing / mocks / index[MOCKS],
  testthat[
    capture_messages,
    capture_warnings,
    expect_equal,
    expect_error,
    expect_false,
    expect_match,
    expect_named,
    expect_null,
    expect_setequal,
    expect_true,
    test_that
  ]
)

# Options backed by a real mock dataset, so the prepare step can run its full
# provided-data branch (config lookups included). Built once at file scope:
# options_load() is pure, so each test activates the returned list with
# withr::local_options() and stays isolated.
prepare_step_options <- local({
  fixture_dir <- withr::local_tempdir(.local_envir = testthat::teardown_env())

  df <- MOCKS$create_mock_df(seed = 42, nrow = 60, n_studies = 5)
  source_path <- file.path(fixture_dir, "run-pipeline-data.csv")
  utils::write.csv(df, source_path, row.names = FALSE)

  options_dir <- file.path(fixture_dir, "options")
  dir.create(options_dir)

  artma::options_create(
    options_file_name = "run-pipeline.yaml",
    options_dir = options_dir,
    user_input = list(
      "data.source_path" = source_path,
      "data.na_handling" = "remove",
      "data.reconcile_mode" = "auto",
      "calc.se_zero_handling" = "ignore",
      "cache.use_cache" = FALSE,
      "verbose" = 1L
    ),
    should_validate = TRUE,
    should_overwrite = TRUE
  )

  artma::options_load(
    options_file_name = "run-pipeline.yaml",
    options_dir = options_dir,
    load_with_prefix = TRUE,
    should_validate = TRUE,
    should_add_temp_options = TRUE,
    should_return = TRUE
  )
})

mock_methods <- function() {
  list(
    method_a = list(run = function(df, ...) "method_a"),
    method_b = list(run = function(df, ...) "method_b"),
    method_c = list(run = function(df, ...) paste("called", nrow(df)))
  )
}

# Write the fake methods into a temporary box module tree and return the
# methods directory, to be passed to invoke_runtime_methods(modules_dir = ...).
# All state changes (temp files, the scoped box.path prepend needed for the
# generated box imports to resolve, and module cache entries) are registered
# for cleanup in the caller's frame via withr, so they are undone even when a
# test fails midway. Package bindings such as PATHS are never touched.
local_mock_methods_dir <- function(fake_methods, env = parent.frame()) {
  temp_root <- withr::local_tempdir(pattern = "artma-test-methods-", .local_envir = env)
  methods_dir <- file.path(temp_root, "artma", "methods")
  dir.create(methods_dir, recursive = TRUE, showWarnings = FALSE)

  for (name in names(fake_methods)) {
    file_path <- file.path(methods_dir, paste0(name, ".R"))
    run_fun <- fake_methods[[name]]$run
    stopifnot(is.function(run_fun))

    run_fun_code <- paste(deparse(run_fun), collapse = "\n")
    module_code <- paste0("run <- ", run_fun_code, "\n")

    meta <- fake_methods[[name]]$meta
    if (!is.null(meta)) {
      meta_code <- paste(deparse(meta), collapse = "\n")
      module_code <- paste0(
        module_code,
        "attr(run, \"artma_method_meta\") <- ", meta_code, "\n"
      )
    }

    writeLines(module_code, file_path)
  }

  withr::local_options(list(box.path = c(temp_root, getOption("box.path"))), .local_envir = env)

  withr::defer(
    {
      for (name in names(fake_methods)) {
        try(box::unload(sprintf("artma/methods/%s", name)), silent = TRUE)
      }
    },
    envir = env
  )

  methods_dir
}

test_that("invoke_runtime_methods handles explicit character vectors", {
  fake_methods <- mock_methods()
  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(
    methods = c("method_b", "method_a", "method_b"),
    df = df,
    modules_dir = methods_dir
  )

  expected_order <- names(fake_methods)[names(fake_methods) %in% c("method_b", "method_a")]

  expect_equal(names(results), expected_order)
  expect_equal(unname(unlist(results)), expected_order)
})

test_that("invoke_runtime_methods expands the all keyword", {
  fake_methods <- mock_methods()
  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame()
  results <- artma:::invoke_runtime_methods(methods = "all", df = df, modules_dir = methods_dir)

  expect_setequal(names(results), names(fake_methods))
})

test_that("invoke_runtime_methods leaves opt-in methods out of the all keyword", {
  fake_methods <- mock_methods()
  fake_methods$method_c$meta <- list(stage = "method_c", opt_in = TRUE)
  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame()
  results <- artma:::invoke_runtime_methods(methods = "all", df = df, modules_dir = methods_dir)

  expect_setequal(names(results), c("method_a", "method_b"))
})

test_that("invoke_runtime_methods still runs opt-in methods when named", {
  fake_methods <- mock_methods()
  fake_methods$method_c$meta <- list(stage = "method_c", opt_in = TRUE)
  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(
    methods = "method_c",
    df = df,
    modules_dir = methods_dir
  )

  expect_equal(names(results), "method_c")
})

test_that("invoke_runtime_methods surfaces invalid inputs early", {
  fake_methods <- mock_methods()
  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame()
  expect_error(
    artma:::invoke_runtime_methods(methods = c("missing", "method_a"), df = df, modules_dir = methods_dir),
    "Invalid runtime methods selected"
  )

  expect_error(
    artma:::invoke_runtime_methods(methods = c("method_a", NA_character_), df = df, modules_dir = methods_dir),
    "must not contain missing values"
  )

  expect_error(
    artma:::invoke_runtime_methods(methods = numeric(), df = df, modules_dir = methods_dir),
    "Runtime methods must be supplied as a character vector"
  )
})

test_that("invoke_runtime_methods isolates a failing method and keeps the rest", {
  fake_methods <- list(
    method_a = list(run = function(df, ...) "a-ok"),
    method_b = list(run = function(df, ...) cli::cli_abort("boom")),
    method_c = list(run = function(df, ...) "c-ok")
  )
  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(methods = "all", df = df, modules_dir = methods_dir)

  expect_setequal(names(results), c("method_a", "method_c"))
  expect_equal(results$method_a, "a-ok")
  expect_equal(results$method_c, "c-ok")

  failed <- attr(results, "failed_methods")
  expect_named(failed, "method_b")
  expect_match(unname(failed[["method_b"]]), "boom")
})

test_that("invoke_runtime_methods warns about failed methods when verbosity allows", {
  fake_methods <- list(
    method_a = list(run = function(df, ...) "a-ok"),
    method_b = list(run = function(df, ...) cli::cli_abort("boom"))
  )
  withr::local_options(list(artma.verbose = 2))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  warnings <- capture_warnings(
    results <- artma:::invoke_runtime_methods(methods = "all", df = df, modules_dir = methods_dir)
  )

  expect_true(any(grepl("method_b", warnings)))
  expect_equal(results$method_a, "a-ok")
})

test_that("invoke_runtime_methods emits a final warning when every method fails", {
  fake_methods <- list(
    method_a = list(run = function(df, ...) cli::cli_abort("first failure")),
    method_b = list(run = function(df, ...) cli::cli_abort("second failure"))
  )
  withr::local_options(list(artma.verbose = 2))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  warnings <- capture_warnings(
    results <- artma:::invoke_runtime_methods(methods = "all", df = df, modules_dir = methods_dir)
  )

  expect_equal(length(results), 0L)
  expect_setequal(names(attr(results, "failed_methods")), c("method_a", "method_b"))
  expect_true(any(grepl("All 2 requested methods failed", warnings)))
})

test_that("partial results from a run with a failing method are exported", {
  box::use(artma / output / export[export_results])

  fake_methods <- list(
    method_a = list(run = function(df, ...) list(tables = list(summary = data.frame(estimate = 1)))),
    method_b = list(run = function(df, ...) cli::cli_abort("boom")),
    method_c = list(run = function(df, ...) list(tables = list(summary = data.frame(estimate = 3))))
  )
  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(methods = "all", df = df, modules_dir = methods_dir)

  output_dir <- withr::local_tempdir()
  dir.create(file.path(output_dir, "tables"), recursive = TRUE)
  export_results(results, output_dir)

  expect_true(file.exists(file.path(output_dir, "tables", "method_a.csv")))
  expect_true(file.exists(file.path(output_dir, "tables", "method_c.csv")))
  expect_equal(length(list.files(file.path(output_dir, "tables"))), 2L)
})

test_that("invoke_runtime_methods forwards BMA result to dependent methods", {
  fake_methods <- list(
    bma = list(run = function(df, ...) list(token = "bma-ready")),
    best_practice_estimate = list(
      run = function(df, bma_result = NULL, ...) {
        if (is.null(bma_result)) {
          return("missing")
        }
        bma_result$token
      },
      meta = list(depends_on = "bma")
    )
  )

  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(
    methods = c("best_practice_estimate", "bma"),
    df = df,
    modules_dir = methods_dir
  )

  expect_named(results, c("bma", "best_practice_estimate"))
  expect_equal(results$best_practice_estimate, "bma-ready")
})

test_that("invoke_runtime_methods runs bma before its dependents when requested in reverse", {
  # fma and best_practice_estimate both depend on bma. Requested last-first,
  # the topological sort must still run bma before either dependent.
  fake_methods <- list(
    best_practice_estimate = list(
      run = function(df, ...) "bpe",
      meta = list(depends_on = "bma")
    ),
    fma = list(
      run = function(df, ...) "fma",
      meta = list(depends_on = "bma")
    ),
    bma = list(run = function(df, ...) "bma")
  )

  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(
    methods = c("best_practice_estimate", "fma", "bma"),
    df = df,
    modules_dir = methods_dir
  )

  bma_pos <- which(names(results) == "bma")
  expect_true(bma_pos < which(names(results) == "fma"))
  expect_true(bma_pos < which(names(results) == "best_practice_estimate"))
})

test_that("invoke_runtime_methods honors a transitive dependency chain", {
  # Explicit chain c -> b -> a; requested in reverse must run a, then b, then c.
  fake_methods <- list(
    c_method = list(run = function(df, ...) "c", meta = list(depends_on = "b_method")),
    b_method = list(run = function(df, ...) "b", meta = list(depends_on = "a_method")),
    a_method = list(run = function(df, ...) "a")
  )

  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(
    methods = c("c_method", "b_method", "a_method"),
    df = df,
    modules_dir = methods_dir
  )

  expect_equal(names(results), c("a_method", "b_method", "c_method"))
})

test_that("invoke_runtime_methods runs a discovered method absent from any order list", {
  # Regression for the silent-drop bug: a method with no declared metadata and
  # no place in a hardcoded order must still run.
  fake_methods <- list(
    method_a = list(run = function(df, ...) "a"),
    orphan_method = list(run = function(df, ...) "orphan")
  )

  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(methods = "all", df = df, modules_dir = methods_dir)

  expect_setequal(names(results), c("method_a", "orphan_method"))
  expect_equal(results$orphan_method, "orphan")
})

test_that("invoke_runtime_methods skips a method whose required columns are missing", {
  fake_methods <- list(
    needs_effect = list(
      run = function(df, ...) "ran",
      meta = list(required_columns = "effect")
    ),
    always_runs = list(run = function(df, ...) "ok")
  )

  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(methods = "all", df = df, modules_dir = methods_dir)

  expect_setequal(names(results), "always_runs")
  skipped <- attr(results, "skipped_methods")
  expect_named(skipped, "needs_effect")
  expect_match(unname(skipped[["needs_effect"]]), "effect")
})

test_that("invoke_runtime_methods skips a method whose suggested package is missing", {
  fake_methods <- list(
    needs_pkg = list(
      run = function(df, ...) "ran",
      meta = list(suggests = "artmaNoSuchPackage")
    ),
    always_runs = list(run = function(df, ...) "ok")
  )

  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(methods = "all", df = df, modules_dir = methods_dir)

  expect_setequal(names(results), "always_runs")
  skipped <- attr(results, "skipped_methods")
  expect_named(skipped, "needs_pkg")
  expect_match(unname(skipped[["needs_pkg"]]), "artmaNoSuchPackage")
})

test_that("invoke_runtime_methods aborts for a lone non-interactive method missing its package", {
  fake_methods <- list(
    needs_pkg = list(
      run = function(df, ...) "ran",
      meta = list(suggests = "artmaNoSuchPackage")
    )
  )

  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  # Non-interactive session (testthat) with a single requested method: hard abort.
  expect_error(
    artma:::invoke_runtime_methods(methods = "needs_pkg", df = df, modules_dir = methods_dir),
    "artmaNoSuchPackage"
  )
})

test_that("invoke_runtime_methods produces the same results in parallel and sequentially", {
  cores <- tryCatch(parallel::detectCores(), error = function(err) NA_integer_)
  testthat::skip_if(
    identical(.Platform$OS.type, "windows") || !is.numeric(cores) || is.na(cores) || cores < 2L,
    "forking is unavailable"
  )

  fake_methods <- list(
    method_a = list(run = function(df, ...) list(name = "a", draws = stats::runif(3))),
    method_b = list(run = function(df, ...) list(name = "b", draws = stats::runif(3))),
    method_c = list(run = function(df, ...) list(name = "c", rows = nrow(df)))
  )
  # Pin the seed: without one configured, each invocation derives its own run
  # seed from the session RNG, so two separate runs would differ by design.
  withr::local_options(list(artma.verbose = 0, artma.general.seed = 20240101L))
  methods_dir <- local_mock_methods_dir(fake_methods)
  df <- data.frame(x = 1:5)
  method_names <- c("method_a", "method_b", "method_c")

  withr::local_options(list(artma.general.parallel = FALSE))
  sequential <- artma:::invoke_runtime_methods(
    methods = method_names, df = df, modules_dir = methods_dir
  )

  withr::local_options(list(artma.general.parallel = TRUE))
  concurrent <- artma:::invoke_runtime_methods(
    methods = method_names, df = df, modules_dir = methods_dir
  )

  expect_equal(names(concurrent), method_names)
  # Per-method RNG streams make the stochastic draws identical either way.
  expect_equal(concurrent, sequential)
})

test_that("invoke_runtime_methods records a failure from a forked method", {
  cores <- tryCatch(parallel::detectCores(), error = function(err) NA_integer_)
  testthat::skip_if(
    identical(.Platform$OS.type, "windows") || !is.numeric(cores) || is.na(cores) || cores < 2L,
    "forking is unavailable"
  )

  fake_methods <- list(
    method_a = list(run = function(df, ...) "method_a"),
    method_b = list(run = function(df, ...) stop("method_b exploded")),
    method_c = list(run = function(df, ...) "method_c")
  )
  withr::local_options(list(artma.verbose = 0, artma.general.parallel = TRUE))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(
    methods = c("method_a", "method_b", "method_c"), df = df, modules_dir = methods_dir
  )

  expect_setequal(names(results), c("method_a", "method_c"))
  failed <- attr(results, "failed_methods")
  expect_equal(names(failed), "method_b")
  expect_match(failed[["method_b"]], "method_b exploded")
})

test_that("invoke_runtime_methods explains a forked method that was killed", {
  cores <- tryCatch(parallel::detectCores(), error = function(err) NA_integer_)
  testthat::skip_if(
    identical(.Platform$OS.type, "windows") || !is.numeric(cores) || is.na(cores) || cores < 2L,
    "forking is unavailable"
  )

  # A worker that dies without signalling (a segfault in a graphics device is
  # the real-world case) used to be recorded as a failure with an empty message.
  # Only ever signal a *forked child*: if this were to run in the current
  # process (because the layer fell back to sequential execution) the kill would
  # take down the testthat subprocess running this file. These methods are
  # written out to a module file, so they cannot close over a local variable;
  # the forked-worker option is set in the child and is readable there.
  fake_methods <- list(
    method_a = list(run = function(df, ...) "method_a"),
    method_b = list(run = function(df, ...) {
      if (!isTRUE(getOption("artma.temp.forked_worker", FALSE))) {
        return("method_b ran in the parent")
      }
      tools::pskill(Sys.getpid())
    }),
    method_c = list(run = function(df, ...) "method_c")
  )
  # Graphics exports force sequential execution where no fork-safe device
  # exists, so turn them off to keep this layer genuinely parallel.
  withr::local_options(list(
    artma.verbose = 0,
    artma.general.parallel = TRUE,
    artma.visualization.export_graphics = FALSE
  ))
  methods_dir <- local_mock_methods_dir(fake_methods)

  box::use(artma / modules / method_execution[resolve_worker_count])
  testthat::skip_if(
    resolve_worker_count(3L) < 2L,
    "this environment runs method layers sequentially"
  )

  df <- data.frame(x = 1:3)
  results <- suppressWarnings(artma:::invoke_runtime_methods(
    methods = c("method_a", "method_b", "method_c"), df = df, modules_dir = methods_dir
  ))

  failed <- attr(results, "failed_methods")
  expect_equal(names(failed), "method_b")
  expect_true(nzchar(failed[["method_b"]]))
  expect_match(failed[["method_b"]], "method_b")
})

test_that("invoke_runtime_methods passes dependency results across layers", {
  fake_methods <- list(
    method_a = list(run = function(df, ...) "from_a"),
    method_b = list(
      run = function(df, method_a_result = NULL, ...) method_a_result,
      meta = list(stage = "method_b", depends_on = "method_a")
    )
  )
  withr::local_options(list(artma.verbose = 0, artma.general.parallel = TRUE))
  methods_dir <- local_mock_methods_dir(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(
    methods = c("method_a", "method_b"), df = df, modules_dir = methods_dir
  )

  expect_equal(results$method_b, "from_a")
})

test_that("two runs with the same pinned seed produce identical stochastic results", {
  fake_methods <- list(
    noisy = list(run = function(df, ...) stats::rnorm(5))
  )
  withr::local_options(list(artma.verbose = 0, artma.general.seed = 20240101L))
  methods_dir <- local_mock_methods_dir(fake_methods)
  df <- data.frame(x = 1:3)

  first <- artma:::invoke_runtime_methods(methods = "noisy", df = df, modules_dir = methods_dir)
  second <- artma:::invoke_runtime_methods(methods = "noisy", df = df, modules_dir = methods_dir)
  expect_equal(first, second)

  withr::local_options(list(artma.general.seed = 7L))
  reseeded <- artma:::invoke_runtime_methods(methods = "noisy", df = df, modules_dir = methods_dir)
  expect_false(identical(first$noisy, reseeded$noisy))
})

test_that("an NA seed hands control of the run to the session RNG", {
  fake_methods <- list(
    noisy = list(run = function(df, ...) stats::rnorm(5))
  )
  withr::local_options(list(artma.verbose = 0, artma.general.seed = NA))
  methods_dir <- local_mock_methods_dir(fake_methods)
  df <- data.frame(x = 1:3)

  run_once <- function() {
    artma:::invoke_runtime_methods(methods = "noisy", df = df, modules_dir = methods_dir)
  }

  set.seed(42)
  first <- run_once()
  set.seed(42)
  second <- run_once()
  expect_equal(first, second)

  # Without reseeding, the next run derives a fresh seed from the session RNG.
  third <- run_once()
  expect_false(identical(first$noisy, third$noisy))

  # The pin of the derived seed is scoped to the run.
  expect_true(is.na(getOption("artma.general.seed")))
})

test_that("invoke_runtime_methods restores the session RNG state it found", {
  fake_methods <- list(
    noisy = list(run = function(df, ...) stats::rnorm(5))
  )
  withr::local_options(list(artma.verbose = 0, artma.general.seed = 20240101L))
  methods_dir <- local_mock_methods_dir(fake_methods)
  df <- data.frame(x = 1:3)

  set.seed(1)
  expected_next <- stats::rnorm(1)
  set.seed(1)
  artma:::invoke_runtime_methods(methods = "noisy", df = df, modules_dir = methods_dir)

  # A sequential layer runs methods in this process; their stream state must
  # not leak into what the caller draws next.
  expect_equal(stats::rnorm(1), expected_next)
})

# Pipeline steps ------------------------------------------------------------
#
# artma()'s linear path is prepare_run_context() -> execute_run() ->
# summarize_run(); the session hub calls the same three steps repeatedly. These
# tests drive the steps directly, with the stub methods dir standing in for the
# package methods.

# Current depth of the output-file capture stack, measured by opening a probe
# frame (its identifier is the new depth) and closing it again.
capture_depth <- function() {
  box::use(
    artma / libs / infrastructure / output_files[
      begin_output_file_capture, end_output_file_capture
    ]
  )
  probe <- begin_output_file_capture()
  end_output_file_capture(probe)
  probe - 1L
}

# A context as prepare_run_context() returns one, without preparing any data.
local_run_context <- function(df, save_results = TRUE, env = parent.frame()) {
  box::use(
    artma / libs / infrastructure / output_files[
      begin_output_file_capture, end_output_file_capture
    ],
    artma / output / export[ensure_output_dirs]
  )

  output_dir <- NULL
  if (isTRUE(save_results)) {
    output_dir <- withr::local_tempdir(.local_envir = env)
    ensure_output_dirs(output_dir)
  }

  capture <- begin_output_file_capture()
  withr::defer(end_output_file_capture(capture), envir = env)

  list(df = df, output_dir = output_dir, save_results = save_results, capture = capture)
}

test_that("execute_run invokes the methods and reports no files when results are not saved", {
  fake_methods <- list(
    method_a = list(run = function(df, ...) list(tables = list(summary = data.frame(estimate = 1))))
  )
  withr::local_options(list(artma.verbose = 0))
  methods_dir <- local_mock_methods_dir(fake_methods)

  context <- local_run_context(data.frame(x = 1:3), save_results = FALSE)
  depth_before <- capture_depth()

  run <- artma:::execute_run(context, methods = "method_a", modules_dir = methods_dir)

  expect_named(run, c("results", "run_files", "context"))
  expect_named(run$results, "method_a")
  expect_equal(run$run_files, character())
  # Nothing was exported, so the capture is still the caller's to close.
  expect_equal(capture_depth(), depth_before)
})

test_that("execute_run exports tables, writes the manifest, and closes the capture", {
  box::use(artma / output / run_manifest[read_run_manifest])

  fake_methods <- list(
    method_a = list(run = function(df, ...) list(tables = list(summary = data.frame(estimate = 1)))),
    method_b = list(run = function(df, ...) list(tables = list(summary = data.frame(estimate = 2))))
  )
  withr::local_options(list(artma.verbose = 0, artma.output.report = FALSE))
  methods_dir <- local_mock_methods_dir(fake_methods)

  context <- local_run_context(data.frame(x = 1:3))
  depth_before <- capture_depth()

  run <- artma:::execute_run(
    context,
    methods = c("method_a", "method_b"),
    modules_dir = methods_dir
  )

  expect_setequal(names(run$results), c("method_a", "method_b"))
  expect_true(file.exists(file.path(context$output_dir, "tables", "method_a.csv")))
  expect_true(file.exists(file.path(context$output_dir, "run.json")))

  # The exported tables were recorded before the capture closed, so the
  # manifest describes this run rather than the directory's contents.
  expect_true(any(basename(run$run_files) == "method_a.csv"))
  manifest <- read_run_manifest(context$output_dir)
  expect_setequal(as.character(manifest$methods_run), c("method_a", "method_b"))

  # The run step owns closing the capture it was handed.
  expect_equal(capture_depth(), depth_before - 1L)
})

test_that("execute_run gives every run its own subdirectory when they are enabled", {
  box::use(
    artma / libs / infrastructure / output_files[begin_output_file_capture],
    artma / output / export[latest_run_output_dir],
    artma / output / run_manifest[read_run_manifest]
  )

  fake_methods <- list(
    method_a = list(run = function(df, ...) list(tables = list(summary = data.frame(estimate = 1)))),
    method_b = list(run = function(df, ...) list(tables = list(summary = data.frame(estimate = 2))))
  )
  withr::local_options(list(
    artma.verbose = 0,
    artma.output.report = FALSE,
    artma.output.run_subdirectories = TRUE,
    artma.temp.run_output_dir = NULL
  ))
  methods_dir <- local_mock_methods_dir(fake_methods)

  # Both runs share one base directory, the way the hub's runs share one
  # prepared context.
  base <- withr::local_tempdir()
  run_context <- function() {
    list(
      df = data.frame(x = 1:3),
      output_dir = base,
      save_results = TRUE,
      capture = begin_output_file_capture()
    )
  }

  first <- artma:::execute_run(run_context(), methods = "method_a", modules_dir = methods_dir)
  second <- artma:::execute_run(run_context(), methods = "method_b", modules_dir = methods_dir)

  first_dir <- first$context$output_dir
  second_dir <- second$context$output_dir
  expect_false(identical(first_dir, second_dir))
  expect_equal(dirname(first_dir), file.path(base, "runs"))
  expect_equal(dirname(second_dir), file.path(base, "runs"))

  # Each run's outputs and manifest stay with that run.
  expect_true(file.exists(file.path(first_dir, "tables", "method_a.csv")))
  expect_true(file.exists(file.path(second_dir, "tables", "method_b.csv")))
  expect_false(file.exists(file.path(first_dir, "tables", "method_b.csv")))
  expect_equal(as.character(read_run_manifest(first_dir)$methods_run), "method_a")
  expect_equal(as.character(read_run_manifest(second_dir)$methods_run), "method_b")

  # What results_open() lands on.
  expect_equal(latest_run_output_dir(base), second_dir)
})

test_that("execute_run keeps the run alive when the report fails to render", {
  fake_methods <- list(
    method_a = list(run = function(df, ...) list(tables = list(summary = data.frame(estimate = 1))))
  )
  withr::local_options(list(artma.verbose = 0, artma.output.report = TRUE))
  methods_dir <- local_mock_methods_dir(fake_methods)

  context <- local_run_context(data.frame(x = 1:3))
  # An unwritable report path makes the render fail without touching anything
  # else the run produces.
  dir.create(file.path(context$output_dir, "report.html"))

  run <- artma:::execute_run(context, methods = "method_a", modules_dir = methods_dir)

  expect_named(run$results, "method_a")
  expect_true(file.exists(file.path(context$output_dir, "run.json")))
})

test_that("prepare_run_context hands over an open capture with the prepared data", {
  withr::local_options(prepare_step_options)
  withr::local_options(list(artma.verbose = 0, artma.output.save_results = FALSE))
  depth_before <- capture_depth()

  context <- artma:::prepare_run_context(data = NULL, methods = "funnel_plot")
  withr::defer({
    box::use(artma / libs / infrastructure / output_files[end_output_file_capture])
    end_output_file_capture(context$capture)
  })

  expect_null(context$output_dir)
  expect_false(isTRUE(context$save_results))
  expect_true(is.data.frame(context$df))
  expect_true(all(c("effect", "se") %in% names(context$df)))
  # The capture stays open: the run step (or artma()'s on.exit) closes it.
  expect_equal(capture_depth(), depth_before + 1L)
})

test_that("prepare_run_context preprocesses a supplied data frame the same way", {
  withr::local_options(prepare_step_options)
  withr::local_options(list(artma.verbose = 0, artma.output.save_results = TRUE))
  output_dir <- withr::local_tempdir()
  withr::local_options(list(artma.output.dir = output_dir))

  from_file <- artma:::prepare_run_context(data = NULL, methods = "funnel_plot")
  box::use(
    artma / data / read[read_data],
    artma / libs / infrastructure / output_files[end_output_file_capture]
  )
  end_output_file_capture(from_file$capture)

  from_data <- artma:::prepare_run_context(data = read_data(), methods = "funnel_plot")
  withr::defer(end_output_file_capture(from_data$capture))

  expect_true(is.data.frame(from_data$df))
  expect_true(all(c("effect", "se") %in% names(from_data$df)))
  expect_equal(names(from_data$df), names(from_file$df))
  # The output directories are resolved and created before any data work.
  expect_equal(from_data$output_dir, output_dir)
  expect_true(dir.exists(file.path(output_dir, "tables")))
})

test_that("prepare_run_context closes its capture when preparation fails", {
  withr::local_options(list(
    artma.verbose = 0,
    artma.output.save_results = FALSE,
    artma.data.source_path = NULL
  ))
  depth_before <- capture_depth()

  expect_error(artma:::prepare_run_context(data = NULL, methods = "funnel_plot"))

  expect_equal(capture_depth(), depth_before)
})

test_that("summarize_run prints the closing messages and returns the results invisibly", {
  context <- local_run_context(data.frame(x = 1:3))
  results <- list(method_a = list(tables = list()))
  attr(results, "run_info") <- list(
    methods_requested = "method_a",
    seed = 1L,
    output_files = list()
  )
  withr::local_options(list(artma.verbose = 3))

  messages <- capture_messages(
    returned <- withVisible(artma:::summarize_run(results, context = context))
  )

  expect_false(returned$visible)
  expect_equal(returned$value, results)
  expect_true(any(grepl("Analysis complete", messages)))
  expect_true(any(grepl("results_open", messages)))
})

test_that("summarize_run omits the results-directory hint when nothing was saved", {
  context <- local_run_context(data.frame(x = 1:3), save_results = FALSE)
  results <- list(method_a = list(tables = list()))
  attr(results, "run_info") <- list(
    methods_requested = "method_a",
    seed = 1L,
    output_files = list()
  )
  withr::local_options(list(artma.verbose = 3))

  messages <- capture_messages(artma:::summarize_run(results, context = context))

  expect_true(any(grepl("Analysis complete", messages)))
  expect_false(any(grepl("results_open", messages)))
})
