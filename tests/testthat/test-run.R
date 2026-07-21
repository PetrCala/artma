box::use(
  testthat[
    capture_warnings,
    expect_equal,
    expect_error,
    expect_match,
    expect_named,
    expect_setequal,
    expect_true,
    test_that
  ]
)

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
  fs::dir_create(file.path(output_dir, "tables"))
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
