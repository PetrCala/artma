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

mock_runtime_method_registry <- function(fake_methods) {
  box::use(artma / modules / runtime_methods)

  temp_root <- tempfile(pattern = "artma-test-methods-")
  artma_root <- file.path(temp_root, "artma")
  methods_dir <- file.path(artma_root, "methods")
  dir.create(methods_dir, recursive = TRUE, showWarnings = FALSE)

  for (name in names(fake_methods)) {
    file_path <- file.path(methods_dir, paste0(name, ".R"))
    run_fun <- fake_methods[[name]]$run
    stopifnot(is.function(run_fun))

    run_fun_code <- paste(deparse(run_fun), collapse = "\n")
    module_code <- paste0("run <- ", run_fun_code, "\n")
    writeLines(module_code, file_path)
  }

  withr::defer(unlink(temp_root, recursive = TRUE, force = TRUE), envir = parent.frame())

  original_box_path <- getOption("box.path")
  withr::local_options(list(box.path = c(temp_root, original_box_path)), .local_envir = parent.frame())

  imports_env <- parent.env(environment(runtime_methods$get_runtime_method_modules))
  original_methods_dir <- imports_env$PATHS$DIR_METHODS
  unlockBinding("PATHS", imports_env)
  imports_env$PATHS$DIR_METHODS <- methods_dir
  lockBinding("PATHS", imports_env)

  withr::defer(
    {
      unlockBinding("PATHS", imports_env)
      imports_env$PATHS$DIR_METHODS <- original_methods_dir
      lockBinding("PATHS", imports_env)
    },
    envir = parent.frame()
  )

  withr::defer(
    {
      for (name in names(fake_methods)) {
        try(box::unload(sprintf("artma/methods/%s", name)), silent = TRUE)
      }
    },
    envir = parent.frame()
  )
}

test_that("invoke_runtime_methods handles explicit character vectors", {
  fake_methods <- mock_methods()
  withr::local_options(list(artma.verbose = 0))
  mock_runtime_method_registry(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(methods = c("method_b", "method_a", "method_b"), df = df)

  expected_order <- names(fake_methods)[names(fake_methods) %in% c("method_b", "method_a")]

  expect_equal(names(results), expected_order)
  expect_equal(unname(unlist(results)), expected_order)
})

test_that("invoke_runtime_methods expands the all keyword", {
  fake_methods <- mock_methods()
  withr::local_options(list(artma.verbose = 0))
  mock_runtime_method_registry(fake_methods)

  df <- data.frame()
  results <- artma:::invoke_runtime_methods(methods = "all", df = df)

  expect_setequal(names(results), names(fake_methods))
})

test_that("invoke_runtime_methods surfaces invalid inputs early", {
  fake_methods <- mock_methods()
  withr::local_options(list(artma.verbose = 0))
  mock_runtime_method_registry(fake_methods)

  df <- data.frame()
  expect_error(
    artma:::invoke_runtime_methods(methods = c("missing", "method_a"), df = df),
    "Invalid runtime methods selected"
  )

  expect_error(
    artma:::invoke_runtime_methods(methods = c("method_a", NA_character_), df = df),
    "must not contain missing values"
  )

  expect_error(
    artma:::invoke_runtime_methods(methods = numeric(), df = df),
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
  mock_runtime_method_registry(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(methods = "all", df = df)

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
  mock_runtime_method_registry(fake_methods)

  df <- data.frame(x = 1:3)
  warnings <- capture_warnings(
    results <- artma:::invoke_runtime_methods(methods = "all", df = df)
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
  mock_runtime_method_registry(fake_methods)

  df <- data.frame(x = 1:3)
  warnings <- capture_warnings(
    results <- artma:::invoke_runtime_methods(methods = "all", df = df)
  )

  expect_equal(length(results), 0L)
  expect_setequal(names(attr(results, "failed_methods")), c("method_a", "method_b"))
  expect_true(any(grepl("All 2 requested methods failed", warnings)))
})

test_that("partial results from a run with a failing method are exported", {
  box::use(artma / output / export[export_results])

  fake_methods <- list(
    method_a = list(run = function(df, ...) data.frame(estimate = 1)),
    method_b = list(run = function(df, ...) cli::cli_abort("boom")),
    method_c = list(run = function(df, ...) data.frame(estimate = 3))
  )
  withr::local_options(list(artma.verbose = 0))
  mock_runtime_method_registry(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(methods = "all", df = df)

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
      }
    )
  )

  withr::local_options(list(artma.verbose = 0))
  mock_runtime_method_registry(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(
    methods = c("best_practice_estimate", "bma"),
    df = df
  )

  expect_named(results, c("bma", "best_practice_estimate"))
  expect_equal(results$best_practice_estimate, "bma-ready")
})
