box::use(testthat[test_that, expect_equal, expect_error, expect_setequal])
box::use(withr[defer, local_options])

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

  defer(unlink(temp_root, recursive = TRUE, force = TRUE), envir = parent.frame())

  original_box_path <- getOption("box.path")
  local_options(list(box.path = c(temp_root, original_box_path)), .local_envir = parent.frame())

  imports_env <- parent.env(environment(runtime_methods$get_runtime_method_modules))
  original_methods_dir <- imports_env$PATHS$DIR_METHODS
  unlockBinding("PATHS", imports_env)
  imports_env$PATHS$DIR_METHODS <- methods_dir
  lockBinding("PATHS", imports_env)

  defer(
    {
      unlockBinding("PATHS", imports_env)
      imports_env$PATHS$DIR_METHODS <- original_methods_dir
      lockBinding("PATHS", imports_env)
    },
    envir = parent.frame()
  )

  defer(
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
  local_options(list(artma.verbose = 0))
  mock_runtime_method_registry(fake_methods)

  df <- data.frame(x = 1:3)
  results <- artma:::invoke_runtime_methods(methods = c("method_b", "method_a", "method_b"), df = df)

  expect_equal(names(results), c("method_b", "method_a"))
  expect_equal(unname(unlist(results)), c("method_b", "method_a"))
})

test_that("invoke_runtime_methods expands the all keyword", {
  fake_methods <- mock_methods()
  local_options(list(artma.verbose = 0))
  mock_runtime_method_registry(fake_methods)

  df <- data.frame()
  results <- artma:::invoke_runtime_methods(methods = "all", df = df)

  expect_setequal(names(results), names(fake_methods))
})

test_that("invoke_runtime_methods surfaces invalid inputs early", {
  fake_methods <- mock_methods()
  local_options(list(artma.verbose = 0))
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
