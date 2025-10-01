box::use(testthat[test_that, expect_equal, expect_error, expect_setequal, with_mocked_bindings])
box::use(withr[local_options])

mock_methods <- function() {
  list(
    method_a = list(run = function(df, ...) "method_a"),
    method_b = list(run = function(df, ...) "method_b"),
    method_c = list(run = function(df, ...) paste("called", nrow(df)))
  )
}

test_that("invoke_runtime_methods handles explicit character vectors", {
  fake_methods <- mock_methods()
  local_options(list(artma.verbose = 0))

  df <- data.frame(x = 1:3)
  results <- with_mocked_bindings(
    artma:::invoke_runtime_methods(methods = c("method_b", "method_a", "method_b"), df = df),
    get_runtime_method_modules = function() fake_methods,
    .package = "artma"
  )

  expect_equal(names(results), c("method_b", "method_a"))
  expect_equal(unname(unlist(results)), c("method_b", "method_a"))
})

test_that("invoke_runtime_methods expands the all keyword", {
  fake_methods <- mock_methods()
  local_options(list(artma.verbose = 0))

  df <- data.frame()
  results <- with_mocked_bindings(
    artma:::invoke_runtime_methods(methods = "all", df = df),
    get_runtime_method_modules = function() fake_methods,
    .package = "artma"
  )

  expect_setequal(names(results), names(fake_methods))
})

test_that("invoke_runtime_methods surfaces invalid inputs early", {
  fake_methods <- mock_methods()
  local_options(list(artma.verbose = 0))

  df <- data.frame()
  expect_error(
    with_mocked_bindings(
      artma:::invoke_runtime_methods(methods = c("missing", "method_a"), df = df),
      get_runtime_method_modules = function() fake_methods,
      .package = "artma"
    ),
    "Invalid runtime methods selected"
  )

  expect_error(
    with_mocked_bindings(
      artma:::invoke_runtime_methods(methods = c("method_a", NA_character_), df = df),
      get_runtime_method_modules = function() fake_methods,
      .package = "artma"
    ),
    "must not contain missing values"
  )

  expect_error(
    with_mocked_bindings(
      artma:::invoke_runtime_methods(methods = numeric(), df = df),
      get_runtime_method_modules = function() fake_methods,
      .package = "artma"
    ),
    "Runtime methods must be supplied as a character vector"
  )
})
