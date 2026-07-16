box::use(testthat[test_that, expect_identical, skip])

test_that("R/generated_check_manifest.R matches the generator's current output", {
  pkg_root <- testthat::test_path("..", "..")
  generator_path <- file.path(pkg_root, "scripts", "R", "generate_check_manifest.R")

  if (!file.exists(generator_path)) {
    skip("generate_check_manifest.R is not available (expected when testing an installed/built package)")
  }

  generator_env <- new.env()
  source(generator_path, local = generator_env) # nolint: undesirable_function_linter.

  expected <- generator_env$build_check_manifest(pkg_root)
  manifest_path <- file.path(pkg_root, "R", "generated_check_manifest.R")
  actual <- readLines(manifest_path, warn = FALSE)

  expect_identical(
    actual,
    expected,
    info = "R/generated_check_manifest.R is stale. Run `make document` to regenerate it."
  )
})
