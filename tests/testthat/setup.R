# Make box imports resolve correctly during tests, ahead of any box.path
# inherited from the environment (e.g. an .Rprofile pointing at a different
# checkout). Runs in every testthat worker before test files load, so the tests
# always exercise the code they sit next to.
#
# Two roots are prepended:
#   * this checkout's inst/ directory, so `artma/...` imports resolve to the
#     code under test (used by `make test` / devtools; under R CMD check the
#     installed package resolves these instead and this directory is absent).
#   * tests/testthat/modules, the home of the test-only scaffolding
#     (`testing/...`) that no longer ships inside the installed package. This
#     directory travels with tests/ into the R CMD check tarball, so the
#     scaffolding resolves there in that context too.
local({
  candidate_paths <- c(
    testthat::test_path("..", "..", "inst"),
    testthat::test_path("modules")
  )
  extra_paths <- normalizePath(candidate_paths, winslash = "/", mustWork = FALSE)
  extra_paths <- extra_paths[dir.exists(extra_paths)]
  if (length(extra_paths) > 0) {
    options(box.path = unique(c(extra_paths, getOption("box.path"))))
  }
})
