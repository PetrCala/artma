# Make box imports resolve to this checkout's inst/ directory, ahead of any
# box.path inherited from the environment (e.g. an .Rprofile pointing at a
# different checkout). Runs in every testthat worker before test files load,
# so the tests always exercise the code they sit next to.
local({
  inst_dir <- normalizePath(
    testthat::test_path("..", "..", "inst"),
    winslash = "/",
    mustWork = FALSE
  )
  if (dir.exists(inst_dir)) {
    options(box.path = unique(c(inst_dir, getOption("box.path"))))
  }
})
