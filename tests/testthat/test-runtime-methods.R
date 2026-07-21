box::use(
  testthat[test_that, expect_equal, expect_error, expect_named, expect_true],
  withr[local_options, local_tempdir, defer]
)

test_that("topo_sort_methods orders dependencies before dependents", {
  box::use(
    artma / modules / runtime_methods[topo_sort_methods]
  )

  deps <- list(
    fma = "bma",
    best_practice_estimate = "bma"
  )

  ordered <- topo_sort_methods(
    c("best_practice_estimate", "fma", "bma"),
    deps
  )

  expect_true(which(ordered == "bma") < which(ordered == "fma"))
  expect_true(which(ordered == "bma") < which(ordered == "best_practice_estimate"))
})

test_that("topo_sort_methods preserves input order for independent methods", {
  box::use(
    artma / modules / runtime_methods[topo_sort_methods]
  )

  ordered <- topo_sort_methods(c("c", "a", "b"), list())
  expect_equal(ordered, c("c", "a", "b"))
})

test_that("topo_sort_methods ignores dependencies outside the requested set", {
  box::use(
    artma / modules / runtime_methods[topo_sort_methods]
  )

  # fma depends on bma, but bma is not requested: fma still runs standalone.
  ordered <- topo_sort_methods(c("fma"), list(fma = "bma"))
  expect_equal(ordered, "fma")
})

test_that("topo_sort_methods aborts on a dependency cycle", {
  box::use(
    artma / modules / runtime_methods[topo_sort_methods]
  )

  deps <- list(a = "b", b = "a")
  expect_error(topo_sort_methods(c("a", "b"), deps), "[Cc]yclic")
})

test_that("missing_required_columns reports absent columns only", {
  box::use(
    artma / modules / runtime_methods[missing_required_columns]
  )

  df <- data.frame(effect = 1, se = 1)
  expect_equal(missing_required_columns(df, c("effect", "se")), character())
  expect_equal(missing_required_columns(df, c("effect", "study_id")), "study_id")
  expect_equal(missing_required_columns(df, character()), character())
})

test_that("missing_suggested_packages uses the injected predicate", {
  box::use(
    artma / modules / runtime_methods[missing_suggested_packages]
  )

  is_installed <- function(pkg) pkg == "installed_pkg"
  expect_equal(
    missing_suggested_packages(c("installed_pkg", "absent_pkg"), is_installed),
    "absent_pkg"
  )
  expect_equal(missing_suggested_packages(character(), is_installed), character())
})

test_that("register_runtime_method attaches declarative metadata", {
  box::use(
    artma / modules / runtime_methods[get_method_metadata, register_runtime_method]
  )

  impl <- function(df, ...) df
  run <- register_runtime_method(
    impl,
    stage = "demo",
    depends_on = "bma",
    required_columns = c("effect", "se"),
    suggests = "BMS"
  )

  meta <- get_method_metadata(run, name = "demo")
  expect_equal(meta$depends_on, "bma")
  expect_equal(meta$required_columns, c("effect", "se"))
  expect_equal(meta$suggests, "BMS")
  expect_equal(meta$stage, "demo")
  expect_equal(meta$opt_in, FALSE)
})

test_that("register_runtime_method records the opt-in flag", {
  box::use(
    artma / modules / runtime_methods[get_method_metadata, register_runtime_method]
  )

  run <- register_runtime_method(function(df, ...) df, stage = "pricey", opt_in = TRUE)

  expect_true(get_method_metadata(run, name = "pricey")$opt_in)
})

test_that("get_method_metadata fills defaults for methods without metadata", {
  box::use(
    artma / modules / runtime_methods[get_method_metadata]
  )

  meta <- get_method_metadata(function(df, ...) df, name = "bare")
  expect_equal(meta$depends_on, character())
  expect_equal(meta$required_columns, character())
  expect_equal(meta$suggests, character())
  expect_equal(meta$stage, "bare")
  expect_equal(meta$opt_in, FALSE)
})

test_that("get_runtime_method_modules ignores helper modules", {
  box::use(
    artma / modules / runtime_methods[get_runtime_method_modules]
  )

  local_options(list(artma.verbose = 0))

  temp_root <- local_tempdir()
  artma_root <- file.path(temp_root, "artma")
  methods_dir <- file.path(artma_root, "methods")

  dir.create(methods_dir, recursive = TRUE, showWarnings = FALSE)

  valid_method_module <- "run <- function(df, ...) 'ok'\n"
  helper_module <- "helper <- function() 'helper'\n"
  opt_out_module <- "run <- function(df, ...) 'ignored'\n.__runtime_method__ <- FALSE\n"

  writeLines(valid_method_module, file.path(methods_dir, "valid_method.R"))
  writeLines(helper_module, file.path(methods_dir, "helpers.R"))
  writeLines(opt_out_module, file.path(methods_dir, "opt_out.R"))

  defer(
    {
      try(box::unload("artma/methods/valid_method"), silent = TRUE)
      try(box::unload("artma/methods/helpers"), silent = TRUE)
      try(box::unload("artma/methods/opt_out"), silent = TRUE)
    },
    envir = parent.frame()
  )

  local_options(list(box.path = c(temp_root, getOption("box.path"))))

  modules <- get_runtime_method_modules(modules_dir = methods_dir)
  expect_named(modules, "valid_method")
})
