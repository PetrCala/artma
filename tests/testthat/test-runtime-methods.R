box::use(
  testthat[test_that, expect_named],
  withr[local_options, local_tempdir, defer]
)

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
