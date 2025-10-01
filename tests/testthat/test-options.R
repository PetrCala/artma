for (fn in c(
  "test_that",
  "expect_equal",
  "expect_error",
  "expect_length",
  "expect_setequal",
  "expect_true"
)) {
  assign(fn, getFromNamespace(fn, "testthat"))
}

make_template <- function(path) {
  template <- list(
    general = list(
      name = list(
        type = "character",
        default = "demo",
        help = "Human readable name"
      )
    ),
    data = list(
      threshold = list(
        type = "numeric",
        help = "Threshold value"
      )
    )
  )
  yaml::write_yaml(template, path)
  invisible(path)
}

write_options <- function(path, name_value = "Alpha", threshold_value = 10) {
  options <- list(
    general = list(name = name_value),
    data = list(threshold = threshold_value)
  )
  yaml::write_yaml(options, path)
  invisible(path)
}

test_that("options.validate returns no errors for valid file", {
  options_dir <- withr::local_tempdir()
  template_path <- withr::local_tempfile(fileext = ".yaml")
  make_template(template_path)

  file_path <- file.path(options_dir, "alpha.yaml")
  write_options(file_path)

  errors <- options.validate(
    options_file_name = basename(file_path),
    options_dir = options_dir,
    template_path = template_path,
    failure_action = "return_errors_quiet"
  )
  expect_length(errors, 0)
})

test_that("options.validate reports missing options", {
  options_dir <- withr::local_tempdir()
  template_path <- withr::local_tempfile(fileext = ".yaml")
  make_template(template_path)

  file_path <- file.path(options_dir, "missing.yaml")
  yaml::write_yaml(list(general = list(name = "Alpha")), file_path)

  errors <- options.validate(
    options_file_name = basename(file_path),
    options_dir = options_dir,
    template_path = template_path,
    failure_action = "return_errors_quiet"
  )
  expect_true(any(vapply(errors, `[[`, character(1), "type") == "missing_option"))
})

test_that("options.list enumerates file and verbose names", {
  options_dir <- withr::local_tempdir()
  template_path <- withr::local_tempfile(fileext = ".yaml")
  make_template(template_path)

  write_options(file.path(options_dir, "alpha.yaml"), name_value = "Alpha")
  write_options(file_path <- file.path(options_dir, "beta.yaml"), name_value = "Beta")

  expect_setequal(options.list(options_dir = options_dir, should_return_verbose_names = FALSE), c("alpha.yaml", "beta.yaml"))
  expect_setequal(options.list(options_dir = options_dir, should_return_verbose_names = TRUE), c("Alpha", "Beta"))
})

test_that("options.load returns flattened options with package prefix", {
  options_dir <- withr::local_tempdir()
  template_path <- withr::local_tempfile(fileext = ".yaml")
  make_template(template_path)

  file_path <- file.path(options_dir, "alpha.yaml")
  write_options(file_path, name_value = "Alpha", threshold_value = 2)

  loaded <- options.load(
    options_file_name = basename(file_path),
    options_dir = options_dir,
    template_path = template_path,
    should_validate = TRUE,
    should_set_to_namespace = FALSE,
    should_add_temp_options = TRUE,
    should_return = TRUE
  )

  expect_equal(loaded[["artma.general.name"]], "Alpha")
  expect_equal(loaded[["artma.data.threshold"]], 2)
  expect_equal(loaded[["artma.temp.file_name"]], basename(file_path))
  expect_equal(loaded[["artma.temp.dir_name"]], options_dir)
})
