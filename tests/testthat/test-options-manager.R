box::use(
  artma / options / manager[
    list_options_files,
    load_template_metadata,
    read_user_options,
    resolve_options_dir,
    resolve_options_path,
    resolve_template_path,
    validate_flat_options
  ]
)

# pull helpers from testthat
for (fn in c(
  "test_that",
  "expect_equal",
  "expect_error",
  "expect_length",
  "expect_true",
  "expect_setequal"
)) {
  assign(fn, getFromNamespace(fn, "testthat"))
}

withr::local_options(list())

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

test_that("resolve_options_dir validates presence and can create directories", {
  temp_dir <- withr::local_tempdir()
  expect_equal(resolve_options_dir(temp_dir), temp_dir)

  missing_dir <- file.path(temp_dir, "missing")
  expect_error(resolve_options_dir(missing_dir), "does not exist")
  expect_equal(resolve_options_dir(missing_dir, must_exist = FALSE), missing_dir)

  created_dir <- resolve_options_dir(missing_dir, must_exist = FALSE, create_if_missing = TRUE)
  expect_true(dir.exists(created_dir))
})

test_that("resolve_template_path and resolve_options_path normalise inputs", {
  template_file <- withr::local_tempfile(fileext = ".yaml")
  make_template(template_file)

  expect_equal(resolve_template_path(template_file), template_file)

  temp_dir <- withr::local_tempdir()
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  option_path <- file.path(temp_dir, "test.yaml")
  yaml::write_yaml(list(general = list(name = "demo")), option_path)

  expect_equal(resolve_options_path("test.yaml", temp_dir, must_exist = TRUE), option_path)
  expect_error(resolve_options_path("missing.yaml", temp_dir, must_exist = TRUE), "does not exist")
})

test_that("read_user_options normalises empty YAML documents", {
  empty_yaml <- withr::local_tempfile(fileext = ".yaml")
  writeLines(character(0), empty_yaml)
  expect_equal(read_user_options(empty_yaml), list())
})

test_that("validate_flat_options captures missing, type mismatches, and redundant entries", {
  template_file <- withr::local_tempfile(fileext = ".yaml")
  make_template(template_file)
  meta <- load_template_metadata(template_file)

  valid_flat <- list(
    "general.name" = "demo",
    "data.threshold" = 10
  )
  validation <- validate_flat_options(valid_flat, meta$definitions)
  expect_length(validation$errors, 0)
  expect_length(validation$redundant, 0)

  invalid_flat <- list(
    "general.name" = 2L,
    "extra.value" = TRUE
  )
  validation <- validate_flat_options(invalid_flat, meta$definitions)
  expect_true(any(vapply(validation$errors, `[[`, character(1), "type") == "missing_option"))
  expect_true(any(vapply(validation$errors, `[[`, character(1), "type") == "type_mismatch"))
  expect_setequal(validation$redundant, "extra.value")
})

test_that("list_options_files returns file and verbose names", {
  options_dir <- withr::local_tempdir()
  template_file <- withr::local_tempfile(fileext = ".yaml")
  make_template(template_file)

  option_a <- file.path(options_dir, "a.yaml")
  option_b <- file.path(options_dir, "b.yaml")

  yaml::write_yaml(list(general = list(name = "Alpha")), option_a)
  yaml::write_yaml(list(general = list(name = "Beta")), option_b)

  expect_setequal(list_options_files(options_dir, should_return_verbose_names = FALSE), c("a.yaml", "b.yaml"))
  expect_setequal(list_options_files(options_dir, should_return_verbose_names = TRUE), c("Alpha", "Beta"))
})
