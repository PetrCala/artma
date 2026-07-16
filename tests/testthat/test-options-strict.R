box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_true,
    skip_if_not,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / options / utils[require_option, validate_option_value],
  artma / options / template[coerce_option_value],
  artma / options / significance_marks[resolve_add_significance_marks]
)

test_that("require_option returns a set value and aborts when unset", {
  local_options("artma.temp.file_name" = "config.yaml")
  expect_equal(require_option("artma.temp.file_name"), "config.yaml")

  local_options("artma.temp.dir_name" = NULL)
  expect_error(
    require_option("artma.temp.dir_name"),
    "artma.temp.dir_name"
  )
})

test_that("coerce_option_value fails loud on a non-numeric numeric option", {
  opt <- list(name = "calc.threshold", type = "numeric")
  expect_error(
    coerce_option_value("not-a-number", opt),
    "calc.threshold"
  )
  expect_equal(coerce_option_value("3.5", opt), 3.5)
})

test_that("coerce_option_value rejects fractional integers", {
  opt <- list(name = "output.number_of_decimals", type = "integer")
  expect_error(
    coerce_option_value(3.7, opt),
    "output.number_of_decimals"
  )
  expect_equal(coerce_option_value(3, opt), 3L)
})

test_that("coerce_option_value validates enum membership", {
  opt <- list(name = "data.reconcile_mode", type = "enum: ask|auto|strict")
  expect_error(
    coerce_option_value("nonsense", opt),
    "data.reconcile_mode"
  )
  expect_equal(coerce_option_value("auto", opt), "auto")
})

test_that("validate_option_value rejects fractional integers", {
  expect_false(is.null(validate_option_value(3.7, "integer", "output.number_of_decimals")))
  expect_true(is.null(validate_option_value(3L, "integer", "output.number_of_decimals")))
})

test_that("significance marks resolve from the canonical key only", {
  local_options(
    "artma.verbose" = 0,
    "artma.methods.add_significance_marks" = FALSE,
    "artma.methods.linear_tests.add_significance_marks" = TRUE
  )
  # The legacy per-method key is ignored; only the canonical key drives the value.
  expect_false(resolve_add_significance_marks())

  local_options(
    "artma.methods.add_significance_marks" = NULL,
    "artma.methods.linear_tests.add_significance_marks" = FALSE
  )
  # With the canonical key unset, the template default (TRUE) applies; the legacy
  # key does not flip it off.
  expect_true(resolve_add_significance_marks())
})

# Grep-based enforcement: every literal `getOption("artma.<key>")` in the package
# sources must pass an explicit default (or go through require_option). A bare
# read returns NULL off the runtime_setup path and fails far from the cause.
test_that("no getOption(\"artma.*\") reads omit a default", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = FALSE)
  dirs <- c(file.path(root, "R"), file.path(root, "inst", "artma"))
  skip_if_not(all(dir.exists(dirs)), "package sources are not available")

  files <- unlist(lapply(
    dirs,
    list.files,
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  ))

  # A single-argument read: getOption("artma.<key>") with the closing paren
  # right after the key string (optional surrounding whitespace).
  no_default_pattern <- "getOption\\(\\s*\"artma\\.[A-Za-z0-9_.]+\"\\s*\\)"

  offenders <- character()
  for (file in files) {
    lines <- readLines(file, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines)] # skip comments and roxygen docs
    hits <- grep(no_default_pattern, lines, perl = TRUE, value = TRUE)
    if (length(hits) > 0L) {
      offenders <- c(offenders, paste0(basename(file), ": ", trimws(hits)))
    }
  }

  expect_equal(offenders, character(0))
})
