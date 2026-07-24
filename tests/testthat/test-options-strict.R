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
  artma / options / significance_marks[resolve_add_significance_marks],
  artma / libs / core / utils[opt_or]
)

test_that("opt_or treats NULL and scalar NA as unset", {
  local_options("artma.test_opt_or" = NULL)
  expect_equal(opt_or("artma.test_opt_or", "fallback"), "fallback")

  local_options("artma.test_opt_or" = NA)
  expect_equal(opt_or("artma.test_opt_or", "fallback"), "fallback")

  local_options("artma.test_opt_or" = "set")
  expect_equal(opt_or("artma.test_opt_or", "fallback"), "set")

  # Multi-element vectors pass through even when they contain NAs.
  local_options("artma.test_opt_or" = c("a", NA))
  expect_equal(opt_or("artma.test_opt_or", "fallback"), c("a", NA))
})

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

# Options whose template default is `.na` (allow_na) are loaded into options()
# as a literal NA, which getOption() returns *instead of* the supplied
# fallback. Any scalar such option must therefore be read via opt_or() (NA
# falls through to the default) or with an explicit `default = NA` raw read
# followed by an is.na() guard. A plain getOption(key, fallback) on these keys
# reintroduces the `if (NA == ...)` crash class from issue #321.
test_that("NA-able scalar options are never read via getOption with a non-NA default", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = FALSE)
  dirs <- c(file.path(root, "R"), file.path(root, "inst", "artma"))
  template_path <- file.path(root, "inst", "artma", "options", "templates", "options_template.yaml")
  skip_if_not(all(dir.exists(dirs)) && file.exists(template_path), "package sources are not available")

  template <- yaml::read_yaml(template_path)

  collect_na_able_keys <- function(node, path = character()) {
    if (!is.list(node)) {
      return(character())
    }
    is_leaf <- is.character(node[["type"]]) && length(node[["type"]]) == 1L
    if (is_leaf) {
      is_scalar <- !startsWith(node[["type"]], "list")
      if (isTRUE(node[["allow_na"]]) && is_scalar) {
        return(paste(path, collapse = "."))
      }
      return(character())
    }
    unlist(lapply(names(node), function(key) {
      collect_na_able_keys(node[[key]], c(path, key))
    }), use.names = FALSE)
  }

  na_able_keys <- collect_na_able_keys(template)
  # temp.* options are populated at runtime and never hold a literal NA; NULL
  # is their documented "not yet set" sentinel, read via require_option() or a
  # NULL default.
  na_able_keys <- na_able_keys[!startsWith(na_able_keys, "temp.")]
  skip_if_not(length(na_able_keys) > 0, "no NA-able scalar options found in the template")

  keys_pattern <- paste(gsub(".", "\\.", na_able_keys, fixed = TRUE), collapse = "|")
  read_pattern <- sprintf("getOption\\(\\s*\"artma\\.(%s)\"", keys_pattern)
  # Allowed raw-read form: an explicit NA default (named or positional), e.g.
  # getOption(key, default = NA) or getOption(key, NA_character_).
  na_default_pattern <- ",\\s*(default\\s*=\\s*)?NA[A-Za-z_]*\\s*\\)"

  files <- unlist(lapply(
    dirs,
    list.files,
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  ))

  offenders <- character()
  for (file in files) {
    lines <- readLines(file, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines)] # skip comments and roxygen docs
    hits <- grep(read_pattern, lines, perl = TRUE, value = TRUE)
    hits <- hits[!grepl(na_default_pattern, hits, perl = TRUE)]
    if (length(hits) > 0L) {
      offenders <- c(offenders, paste0(basename(file), ": ", trimws(hits)))
    }
  }

  expect_equal(offenders, character(0))
})
