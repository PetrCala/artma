box::use(testthat[test_that, expect_equal, expect_true, expect_false, expect_null, expect_setequal, expect_length, expect_identical, expect_named, expect_output, expect_s3_class])

# A small template that mirrors the shape of the real one: several top-level
# sections, a required option with no default, and a list-typed leaf.
local_template <- function(dir) {
  template <- list(
    general = list(
      name = list(
        type = "character",
        default = "Default Config",
        help = "Friendly name for the configuration"
      )
    ),
    data = list(
      source_path = list(
        type = "character",
        help = "Path to the data file"
      ),
      columns = list(
        type = "list",
        default = list(),
        help = "The unified column store"
      )
    ),
    methods = list(
      bma = list(
        iter = list(
          type = "integer",
          default = 100L,
          help = "Number of iterations"
        ),
        burn = list(
          type = "integer",
          default = 10L,
          help = "Number of burn-in draws"
        )
      ),
      linear_tests = list(
        conf_level = list(
          type = "numeric",
          default = 0.95,
          help = "Confidence level"
        )
      )
    )
  )
  template_path <- file.path(dir, "template.yaml")
  yaml::write_yaml(template, template_path)
  template_path
}

test_that("option name tokens expand exact names and group prefixes", {
  box::use(artma / options / inspect[expand_option_tokens])

  all_names <- c("general.name", "methods.bma.iter", "methods.bma.burn", "methods.linear_tests.conf_level")

  exact <- expand_option_tokens("methods.bma.iter", all_names)
  expect_equal(exact$matched, "methods.bma.iter")
  expect_length(exact$not_found, 0)

  node <- expand_option_tokens("methods.bma", all_names)
  expect_equal(node$matched, c("methods.bma.iter", "methods.bma.burn"))

  group <- expand_option_tokens("methods", all_names)
  expect_equal(group$matched, c("methods.bma.iter", "methods.bma.burn", "methods.linear_tests.conf_level"))

  mixed <- expand_option_tokens(c("methods.bma", "nonsense", "general.name"), all_names)
  expect_equal(mixed$matched, c("methods.bma.iter", "methods.bma.burn", "general.name"))
  expect_equal(mixed$not_found, "nonsense")

  # A prefix that is not followed by a separator must not match by accident
  expect_equal(expand_option_tokens("methods.bm", all_names)$not_found, "methods.bm")
})

test_that("options_help with no arguments prints the whole option tree", {
  box::use(artma[options_help])

  tmp_dir <- withr::local_tempdir()
  template_path <- local_template(tmp_dir)

  output <- testthat::capture_output(
    options_help(template_path = template_path),
    print = TRUE
  )

  expect_true(grepl("general.name", output, fixed = TRUE))
  expect_true(grepl("methods.bma.iter", output, fixed = TRUE))
  expect_true(grepl("methods.linear_tests.conf_level", output, fixed = TRUE))
  # The tree is the compact rendering: no per-option help text
  expect_false(grepl("Number of iterations", output, fixed = TRUE))
})

test_that("options_help expands a group name to every option beneath it", {
  box::use(artma[options_help])

  tmp_dir <- withr::local_tempdir()
  template_path <- local_template(tmp_dir)

  output <- testthat::capture_output(
    options_help("methods.bma", template_path = template_path),
    print = TRUE
  )

  expect_true(grepl("Number of iterations", output, fixed = TRUE))
  expect_true(grepl("Number of burn-in draws", output, fixed = TRUE))
  expect_false(grepl("Confidence level", output, fixed = TRUE))
})

test_that("options_help reports unrecognized names but still explains the rest", {
  box::use(artma[options_help])

  tmp_dir <- withr::local_tempdir()
  template_path <- local_template(tmp_dir)

  messages <- character(0)
  output <- testthat::capture_output(
    messages <- testthat::capture_messages(
      options_help(c("nonsense", "general.name"), template_path = template_path)
    ),
    print = TRUE
  )

  expect_true(any(grepl("not recognized", c(output, messages), fixed = TRUE)))
  expect_true(grepl("Friendly name for the configuration", output, fixed = TRUE))
})

test_that("options_list(details = TRUE) describes each options file", {
  box::use(artma[options_list])

  tmp_dir <- withr::local_tempdir()
  template_path <- local_template(tmp_dir)

  yaml::write_yaml(
    list(
      general = list(name = "Baseline"),
      data = list(source_path = "/data/baseline.csv", columns = list()),
      methods = list(bma = list(iter = 100L, burn = 10L), linear_tests = list(conf_level = 0.95))
    ),
    file.path(tmp_dir, "baseline.yaml")
  )
  yaml::write_yaml(
    list(
      general = list(name = "Tweaked"),
      data = list(source_path = "/data/other.csv", columns = list()),
      methods = list(bma = list(iter = 500L, burn = 10L), linear_tests = list(conf_level = 0.99))
    ),
    file.path(tmp_dir, "tweaked.yaml")
  )

  details <- options_list(options_dir = tmp_dir, details = TRUE, template_path = template_path)

  expect_s3_class(details, "data.frame")
  expect_named(details, c("file", "data_source_path", "modified", "last_run", "n_non_default"))
  expect_setequal(details$file, c("baseline.yaml", "tweaked.yaml"))

  baseline <- details[details$file == "baseline.yaml", ]
  tweaked <- details[details$file == "tweaked.yaml", ]

  expect_equal(baseline$data_source_path, "/data/baseline.csv")
  expect_equal(tweaked$data_source_path, "/data/other.csv")

  # 'general.name' and the required 'data.source_path' deviate in both files;
  # the tweaked file also moves two method options.
  expect_equal(baseline$n_non_default, 2L)
  expect_equal(tweaked$n_non_default, 4L)

  expect_true(all(!is.na(details$modified)))
  # No run has produced output for these files, so the last run time is unknown
  expect_true(all(is.na(details$last_run)))
})

test_that("options_list(details = TRUE) returns an empty frame when no files exist", {
  box::use(artma[options_list])

  template_path <- local_template(withr::local_tempdir())
  empty_dir <- withr::local_tempdir()

  details <- options_list(options_dir = empty_dir, details = TRUE, template_path = template_path)

  expect_s3_class(details, "data.frame")
  expect_equal(nrow(details), 0L)
})

test_that("options_list without details keeps returning file names", {
  box::use(artma[options_list])

  tmp_dir <- withr::local_tempdir()
  yaml::write_yaml(list(general = list(name = "One")), file.path(tmp_dir, "one.yaml"))

  expect_identical(options_list(options_dir = tmp_dir), "one.yaml")
})

test_that("options_diff reports differing options and deviations from defaults", {
  box::use(artma[options_diff])

  tmp_dir <- withr::local_tempdir()
  template_path <- local_template(tmp_dir)

  yaml::write_yaml(
    list(
      general = list(name = "Baseline"),
      data = list(source_path = "/data/baseline.csv", columns = list(effect = list(source_name = "eff"))),
      methods = list(bma = list(iter = 100L, burn = 10L), linear_tests = list(conf_level = 0.95))
    ),
    file.path(tmp_dir, "baseline.yaml")
  )
  yaml::write_yaml(
    list(
      general = list(name = "Baseline"),
      data = list(source_path = "/data/other.csv", columns = list(effect = list(source_name = "effect_size"))),
      methods = list(bma = list(iter = 500L, burn = 10L), linear_tests = list(conf_level = 0.95))
    ),
    file.path(tmp_dir, "tweaked.yaml")
  )

  diff <- options_diff(
    options_file_name_a = "baseline.yaml",
    options_file_name_b = "tweaked.yaml",
    options_dir = tmp_dir,
    template_path = template_path
  )

  expect_setequal(
    diff$differences$option,
    c("data.source_path", "data.columns.effect.source_name", "methods.bma.iter")
  )
  expect_named(diff$differences, c("option", "baseline.yaml", "tweaked.yaml"))
  expect_equal(diff$differences[[2]][diff$differences$option == "methods.bma.iter"], "100")
  expect_equal(diff$differences[[3]][diff$differences$option == "methods.bma.iter"], "500")

  expect_setequal(
    diff$deviations[["baseline.yaml"]]$option,
    c("general.name", "data.source_path", "data.columns")
  )
  expect_setequal(
    diff$deviations[["tweaked.yaml"]]$option,
    c("general.name", "data.source_path", "data.columns", "methods.bma.iter")
  )
})

test_that("options_diff on identical files reports no differences", {
  box::use(artma[options_copy, options_diff])

  tmp_dir <- withr::local_tempdir()
  template_path <- local_template(tmp_dir)

  yaml::write_yaml(
    list(
      general = list(name = "Baseline"),
      data = list(source_path = "/data/baseline.csv", columns = list()),
      methods = list(bma = list(iter = 100L, burn = 10L), linear_tests = list(conf_level = 0.95))
    ),
    file.path(tmp_dir, "baseline.yaml")
  )
  options_copy(
    options_file_name_from = "baseline.yaml",
    options_file_name_to = "clone.yaml",
    options_dir = tmp_dir,
    should_overwrite = TRUE
  )

  diff <- options_diff(
    options_file_name_a = "baseline.yaml",
    options_file_name_b = "clone.yaml",
    options_dir = tmp_dir,
    template_path = template_path
  )

  expect_equal(nrow(diff$differences), 0L)
})

test_that("option values are formatted for a single display line", {
  box::use(artma / options / inspect[format_option_value])

  expect_equal(format_option_value(NULL), "null")
  expect_equal(format_option_value(NA), "NA")
  expect_equal(format_option_value(TRUE), "true")
  expect_equal(format_option_value(3L), "3")
  expect_equal(format_option_value(c(1.645, 1.96)), "[1.645, 1.96]")
  expect_equal(format_option_value(list(a = 1, b = 2)), "<list of 2 entries>")
  expect_equal(format_option_value(list(a = 1)), "<list of 1 entry>")
  expect_equal(nchar(format_option_value(strrep("x", 200), max_len = 20L)), 20L)
})

test_that("value comparison tolerates YAML type noise but not missing keys", {
  box::use(artma / options / inspect[values_equal])

  expect_true(values_equal(10L, 10))
  expect_true(values_equal(list(a = 1), list(a = 1)))
  expect_true(values_equal(NULL, NULL))
  expect_false(values_equal(NULL, NA))
  expect_false(values_equal("10", 10))
})

# Help strings are rendered through cli::format_inline(), which also makes them
# glue templates: a literal brace (e.g. `\usepackage{booktabs}`) is evaluated as
# an R expression and aborts. That took out `options_help("output")` entirely, so
# guard both the template content and the renderer.
test_that("every template help string renders without a glue error", {
  box::use(artma / options / template[get_option_defs])

  broken <- Filter(
    function(def) {
      txt <- def$help
      if (is.null(txt) || !is.character(txt)) {
        return(FALSE)
      }
      inherits(tryCatch(cli::format_inline(txt), error = function(e) e), "error")
    },
    get_option_defs()
  )

  expect_equal(vapply(broken, function(def) def$name, character(1)), character(0))
})

test_that("print_options_help_text falls back to raw text on a malformed help string", {
  box::use(artma / options / utils[print_options_help_text])

  expect_output(
    print_options_help_text("requires \\usepackage{booktabs} in your preamble"),
    "booktabs",
    fixed = TRUE
  )
})
