box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_true,
    test_that
  ]
)

# A stand-in for a discovered method module: everything the table builder reads
# is the metadata attached to the module's `run` wrapper.
fake_module <- function(...) {
  box::use(artma / modules / runtime_methods[register_runtime_method])
  list(run = register_runtime_method(function(df, ...) df, ...))
}

fake_modules <- function() {
  list(
    fma = fake_module(
      stage = "fma",
      description = "Frequentist Model Averaging",
      depends_on = "bma",
      required_columns = c("effect", "se"),
      suggests = c("BMS", "quadprog")
    ),
    bma = fake_module(
      stage = "bma",
      description = "Bayesian Model Averaging",
      required_columns = c("effect", "se"),
      suggests = "BMS"
    ),
    robma = fake_module(
      stage = "robma",
      description = "Robust Bayesian meta-analysis",
      required_columns = c("effect", "se"),
      suggests = "RoBMA",
      opt_in = TRUE
    ),
    box_plot = fake_module(
      stage = "box_plot",
      description = "Box plots of the effect",
      required_columns = "effect"
    )
  )
}

# Only BMS and quadprog count as installed, so RoBMA is a missing package.
fake_is_installed <- function(pkg) pkg %in% c("BMS", "quadprog")

built_table <- function(...) {
  box::use(artma / modules / methods_table[build_methods_table])
  build_methods_table(modules = fake_modules(), is_installed = fake_is_installed, ...)
}

test_that("build_methods_table exposes the registered metadata per method", {
  df <- built_table()

  expect_equal(df$method, c("bma", "box_plot", "fma", "robma"))
  expect_equal(df$description[df$method == "bma"], "Bayesian Model Averaging")
  expect_equal(df$required_columns[df$method == "fma"], "effect, se")
  expect_equal(df$suggests[df$method == "fma"], "BMS, quadprog")
  expect_equal(df$depends_on[df$method == "fma"], "bma")
  expect_equal(df$depends_on[df$method == "bma"], "")
  expect_true(df$opt_in[df$method == "robma"])
  expect_false(df$opt_in[df$method == "bma"])
})

test_that("build_methods_table flags methods whose suggested packages are absent", {
  df <- built_table()

  expect_false(df$installed[df$method == "robma"])
  expect_equal(df$missing_packages[df$method == "robma"], "RoBMA")
  expect_true(df$installed[df$method == "fma"])
  expect_equal(df$missing_packages[df$method == "fma"], "")
  # A method with no suggested packages counts as installed.
  expect_true(df$installed[df$method == "box_plot"])
})

test_that("build_methods_table has no availability columns without a data frame", {
  df <- built_table()
  expect_false("missing_columns" %in% names(df))
  expect_false("available" %in% names(df))
})

test_that("available_for turns the table into a pre-flight check", {
  df <- built_table(available_for = data.frame(effect = 1, study_id = "a"))

  expect_equal(df$missing_columns[df$method == "bma"], "se")
  expect_false(df$available[df$method == "bma"])
  expect_equal(df$missing_columns[df$method == "box_plot"], "")
  expect_true(df$available[df$method == "box_plot"])
  # Columns present, package missing: still unavailable.
  expect_equal(df$missing_columns[df$method == "robma"], "se")
  expect_false(df$available[df$method == "robma"])
})

test_that("build_methods_table rejects a non-data-frame available_for", {
  expect_error(built_table(available_for = "effect"), "data frame")
})

test_that("methods_status condenses the reasons a method would not run", {
  box::use(artma / modules / methods_table[methods_status])

  status <- methods_status(built_table())
  expect_equal(status[built_table()$method == "bma"], "ok")
  expect_equal(status[built_table()$method == "robma"], "opt-in; install RoBMA")

  with_data <- built_table(available_for = data.frame(effect = 1))
  expect_equal(methods_status(with_data)[with_data$method == "bma"], "needs se")
})

test_that("format_methods_table fits an 80-column console, one line per method", {
  box::use(artma / modules / methods_table[format_methods_table])

  df <- built_table()
  lines <- format_methods_table(df, width = 80)

  # Header, rule, and one row per method.
  expect_equal(length(lines), nrow(df) + 2L)
  expect_true(all(nchar(lines) <= 80))
  expect_true(all(grepl("^bma ", lines[3])))
})

test_that("format_methods_table truncates the description instead of wrapping", {
  box::use(artma / modules / methods_table[format_methods_table])

  df <- built_table()
  df$description <- strrep("x", 200)
  lines <- format_methods_table(df, width = 80)

  expect_equal(length(lines), nrow(df) + 2L)
  expect_true(all(nchar(lines) <= 80))
  expect_false(any(grepl(strrep("x", 60), lines)))
})

test_that("format_methods_table gives spare width back to the description", {
  box::use(artma / modules / methods_table[format_methods_table])

  df <- built_table()
  narrow <- format_methods_table(df, width = 80)
  wide <- format_methods_table(df, width = 200)

  expect_true(all(nchar(wide) <= 200))
  expect_true(any(grepl("Robust Bayesian meta-analysis", wide, fixed = TRUE)))
  expect_false(any(grepl("Robust Bayesian meta-analysis", narrow, fixed = TRUE)))
})

test_that("fit_column_widths shrinks in order and never below the minimum", {
  box::use(artma / modules / methods_table[fit_column_widths])

  widths <- fit_column_widths(
    natural = list(a = 10L, b = 30L, c = 20L),
    minimum = list(a = 10L, b = 5L, c = 5L),
    shrink_order = c("b", "c"),
    available = 40L,
    gap = 2L
  )

  # 10 + 30 + 20 + 4 = 64, so 24 characters have to go: b absorbs 24 down to 6.
  expect_equal(widths$a, 10L)
  expect_equal(widths$b, 6L)
  expect_equal(widths$c, 20L)
})

test_that("fit_column_widths leaves a fitting row untouched", {
  box::use(artma / modules / methods_table[fit_column_widths])

  natural <- list(a = 10L, b = 12L)
  widths <- fit_column_widths(
    natural = natural,
    minimum = list(a = 5L, b = 5L),
    shrink_order = c("b", "a"),
    available = 80L
  )

  expect_equal(widths, natural)
})

test_that("every registered runtime method declares a one-line description", {
  box::use(
    artma / modules / runtime_methods[get_method_metadata, get_runtime_method_modules]
  )

  modules <- get_runtime_method_modules()
  for (name in names(modules)) {
    description <- get_method_metadata(modules[[name]][["run"]], name = name)$description
    expect_true(
      is.character(description) && length(description) == 1L &&
        !is.na(description) && nzchar(description),
      label = sprintf("Method '%s' registers a description", name)
    )
    expect_false(grepl("\n", description, fixed = TRUE))
  }
})

test_that("methods.list returns the table invisibly", {
  df <- withr::with_options(list(artma.verbose = 1), artma::methods.list())
  expect_true(is.data.frame(df))
  expect_true(all(c("method", "description", "required_columns", "installed") %in% names(df)))
  expect_true("bma" %in% df$method)
})
