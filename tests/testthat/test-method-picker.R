box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_true,
    test_that
  ]
)

box::use(
  artma / interactive / method_picker[
    ask_runtime_methods,
    compose_method_choices,
    method_status_markers
  ]
)

# The columns of a build_methods_table(available_for = ...) frame the picker
# reads, with one method per preflight state.
picker_frame <- function() {
  data.frame(
    method = c("bma", "funnel_plot", "pub_bias", "robma"),
    description = c(
      "Bayesian model averaging over moderators",
      "Funnel plot of effects against precision",
      "Publication bias tests",
      "Robust Bayesian meta-analysis"
    ),
    missing_packages = c("", "", "", "RoBMA"),
    opt_in = c(FALSE, FALSE, FALSE, TRUE),
    missing_columns = c("", "", "n_obs", ""),
    stringsAsFactors = FALSE
  )
}

plain_labels <- function(choices) cli::ansi_strip(names(choices))

test_that("method_status_markers flags missing columns, missing packages, and opt-in", {
  markers <- method_status_markers(picker_frame())
  expect_equal(markers, c("", "", "needs: n_obs", "install RoBMA . opt-in"))
})

test_that("method_status_markers works without the availability columns", {
  df <- picker_frame()
  df$missing_columns <- NULL
  expect_equal(method_status_markers(df), c("", "", "", "install RoBMA . opt-in"))
})

test_that("compose_method_choices is value-keyed with decorated labels", {
  choices <- compose_method_choices(picker_frame(), width = 100)

  expect_equal(unname(choices), c("bma", "funnel_plot", "pub_bias", "robma"))

  labels <- plain_labels(choices)
  # Method names are padded to one fixed column, descriptions to the next.
  expect_true(grepl("^bma          Bayesian model averaging", labels[[1]]))
  expect_true(grepl("^funnel_plot  Funnel plot", labels[[2]]))
  expect_true(grepl("needs: n_obs$", labels[[3]]))
  expect_true(grepl("install RoBMA . opt-in", labels[[4]], fixed = TRUE))
  # Clean methods carry no status marker.
  expect_false(grepl("needs|install|opt-in", labels[[1]]))
})

test_that("compose_method_choices truncates descriptions to the console width", {
  df <- picker_frame()
  df$description <- strrep("x", 200)
  choices <- compose_method_choices(df, width = 80)

  labels <- plain_labels(choices)
  expect_true(all(nchar(labels) <= 80))
  expect_true(any(grepl(cli::symbol$ellipsis, labels, fixed = TRUE)))
  # The status markers survive the truncation.
  expect_true(grepl("needs: n_obs$", labels[[3]]))
})

test_that("compose_method_choices rejects an empty frame", {
  expect_error(
    compose_method_choices(picker_frame()[0, ]),
    "at least one method"
  )
})

test_that("ask_runtime_methods returns method names, never labels", {
  selected <- ask_runtime_methods(
    picker_frame(),
    width = 100,
    checkbox_fn = function(choices, prompt, selected, allow_select_all) {
      choices[c(1, 4)]
    }
  )
  expect_equal(selected, c("bma", "robma"))
})

test_that("ask_runtime_methods preselects the previous selection by value", {
  seen <- NULL
  ask_runtime_methods(
    picker_frame(),
    default = c("pub_bias", "bma", "not_a_method"),
    width = 100,
    checkbox_fn = function(choices, prompt, selected, allow_select_all) {
      seen <<- list(selected = selected, allow_select_all = allow_select_all)
      character(0)
    }
  )
  # Unknown names are dropped; the rest map to their choice indices.
  expect_equal(seen$selected, c(3L, 1L))
  expect_true(seen$allow_select_all)
})

test_that("ask_runtime_methods returns character(0) on an empty confirmation", {
  selected <- ask_runtime_methods(
    picker_frame(),
    width = 100,
    checkbox_fn = function(choices, prompt, selected, allow_select_all) NULL
  )
  expect_equal(selected, character(0))
})
