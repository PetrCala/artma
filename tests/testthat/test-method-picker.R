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

test_that("method_status_markers flags missing columns, missing packages, and opt-in", {
  markers <- method_status_markers(picker_frame())
  expect_equal(markers, c("", "", "needs: n_obs", "install RoBMA . opt-in"))
})

test_that("method_status_markers works without the availability columns", {
  df <- picker_frame()
  df$missing_columns <- NULL
  expect_equal(method_status_markers(df), c("", "", "", "install RoBMA . opt-in"))
})

test_that("compose_method_choices pairs plain method names with marked descriptions", {
  composed <- compose_method_choices(picker_frame())

  expect_equal(unname(composed$choices), c("bma", "funnel_plot", "pub_bias", "robma"))
  expect_equal(names(composed$choices), c("bma", "funnel_plot", "pub_bias", "robma"))

  descriptions <- composed$descriptions
  expect_equal(descriptions[[1]], "Bayesian model averaging over moderators")
  expect_true(grepl("[needs: n_obs]", descriptions[[3]], fixed = TRUE))
  expect_true(grepl("[install RoBMA . opt-in]", descriptions[[4]], fixed = TRUE))
  # Clean methods carry no status marker.
  expect_false(grepl("needs|install|opt-in", descriptions[[1]]))
  # The backend renders and echoes these; they must reach it unstyled.
  expect_false(any(grepl("\033", c(names(composed$choices), descriptions), fixed = TRUE)))
})

test_that("compose_method_choices keeps an NA description empty", {
  df <- picker_frame()
  df$description[[1]] <- NA_character_
  composed <- compose_method_choices(df)
  expect_equal(composed$descriptions[[1]], "")
})

test_that("compose_method_choices rejects an empty frame", {
  expect_error(
    compose_method_choices(picker_frame()[0, ]),
    "at least one method"
  )
})

test_that("ask_runtime_methods returns unnamed method names", {
  selected <- ask_runtime_methods(
    picker_frame(),
    checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions = NULL) {
      choices[c(1, 4)]
    }
  )
  expect_equal(selected, c("bma", "robma"))
})

test_that("ask_runtime_methods hands the descriptions to the backend", {
  seen <- NULL
  ask_runtime_methods(
    picker_frame(),
    checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions = NULL) {
      seen <<- descriptions
      character(0)
    }
  )
  expect_true(grepl("[needs: n_obs]", seen[[3]], fixed = TRUE))
})

test_that("ask_runtime_methods preselects the previous selection by value", {
  seen <- NULL
  ask_runtime_methods(
    picker_frame(),
    default = c("pub_bias", "bma", "not_a_method"),
    checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions = NULL) {
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
    checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions = NULL) NULL
  )
  expect_equal(selected, character(0))
})
