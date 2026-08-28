box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(artma / interactive / input[ask_checkbox, ask_select, ask_text, ask_yes_no])

test_that("ask_text returns the trimmed answer", {
  answer <- ask_text("Name", read_input = function(prompt) "  hello  ")
  expect_equal(answer, "hello")
})

test_that("ask_text falls back to the default on Enter and shows it in the input row", {
  seen_prompt <- NULL
  answer <- ask_text(
    "Name",
    default = "my_analysis",
    read_input = function(prompt) {
      seen_prompt <<- prompt
      ""
    }
  )
  expect_equal(answer, "my_analysis")
  expect_true(grepl("my_analysis", seen_prompt, fixed = TRUE))
})

test_that("ask_text prefers a typed answer over the default", {
  answer <- ask_text("Name", default = "my_analysis", read_input = function(prompt) "custom")
  expect_equal(answer, "custom")
})

test_that("ask_text re-asks on empty input until a value arrives", {
  answers <- c("", "", "value")
  attempt <- 0
  answer <- ask_text("Name", read_input = function(prompt) {
    attempt <<- attempt + 1
    answers[[attempt]]
  })
  expect_equal(answer, "value")
  expect_equal(attempt, 3)
})

test_that("ask_text gives up with an empty string after max_retries attempts", {
  attempt <- 0
  answer <- ask_text("Name", max_retries = 2, read_input = function(prompt) {
    attempt <<- attempt + 1
    ""
  })
  expect_equal(answer, "")
  expect_equal(attempt, 2)
})

test_that("ask_text accepts an empty answer when allow_empty is set", {
  attempt <- 0
  answer <- ask_text("Name", allow_empty = TRUE, read_input = function(prompt) {
    attempt <<- attempt + 1
    ""
  })
  expect_equal(answer, "")
  expect_equal(attempt, 1)
})

test_that("ask_text aborts in non-interactive sessions with the default reader", {
  expect_error(ask_text("Name"), "interactive")
})

test_that("ask_text applies sanitize before the default substitution", {
  # A sanitizer that strips an answer down to nothing must fall through to the
  # default, matching the quoted-path behavior of option prompts.
  answer <- ask_text(
    "Path",
    default = "fallback",
    sanitize = function(x) gsub("\"", "", x),
    read_input = function(prompt) "\"\""
  )
  expect_equal(answer, "fallback")
})

test_that("ask_text passes the sanitized answer to validate", {
  seen <- NULL
  answer <- ask_text(
    "Name",
    sanitize = toupper,
    validate = function(x) {
      seen <<- x
      NULL
    },
    read_input = function(prompt) "abc"
  )
  expect_equal(answer, "ABC")
  expect_equal(seen, "ABC")
})

test_that("ask_text re-asks on a validation failure and accepts a later answer", {
  answers <- c("bad", "good")
  attempt <- 0
  answer <- ask_text(
    "Name",
    validate = function(x) if (x == "bad") "Not a valid name." else NULL,
    read_input = function(prompt) {
      attempt <<- attempt + 1
      answers[[attempt]]
    }
  )
  expect_equal(answer, "good")
  expect_equal(attempt, 2)
})

test_that("ask_text gives up with an empty string after repeated validation failures", {
  attempt <- 0
  answer <- ask_text(
    "Name",
    max_retries = 2,
    validate = function(x) "Never valid.",
    read_input = function(prompt) {
      attempt <<- attempt + 1
      "value"
    }
  )
  expect_equal(answer, "")
  expect_equal(attempt, 2)
})

test_that("ask_text does not validate the default or an allowed empty answer", {
  validate_calls <- 0
  counting_validate <- function(x) {
    validate_calls <<- validate_calls + 1
    "Rejected."
  }

  answer <- ask_text(
    "Name",
    default = "my_default",
    validate = counting_validate,
    read_input = function(prompt) ""
  )
  expect_equal(answer, "my_default")

  answer <- ask_text(
    "Name",
    allow_empty = TRUE,
    validate = counting_validate,
    read_input = function(prompt) ""
  )
  expect_equal(answer, "")
  expect_equal(validate_calls, 0)
})

test_that("ask_select hands the named vector to the backend and returns its value", {
  choices <- c("1% (default)" = "0.01", "5%" = "0.05")
  seen <- NULL
  answer <- ask_select(
    "Winsorization level",
    choices = choices,
    default = "0.01",
    select_fn = function(choices, prompt, selected, descriptions = NULL) {
      seen <<- list(choices = choices, prompt = prompt, selected = selected)
      "0.05"
    }
  )
  expect_equal(answer, "0.05")
  expect_equal(seen$choices, c("1% (default)" = "0.01", "5%" = "0.05"))
  expect_equal(seen$prompt, "Winsorization level")
  expect_equal(seen$selected, 1)
})

test_that("ask_select forwards the descriptions column to the backend", {
  seen <- NULL
  ask_select(
    "Preview data",
    choices = c("Studies" = "studies", "Back" = "back"),
    descriptions = c("per-study estimate counts", ""),
    select_fn = function(choices, prompt, selected, descriptions = NULL) {
      seen <<- descriptions
      "back"
    }
  )
  expect_equal(seen, c("per-study estimate counts", ""))
})

test_that("ask_select passes plain vectors through unchanged", {
  answer <- ask_select(
    "Theme",
    choices = c("light", "dark"),
    select_fn = function(choices, prompt, selected, descriptions = NULL) {
      expect_null(selected)
      "dark"
    }
  )
  expect_equal(answer, "dark")
})

test_that("ask_select falls back to the default on an empty selection", {
  for (empty in list(NULL, character(0))) {
    answer <- ask_select(
      "Strategy",
      choices = c("Stop" = "stop", "Remove" = "remove"),
      default = "stop",
      select_fn = function(choices, prompt, selected, descriptions = NULL) empty
    )
    expect_equal(answer, "stop")
  }
})

test_that("ask_select strips a named value returned by the backend", {
  answer <- ask_select(
    "Strategy",
    choices = c("Stop" = "stop", "Remove" = "remove"),
    select_fn = function(choices, prompt, selected, descriptions = NULL) choices[1]
  )
  expect_equal(answer, "stop")
})

test_that("ask_select returns character(0) on an empty selection without a default", {
  answer <- ask_select(
    "Strategy",
    choices = c("stop", "remove"),
    select_fn = function(choices, prompt, selected, descriptions = NULL) NULL
  )
  expect_equal(answer, character(0))
})

test_that("ask_select rejects a default that is not one of the choices", {
  expect_error(
    ask_select("Q", choices = c("a", "b"), default = "c", select_fn = function(...) "a")
  )
})

test_that("ask_select aborts in non-interactive sessions with the default backend", {
  expect_error(ask_select("Q", choices = c("a", "b")), "interactive")
})

test_that("ask_checkbox hands the named vector to the backend and returns its values", {
  choices <- c("Bayesian model averaging" = "bma", "Linear tests" = "linear_tests", "MAIVE" = "maive")
  seen <- NULL
  answer <- ask_checkbox(
    "Pick methods",
    choices = choices,
    checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions = NULL) {
      seen <<- list(choices = choices, prompt = prompt, selected = selected, allow_select_all = allow_select_all)
      c("bma", "maive")
    }
  )
  expect_equal(answer, c("bma", "maive"))
  expect_equal(seen$choices, choices)
  expect_equal(seen$prompt, "Pick methods")
  expect_null(seen$selected)
  expect_false(seen$allow_select_all)
})

test_that("ask_checkbox forwards the descriptions column to the backend", {
  seen <- NULL
  ask_checkbox(
    "Pick methods",
    choices = c("BMA" = "bma", "MAIVE" = "maive"),
    descriptions = c("Bayesian model averaging", "spurious-precision robust"),
    checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions = NULL) {
      seen <<- descriptions
      character(0)
    }
  )
  expect_equal(seen, c("Bayesian model averaging", "spurious-precision robust"))
})

test_that("ask_checkbox passes plain vectors through unchanged", {
  answer <- ask_checkbox(
    "Pick methods",
    choices = c("bma", "maive"),
    allow_select_all = TRUE,
    checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions = NULL) {
      expect_true(allow_select_all)
      choices
    }
  )
  expect_equal(answer, c("bma", "maive"))
})

test_that("ask_checkbox preselects the defaults by value", {
  seen_selected <- NULL
  ask_checkbox(
    "Pick methods",
    choices = c("BMA" = "bma", "Linear tests" = "linear_tests", "MAIVE" = "maive"),
    default = c("bma", "maive"),
    checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions = NULL) {
      seen_selected <<- selected
      character(0)
    }
  )
  expect_equal(seen_selected, c(1, 3))
})

test_that("ask_checkbox returns character(0) on a cancelled or empty confirmation", {
  for (empty in list(NULL, character(0))) {
    answer <- ask_checkbox(
      "Pick methods",
      choices = c("BMA" = "bma", "MAIVE" = "maive"),
      default = "bma",
      checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions = NULL) empty
    )
    expect_equal(answer, character(0))
  }
})

test_that("ask_checkbox rejects defaults that are not among the choices", {
  expect_error(
    ask_checkbox(
      "Pick methods",
      choices = c("a", "b"),
      default = c("a", "c"),
      checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions = NULL) "a"
    )
  )
})

test_that("ask_checkbox rejects choices with duplicate labels", {
  expect_error(
    ask_checkbox(
      "Pick methods",
      choices = c("Same" = "a", "Same" = "b"),
      checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions = NULL) "a"
    )
  )
})

test_that("ask_checkbox aborts in non-interactive sessions with the default backend", {
  expect_error(ask_checkbox("Pick methods", choices = c("a", "b")), "interactive")
})

test_that("ask_yes_no returns a logical and honors the default", {
  expect_true(ask_yes_no("Proceed?", select_fn = function(choices, prompt, selected, descriptions = NULL) "yes"))
  expect_false(ask_yes_no("Proceed?", select_fn = function(choices, prompt, selected, descriptions = NULL) "no"))

  # Empty selection falls back to the default.
  expect_false(ask_yes_no("Proceed?", select_fn = function(choices, prompt, selected, descriptions = NULL) NULL))
  expect_true(ask_yes_no("Proceed?", default = TRUE, select_fn = function(choices, prompt, selected, descriptions = NULL) NULL))
})

test_that("ask_yes_no preselects the default choice", {
  seen_selected <- NULL
  ask_yes_no("Proceed?", default = TRUE, select_fn = function(choices, prompt, selected, descriptions = NULL) {
    seen_selected <<- selected
    "no"
  })
  expect_equal(seen_selected, 1)

  ask_yes_no("Proceed?", default = FALSE, select_fn = function(choices, prompt, selected, descriptions = NULL) {
    seen_selected <<- selected
    "yes"
  })
  expect_equal(seen_selected, 2)
})
