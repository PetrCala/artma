box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_true,
    test_that
  ]
)

box::use(artma / interactive / input[ask_text])

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
