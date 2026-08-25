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
