box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_identical,
    expect_true,
    test_that
  ]
)

box::use(artma / options / template[prompt_user_for_option_value])

readline_opt <- function(...) {
  utils::modifyList(
    list(name = "general.example", type = "character", prompt = "readline"),
    list(...)
  )
}

test_that("prompt_user_for_option_value returns a typed answer", {
  answer <- prompt_user_for_option_value(
    readline_opt(),
    read_input = function(prompt) "hello"
  )
  expect_equal(answer, "hello")
})

test_that("prompt_user_for_option_value strips quotes from a pasted path", {
  answer <- prompt_user_for_option_value(
    readline_opt(name = "data.source_path", prompt = "file"),
    read_input = function(prompt) "\"C:\\Users\\me\\data.xlsx\""
  )
  expect_equal(answer, "C:\\Users\\me\\data.xlsx")
})

test_that("prompt_user_for_option_value hands back the original default object", {
  answer <- prompt_user_for_option_value(
    readline_opt(name = "data.winsorization_level", type = "numeric", default = 0.01),
    read_input = function(prompt) ""
  )
  expect_identical(answer, 0.01)
})

test_that("prompt_user_for_option_value returns NA for a blank allow_na option", {
  answer <- prompt_user_for_option_value(
    readline_opt(allow_na = TRUE),
    read_input = function(prompt) ""
  )
  expect_true(is.na(answer))
})

test_that("prompt_user_for_option_value aborts when a required option stays blank", {
  expect_error(
    prompt_user_for_option_value(
      readline_opt(),
      read_input = function(prompt) ""
    ),
    "left blank"
  )
})

test_that("prompt_user_for_option_value dispatches 'choose' to the path picker", {
  picker_calls <- list()
  answer <- prompt_user_for_option_value(
    readline_opt(name = "data.source_path", prompt = "file"),
    read_input = function(prompt) "choose",
    choose_path = function(type, caption) {
      picker_calls[[length(picker_calls) + 1]] <<- list(type = type, caption = caption)
      "/tmp/data.csv"
    }
  )
  expect_equal(answer, "/tmp/data.csv")
  expect_equal(length(picker_calls), 1)
  expect_equal(picker_calls[[1]]$type, "file")
})

test_that("prompt_user_for_option_value re-asks after a cancelled picker", {
  # First attempt opens the picker and the user cancels; the second types a path.
  answers <- c("choose", "/tmp/typed.csv")
  attempt <- 0
  answer <- prompt_user_for_option_value(
    readline_opt(name = "data.source_path", prompt = "file"),
    read_input = function(prompt) {
      attempt <<- attempt + 1
      answers[[attempt]]
    },
    choose_path = function(type, caption) ""
  )
  expect_equal(answer, "/tmp/typed.csv")
  expect_equal(attempt, 2)
})

test_that("prompt_user_for_option_value never saves the literal 'choose'", {
  # Every attempt asks for a picker that keeps getting cancelled; the residual
  # "choose" must be blanked, which surfaces as the required-option abort.
  expect_error(
    prompt_user_for_option_value(
      readline_opt(name = "data.source_path", prompt = "file"),
      read_input = function(prompt) "choose",
      choose_path = function(type, caption) ""
    ),
    "left blank"
  )
})

test_that("prompt_user_for_option_value rejects an invalid prompt type", {
  expect_error(
    prompt_user_for_option_value(
      readline_opt(prompt = "nonsense"),
      read_input = function(prompt) "value"
    ),
    "Invalid prompt type"
  )
})

test_that("prompt_user_for_option_value aborts non-interactively with the default reader", {
  expect_error(
    prompt_user_for_option_value(readline_opt()),
    "non-interactive"
  )
})
