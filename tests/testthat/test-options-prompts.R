box::use(
  testthat[
    expect_equal,
    expect_identical,
    test_that
  ]
)

box::use(
  artma / options / prompts[
    prompt_autonomy_level,
    prompt_na_handling,
    prompt_se_zero_handling,
    prompt_winsorization_level
  ]
)

test_that("prompt_winsorization_level returns the numeric level", {
  value <- prompt_winsorization_level(select_fn = function(choices, prompt, selected, descriptions = NULL) "0.05")
  expect_identical(value, 0.05)
})

test_that("prompt_winsorization_level defaults to 1% on an empty selection", {
  value <- prompt_winsorization_level(select_fn = function(choices, prompt, selected, descriptions = NULL) NULL)
  expect_identical(value, 0.01)
})

test_that("prompt_na_handling returns the strategy behind the picked label", {
  value <- prompt_na_handling(
    select_fn = function(choices, prompt, selected, descriptions = NULL) {
      expect_equal(selected, 1) # "stop" preselected
      unname(choices[grepl("^Median", names(choices))])
    }
  )
  expect_equal(value, "median")
})

test_that("prompt_se_zero_handling defaults to remove on an empty selection", {
  value <- prompt_se_zero_handling(select_fn = function(choices, prompt, selected, descriptions = NULL) NULL)
  expect_equal(value, "remove")
})

test_that("prompt_autonomy_level returns the bare level name", {
  value <- prompt_autonomy_level(
    select_fn = function(choices, prompt, selected, descriptions = NULL) {
      unname(choices[grepl("^balanced", names(choices))])
    }
  )
  expect_equal(value, "balanced")
})
