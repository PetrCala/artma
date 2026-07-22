box::use(
  testthat[expect_identical, test_that]
)

box::use(
  artma / options / template[sanitize_prompt_input]
)

test_that("sanitize_prompt_input strips quotes around a pasted Windows path", {
  # Explorer's "Copy as path" wraps the path in double quotes; keeping them
  # makes every downstream file.exists() check fail on a file that is there.
  expect_identical(
    sanitize_prompt_input("\"C:\\Users\\me\\Documents\\data.xlsx\"", "file"),
    "C:\\Users\\me\\Documents\\data.xlsx"
  )
  expect_identical(
    sanitize_prompt_input("  '/home/me/data.csv'  ", "directory"),
    "/home/me/data.csv"
  )
})

test_that("sanitize_prompt_input trims whitespace for every prompt type", {
  expect_identical(sanitize_prompt_input("  0.05 ", "readline"), "0.05")
})

test_that("sanitize_prompt_input leaves quotes alone outside path prompts", {
  expect_identical(sanitize_prompt_input("\"quoted\"", "readline"), "\"quoted\"")
})

test_that("sanitize_prompt_input passes non-string input through unchanged", {
  expect_identical(sanitize_prompt_input(NA_character_, "file"), NA_character_)
  expect_identical(sanitize_prompt_input(character(0), "file"), character(0))
})
