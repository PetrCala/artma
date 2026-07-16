box::use(
  testthat[
    expect_error,
    expect_false,
    expect_true,
    test_that
  ],
  withr[local_options, local_tempdir],
  artma / interactive / ask[ask_for_overwrite_permission],
  artma / interactive / save_preference[
    prompt_save_preference,
    prompt_save_variable_selection
  ],
  artma / interactive / welcome[is_first_time_user, mark_welcome_as_shown]
)

# ask_for_overwrite_permission ----------------------------------------------

test_that("ask_for_overwrite_permission allows writing a non-existent file", {
  local_options(artma.verbose = 1)
  expect_true(ask_for_overwrite_permission(tempfile()))
})

test_that("ask_for_overwrite_permission honours an explicit should_overwrite", {
  local_options(artma.verbose = 1)
  path <- tempfile()
  writeLines("x", path)

  expect_true(ask_for_overwrite_permission(path, should_overwrite = TRUE))
  expect_error(ask_for_overwrite_permission(path, should_overwrite = FALSE))
})

test_that("ask_for_overwrite_permission aborts rather than prompting when non-interactive", {
  local_options(artma.verbose = 1)
  path <- tempfile()
  writeLines("x", path)

  # The test runner is non-interactive, so the interactive prompt path aborts.
  expect_error(ask_for_overwrite_permission(path))
})

# prompt_save_preference ----------------------------------------------------

test_that("prompt_save_preference does not save in non-interactive mode", {
  local_options(artma.verbose = 1)
  expect_false(prompt_save_preference("data.example", 1, description = "example"))
})

test_that("prompt_save_preference validates its arguments", {
  local_options(artma.verbose = 1)
  expect_error(prompt_save_preference(123, 1))
  expect_error(prompt_save_preference("data.example", 1, respect_autonomy = "no"))
})

# prompt_save_variable_selection --------------------------------------------

test_that("prompt_save_variable_selection short-circuits on empty input", {
  local_options(artma.verbose = 1)
  expect_false(prompt_save_variable_selection(character(0)))
})

test_that("prompt_save_variable_selection does not save in non-interactive mode", {
  local_options(artma.verbose = 1)
  expect_false(prompt_save_variable_selection(c("effect", "se")))
})

test_that("prompt_save_variable_selection validates its arguments", {
  local_options(artma.verbose = 1)
  expect_error(prompt_save_variable_selection(123))
})

# welcome -------------------------------------------------------------------

test_that("is_first_time_user returns FALSE once the session flag is set", {
  local_options(artma.welcome.shown = TRUE)
  expect_false(is_first_time_user(local_tempdir()))
})

test_that("is_first_time_user is TRUE for a fresh options directory", {
  local_options(artma.welcome.shown = NULL)
  expect_true(is_first_time_user(local_tempdir()))
})

test_that("mark_welcome_as_shown creates a flag file and is then not first-time", {
  local_options(artma.welcome.shown = NULL)
  dir <- local_tempdir()

  mark_welcome_as_shown(dir)

  expect_true(file.exists(file.path(dir, ".welcome_shown")))
  expect_true(isTRUE(getOption("artma.welcome.shown")))

  # A fresh session (option cleared) still sees the persistent flag file.
  local_options(artma.welcome.shown = NULL)
  expect_false(is_first_time_user(dir))
})
