box::use(testthat[
  test_that, expect_equal, expect_true, expect_false, expect_null, expect_error, expect_warning
])

test_that("get_autonomy_level returns NULL when not set", {
  box::use(artma / libs / core / autonomy[get_autonomy_level])

  withr::local_options(list(artma.autonomy.level = NULL))

  expect_null(get_autonomy_level())
})

test_that("set_autonomy_level sets and get_autonomy_level retrieves each valid level", {
  box::use(artma / libs / core / autonomy[set_autonomy_level, get_autonomy_level])

  withr::local_options(list(artma.autonomy.level = NULL))

  for (level in c("ask_more", "balanced", "autonomous")) {
    set_autonomy_level(level)
    expect_equal(get_autonomy_level(), level)
  }
})

test_that("set_autonomy_level translates legacy numeric levels with a warning", {
  box::use(artma / libs / core / autonomy[set_autonomy_level, get_autonomy_level])

  withr::local_options(list(artma.autonomy.level = NULL))

  expect_warning(set_autonomy_level(1), class = "rlang_warning")
  expect_equal(get_autonomy_level(), "ask_more")

  expect_warning(set_autonomy_level(2))
  expect_equal(get_autonomy_level(), "ask_more")

  expect_warning(set_autonomy_level(3))
  expect_equal(get_autonomy_level(), "balanced")

  expect_warning(set_autonomy_level(4))
  expect_equal(get_autonomy_level(), "autonomous")

  expect_warning(set_autonomy_level(5))
  expect_equal(get_autonomy_level(), "autonomous")
})

test_that("set_autonomy_level rejects invalid levels with a clear message", {
  box::use(artma / libs / core / autonomy[set_autonomy_level])

  expect_error(set_autonomy_level("invalid"), class = "validation_error")
  expect_error(set_autonomy_level(0), class = "validation_error")
  expect_error(set_autonomy_level(6), class = "validation_error")
  expect_error(set_autonomy_level(-1), class = "validation_error")
  expect_error(set_autonomy_level(1.5), class = "validation_error")
  expect_error(set_autonomy_level(c("ask_more", "balanced")), class = "validation_error")
  expect_error(set_autonomy_level(NULL), class = "validation_error")
})

test_that("is_autonomy_level_set works correctly", {
  box::use(artma / libs / core / autonomy[
    is_autonomy_level_set,
    set_autonomy_level
  ])

  withr::local_options(list(artma.autonomy.level = NULL))
  expect_false(is_autonomy_level_set())

  set_autonomy_level("balanced")
  expect_true(is_autonomy_level_set())
})

test_that("should_prompt_user respects autonomy level ordering", {
  box::use(artma / libs / core / autonomy[
    should_prompt_user,
    set_autonomy_level
  ])

  withr::local_options(list(artma.autonomy.level = NULL))

  # This test only works in interactive mode; in non-interactive mode,
  # should_prompt_user always returns FALSE regardless of the level.
  if (!interactive()) {
    skip("Test requires interactive mode")
  }

  levels <- c("ask_more", "balanced", "autonomous")
  ordinal <- stats::setNames(seq_along(levels), levels)

  for (current_level in levels) {
    set_autonomy_level(current_level)

    for (required_level in levels) {
      should_prompt <- should_prompt_user(required_level)
      expected <- ordinal[[current_level]] < ordinal[[required_level]]
      expect_equal(should_prompt, expected,
        info = paste0(
          "Level ", current_level, " with required ", required_level,
          " should prompt: ", expected
        )
      )
    }
  }
})

test_that("should_prompt_user returns TRUE when level not set (interactive mode)", {
  box::use(artma / libs / core / autonomy[should_prompt_user])

  withr::local_options(list(artma.autonomy.level = NULL))

  if (interactive()) {
    expect_true(should_prompt_user(required_level = "ask_more"))
    expect_true(should_prompt_user(required_level = "autonomous"))
  }
})

test_that("should_prompt_user returns FALSE in non-interactive mode regardless of level", {
  box::use(artma / libs / core / autonomy[should_prompt_user, set_autonomy_level])

  withr::local_options(list(artma.autonomy.level = NULL))
  set_autonomy_level("ask_more")

  # The test runner itself is non-interactive, so should_prompt_user should
  # already return FALSE here without needing to mock interactive().
  expect_false(should_prompt_user(required_level = "ask_more"))
  expect_false(should_prompt_user(required_level = "autonomous"))
})

test_that("should_prompt_user validates required_level", {
  box::use(artma / libs / core / autonomy[should_prompt_user])

  expect_error(should_prompt_user(required_level = "invalid"), class = "validation_error")
  expect_error(should_prompt_user(required_level = 4), class = "validation_error")
})

test_that("get_default_autonomy_level returns 'autonomous'", {
  box::use(artma / libs / core / autonomy[get_default_autonomy_level])

  expect_equal(get_default_autonomy_level(), "autonomous")
})

test_that("public API functions work correctly", {
  box::use(artma[autonomy.get, autonomy.set, autonomy.is_set, autonomy.is_full])

  withr::local_options(list(artma.autonomy.level = NULL))

  # Test autonomy.get
  expect_null(autonomy.get())

  # Test autonomy.set
  autonomy.set("balanced")
  expect_equal(autonomy.get(), "balanced")

  # Test autonomy.is_set
  expect_true(autonomy.is_set())
  options(artma.autonomy.level = NULL)
  expect_false(autonomy.is_set())

  # Test autonomy.is_full: non-interactive sessions are always fully autonomous
  autonomy.set("autonomous")
  expect_true(autonomy.is_full())
  autonomy.set("balanced")
  expect_equal(autonomy.is_full(), !interactive())
})

test_that("public API accepts legacy numeric levels with a warning", {
  box::use(artma[autonomy.get, autonomy.set])

  withr::local_options(list(artma.autonomy.level = NULL))

  expect_warning(autonomy.set(4))
  expect_equal(autonomy.get(), "autonomous")
})
