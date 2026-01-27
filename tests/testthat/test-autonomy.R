box::use(testthat[
  test_that, expect_equal, expect_true, expect_false, expect_null, expect_error
])

test_that("get_autonomy_levels returns all 5 levels", {
  box::use(artma / libs / core / autonomy[get_autonomy_levels])

  levels <- get_autonomy_levels()

  expect_equal(length(levels), 5)
  expect_true(all(c("1", "2", "3", "4", "5") %in% names(levels)))

  # Check structure of each level
  for (level_name in names(levels)) {
    level <- levels[[level_name]]
    expect_true("name" %in% names(level))
    expect_true("description" %in% names(level))
    expect_true("prompt_frequency" %in% names(level))
  }
})

test_that("get_autonomy_level returns NULL when not set (interactive mode)", {
  box::use(artma / libs / core / autonomy[get_autonomy_level])

  # Clear any existing autonomy level
  options(artma.autonomy.level = NULL)

  # In interactive mode, should return NULL when not set
  # In non-interactive mode, returns 5
  if (interactive()) {
    level <- get_autonomy_level()
    expect_null(level)
  } else {
    level <- get_autonomy_level()
    expect_equal(level, 5L)
  }
})

test_that("set_autonomy_level sets and get_autonomy_level retrieves", {
  box::use(artma / libs / core / autonomy[set_autonomy_level, get_autonomy_level])

  # Test each valid level
  for (level in 1:5) {
    set_autonomy_level(level)
    retrieved <- get_autonomy_level()
    expect_equal(retrieved, as.integer(level))
  }

  # Clean up
  options(artma.autonomy.level = NULL)
})

test_that("set_autonomy_level validates input", {
  box::use(artma / libs / core / autonomy[set_autonomy_level])

  # Invalid levels should error with validation_error class
  expect_error(set_autonomy_level(0), class = "validation_error")
  expect_error(set_autonomy_level(6), class = "validation_error")
  expect_error(set_autonomy_level(-1), class = "validation_error")
  expect_error(set_autonomy_level(1.5), class = "validation_error")
  expect_error(set_autonomy_level(c(1, 2)), class = "validation_error")
})

test_that("is_autonomy_level_set works correctly", {
  box::use(artma / libs / core / autonomy[
    is_autonomy_level_set,
    set_autonomy_level
  ])

  # Clear level
  options(artma.autonomy.level = NULL)
  # In non-interactive mode, get_autonomy_level returns 5 when not set
  # So is_autonomy_level_set will return TRUE in non-interactive mode
  if (interactive()) {
    expect_false(is_autonomy_level_set())
  } else {
    expect_true(is_autonomy_level_set()) # Returns 5 in non-interactive mode
  }

  # Set level
  set_autonomy_level(4)
  expect_true(is_autonomy_level_set())

  # Clean up
  options(artma.autonomy.level = NULL)
})

test_that("should_prompt_user respects autonomy levels", {
  box::use(artma / libs / core / autonomy[
    should_prompt_user,
    set_autonomy_level
  ])

  # This test only works in interactive mode
  # In non-interactive mode, should_prompt_user always returns FALSE
  if (!interactive()) {
    skip("Test requires interactive mode")
  }

  # Test each level
  for (current_level in 1:5) {
    set_autonomy_level(current_level)

    # Should prompt if required_level > current_level
    for (required_level in 1:5) {
      should_prompt <- should_prompt_user(required_level)
      expected <- current_level < required_level
      expect_equal(should_prompt, expected,
        info = paste0(
          "Level ", current_level, " with required ", required_level,
          " should prompt: ", expected
        )
      )
    }
  }

  # Clean up
  options(artma.autonomy.level = NULL)
})

test_that("should_prompt_user returns TRUE when level not set (interactive mode)", {
  box::use(artma / libs / core / autonomy[should_prompt_user])

  # Clear level
  options(artma.autonomy.level = NULL)

  # In interactive mode, should prompt if level not set
  if (interactive()) {
    expect_true(should_prompt_user(required_level = 1))
    expect_true(should_prompt_user(required_level = 5))
  }
})

test_that("should_prompt_user returns FALSE in non-interactive mode", {
  box::use(artma / libs / core / autonomy[should_prompt_user, set_autonomy_level])

  # Set a low level
  set_autonomy_level(1)

  # Mock non-interactive mode
  withr::with_options(
    list(rlang_interactive = FALSE),
    {
      # Should never prompt in non-interactive mode
      expect_false(should_prompt_user(required_level = 1))
      expect_false(should_prompt_user(required_level = 5))
    }
  )

  # Clean up
  options(artma.autonomy.level = NULL)
})

test_that("get_autonomy_description works for all levels", {
  box::use(artma / libs / core / autonomy[
    get_autonomy_description,
    get_autonomy_levels,
    set_autonomy_level
  ])

  levels <- get_autonomy_levels()

  # Test with explicit level
  for (level_num in 1:5) {
    desc <- get_autonomy_description(level_num)
    expect_true(is.character(desc))
    expect_true(nchar(desc) > 0)
    expect_true(grepl(paste0("Level ", level_num), desc))
  }

  # Test with current level
  set_autonomy_level(3)
  desc <- get_autonomy_description()
  expect_true(is.character(desc))
  expect_true(grepl("Level 3", desc))

  # Clean up
  options(artma.autonomy.level = NULL)
})

test_that("get_autonomy_description returns 'Not set' when level is NULL (interactive mode)", {
  box::use(artma / libs / core / autonomy[get_autonomy_description])

  # Clear level
  options(artma.autonomy.level = NULL)

  desc <- get_autonomy_description()
  # In interactive mode, returns "Not set"
  # In non-interactive mode, get_autonomy_level returns 5, so description is for level 5
  if (interactive()) {
    expect_equal(desc, "Not set")
  } else {
    expect_true(grepl("Level 5", desc))
  }
})

test_that("is_fully_autonomous works correctly", {
  box::use(artma / libs / core / autonomy[
    is_fully_autonomous,
    set_autonomy_level
  ])

  # Clear level
  options(artma.autonomy.level = NULL)
  # In non-interactive mode, get_autonomy_level returns 5 when not set
  if (interactive()) {
    expect_false(is_fully_autonomous())
  } else {
    expect_true(is_fully_autonomous()) # Returns 5 in non-interactive mode
  }

  # Set to level 5
  set_autonomy_level(5)
  expect_true(is_fully_autonomous())

  # Set to level 4
  set_autonomy_level(4)
  expect_false(is_fully_autonomous())

  # Clean up
  options(artma.autonomy.level = NULL)
})

test_that("get_default_autonomy_level returns 4", {
  box::use(artma / libs / core / autonomy[get_default_autonomy_level])

  default <- get_default_autonomy_level()
  expect_equal(default, 4L)
})

test_that("get_autonomy_level returns 5 in non-interactive mode when not set", {
  box::use(artma / libs / core / autonomy[get_autonomy_level])

  # Clear level
  options(artma.autonomy.level = NULL)

  # Mock non-interactive mode
  withr::with_options(
    list(rlang_interactive = FALSE),
    {
      level <- get_autonomy_level()
      expect_equal(level, 5L)
    }
  )
})

test_that("public API functions work correctly", {
  box::use(artma[autonomy.get, autonomy.set, autonomy.is_set, autonomy.is_full, autonomy.describe, autonomy.levels])

  # Test autonomy.get
  options(artma.autonomy.level = NULL)
  # In non-interactive mode, returns 5 when not set
  if (interactive()) {
    expect_null(autonomy.get())
  } else {
    expect_equal(autonomy.get(), 5L)
  }

  # Test autonomy.set
  autonomy.set(3)
  expect_equal(autonomy.get(), 3L)

  # Test autonomy.is_set
  expect_true(autonomy.is_set())
  options(artma.autonomy.level = NULL)
  # In non-interactive mode, returns TRUE because get_autonomy_level returns 5
  if (interactive()) {
    expect_false(autonomy.is_set())
  } else {
    expect_true(autonomy.is_set())
  }

  # Test autonomy.is_full
  autonomy.set(5)
  expect_true(autonomy.is_full())
  autonomy.set(4)
  expect_false(autonomy.is_full())

  # Test autonomy.describe
  desc <- autonomy.describe(2)
  expect_true(is.character(desc))
  expect_true(grepl("Level 2", desc))

  # Test autonomy.levels
  levels <- autonomy.levels()
  expect_equal(length(levels), 5)

  # Clean up
  options(artma.autonomy.level = NULL)
})
