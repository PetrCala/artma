box::use(testthat[
  test_that,
  expect_equal,
  expect_true,
  expect_false,
  expect_null,
  expect_error,
  expect_type,
  expect_length
])

test_that("autonomy level can be set and retrieved", {
  box::use(artma / libs / autonomy[
    get_autonomy_level,
    set_autonomy_level,
    is_autonomy_level_set
  ])

  # Initially not set
  withr::local_options(list("artma.autonomy.level" = NULL))
  expect_null(get_autonomy_level())
  expect_false(is_autonomy_level_set())

  # Set to level 3
  set_autonomy_level(3)
  expect_equal(get_autonomy_level(), 3L)
  expect_true(is_autonomy_level_set())

  # Set to level 5
  set_autonomy_level(5)
  expect_equal(get_autonomy_level(), 5L)

  # Set to level 1
  set_autonomy_level(1)
  expect_equal(get_autonomy_level(), 1L)
})

test_that("autonomy level validation works", {
  box::use(artma / libs / autonomy[set_autonomy_level])

  # Invalid levels should error
  expect_error(set_autonomy_level(0))
  expect_error(set_autonomy_level(6))
  expect_error(set_autonomy_level(-1))
  expect_error(set_autonomy_level(3.5))
  expect_error(set_autonomy_level("high"))
  expect_error(set_autonomy_level(c(3, 4)))
})

test_that("autonomy levels definition is correct", {
  box::use(artma / libs / autonomy[get_autonomy_levels])

  levels <- get_autonomy_levels()

  expect_type(levels, "list")
  expect_length(levels, 5)

  # Check all levels are defined
  for (i in 1:5) {
    level_def <- levels[[as.character(i)]]
    expect_true("name" %in% names(level_def))
    expect_true("description" %in% names(level_def))
    expect_true("prompt_frequency" %in% names(level_def))
    expect_type(level_def$name, "character")
    expect_type(level_def$description, "character")
    expect_type(level_def$prompt_frequency, "character")
  }

  # Check specific level names
  expect_equal(levels[["1"]]$name, "Minimal")
  expect_equal(levels[["2"]]$name, "Low")
  expect_equal(levels[["3"]]$name, "Medium")
  expect_equal(levels[["4"]]$name, "High")
  expect_equal(levels[["5"]]$name, "Full")
})

test_that("autonomy description is generated correctly", {
  box::use(artma / libs / autonomy[
    get_autonomy_description,
    set_autonomy_level
  ])

  withr::local_options(list("artma.autonomy.level" = NULL))

  # No level set
  expect_equal(get_autonomy_description(), "Not set")

  # Level 4 set
  set_autonomy_level(4)
  desc <- get_autonomy_description()
  expect_type(desc, "character")
  expect_true(grepl("Level 4", desc))
  expect_true(grepl("High", desc))

  # Get description for specific level
  desc5 <- get_autonomy_description(5)
  expect_type(desc5, "character")
  expect_true(grepl("Level 5", desc5))
  expect_true(grepl("Full", desc5))
})

test_that("should_prompt_user logic is correct", {
  box::use(artma / libs / autonomy[
    should_prompt_user,
    set_autonomy_level
  ])

  withr::local_options(list("artma.autonomy.level" = NULL))

  # Note: Tests typically run in non-interactive mode
  # In non-interactive mode, should_prompt_user always returns FALSE
  # We test the logic but account for the non-interactive environment

  if (interactive()) {
    # In interactive mode, test prompting logic

    # No level set - should prompt
    expect_true(should_prompt_user(required_level = 4))

    # Level 3 set, required 4 - should prompt
    set_autonomy_level(3)
    expect_true(should_prompt_user(required_level = 4))

    # Level 4 set, required 4 - should not prompt
    set_autonomy_level(4)
    expect_false(should_prompt_user(required_level = 4))

    # Level 5 set, required 4 - should not prompt
    set_autonomy_level(5)
    expect_false(should_prompt_user(required_level = 4))

    # Level 5 set, required 3 - should not prompt
    expect_false(should_prompt_user(required_level = 3))

    # Level 2 set, required 3 - should prompt
    set_autonomy_level(2)
    expect_true(should_prompt_user(required_level = 3))
  } else {
    # In non-interactive mode, should always be FALSE regardless of level
    expect_false(should_prompt_user(required_level = 4))

    set_autonomy_level(1)
    expect_false(should_prompt_user(required_level = 5))

    set_autonomy_level(3)
    expect_false(should_prompt_user(required_level = 4))

    set_autonomy_level(5)
    expect_false(should_prompt_user(required_level = 3))
  }
})

test_that("is_fully_autonomous works correctly", {
  box::use(artma / libs / autonomy[
    is_fully_autonomous,
    set_autonomy_level
  ])

  withr::local_options(list("artma.autonomy.level" = NULL))

  # No level set
  expect_false(is_fully_autonomous())

  # Level 4
  set_autonomy_level(4)
  expect_false(is_fully_autonomous())

  # Level 5
  set_autonomy_level(5)
  expect_true(is_fully_autonomous())

  # Level 1
  set_autonomy_level(1)
  expect_false(is_fully_autonomous())
})

test_that("get_default_autonomy_level returns correct defaults", {
  box::use(artma / libs / autonomy[get_default_autonomy_level])

  # Default should always be 4
  default_level <- get_default_autonomy_level()
  expect_equal(default_level, 4L)
})

test_that("autonomy level persists in options namespace", {
  box::use(artma / libs / autonomy[
    set_autonomy_level,
    get_autonomy_level
  ])

  withr::local_options(list("artma.autonomy.level" = NULL))

  # Set level
  set_autonomy_level(3)

  # Get from options namespace directly
  opt_value <- getOption("artma.autonomy.level")
  expect_equal(opt_value, 3L)

  # Get via function
  expect_equal(get_autonomy_level(), 3L)
})

test_that("exported autonomy functions work", {
  box::use(artma[
    autonomy.get,
    autonomy.set,
    autonomy.is_set,
    autonomy.describe,
    autonomy.levels,
    autonomy.is_full
  ])

  withr::local_options(list("artma.autonomy.level" = NULL))

  # Test exported functions
  expect_null(autonomy.get())
  expect_false(autonomy.is_set())

  autonomy.set(4)
  expect_equal(autonomy.get(), 4L)
  expect_true(autonomy.is_set())
  expect_false(autonomy.is_full())

  desc <- autonomy.describe()
  expect_type(desc, "character")
  expect_true(grepl("Level 4", desc))

  levels <- autonomy.levels()
  expect_type(levels, "list")
  expect_length(levels, 5)

  autonomy.set(5)
  expect_true(autonomy.is_full())
})

test_that("autonomy level is loaded from options file", {
  box::use(
    artma / libs / autonomy[get_autonomy_level],
    artma / paths[PATHS]
  )

  tmp_dir <- withr::local_tempdir()
  withr::local_options(list("artma.autonomy.level" = NULL))

  # Create options file manually with autonomy level 3
  options_content <- list(
    general = list(
      artma_version = as.character(utils::packageVersion("artma"))
    ),
    autonomy = list(
      level = 3L
    ),
    data = list(
      source_path = tempfile(fileext = ".csv"),
      colnames = list(
        study = "study",
        effect = "effect",
        se = "se",
        n_obs = "n_obs"
      )
    ),
    verbose = 3L
  )

  options_path <- file.path(tmp_dir, "test_autonomy.yaml")
  yaml::write_yaml(options_content, options_path)

  # Load options using internal function
  box::use(artma / options / files[read_options_file])
  box::use(artma / options / template[flatten_user_options, collect_leaf_paths])
  box::use(artma / const[CONST])

  nested_options <- read_options_file(options_path)
  leaf_set <- collect_leaf_paths(PATHS$FILE_OPTIONS_TEMPLATE)
  prefixed_options <- flatten_user_options(
    user_options = nested_options,
    leaf_set = leaf_set,
    parent = CONST$PACKAGE_NAME
  )

  # Set options
  options(prefixed_options)

  # Set autonomy level
  autonomy_key <- paste0(CONST$PACKAGE_NAME, ".autonomy.level")
  if (autonomy_key %in% names(prefixed_options)) {
    box::use(artma / libs / autonomy[set_autonomy_level])
    level_val <- as.integer(prefixed_options[[autonomy_key]])
    if (!is.na(level_val) && level_val >= 1 && level_val <= 5) {
      set_autonomy_level(level_val)
    }
  }

  # Autonomy level should be set to 3
  level <- get_autonomy_level()
  expect_equal(level, 3L)
})

test_that("autonomy level validation in template works", {
  # This tests that the template correctly validates autonomy levels
  # through the normal options validation system

  box::use(
    artma / options / template[parse_options_from_template],
    artma / paths[PATHS]
  )

  template_path <- PATHS$FILE_OPTIONS_TEMPLATE

  # Valid autonomy level
  valid_input <- list(
    "autonomy.level" = 4L,
    "data.source_path" = tempfile(fileext = ".csv"),
    "data.colnames.study" = "study",
    "data.colnames.effect" = "effect",
    "data.colnames.se" = "se",
    "data.colnames.n_obs" = "n_obs"
  )

  result <- parse_options_from_template(
    path = template_path,
    user_input = valid_input,
    interactive = FALSE,
    add_prefix = FALSE
  )

  expect_equal(result[["autonomy.level"]], 4L)

  # Invalid autonomy level (should be coerced)
  invalid_input <- valid_input
  invalid_input[["autonomy.level"]] <- "4"  # String instead of integer

  result <- parse_options_from_template(
    path = template_path,
    user_input = invalid_input,
    interactive = FALSE,
    add_prefix = FALSE
  )

  # Should be coerced to integer
  expect_type(result[["autonomy.level"]], "integer")
  expect_equal(result[["autonomy.level"]], 4L)
})
