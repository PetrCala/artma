box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_message,
    expect_no_error,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(
  artma / data / interactive_mapping[
    confirm_column_mapping,
    confirm_provisional_mappings,
    format_mapping_display,
    format_provisional_evidence,
    interactive_column_mapping
  ],
  artma / data_config / column_mapping[save_column_mapping_to_options],
  artma / data / column_recognition[get_required_column_names]
)


# Note: the prompting paths of interactive_column_mapping and
# column_mapping_workflow require user interaction via climenu, so they are
# tested in E2E tests instead. The non-interactive branches and helper
# functions are tested here.


test_that("interactive_column_mapping never guesses a missing required column non-interactively", {
  df <- data.frame(
    effect = c(0.5, 0.7),
    se = c(0.1, 0.2),
    study_id = c("A", "B")
  )

  # n_obs is required but absent from the data, so it cannot be auto-detected.
  auto_mapping <- list(
    effect = "effect",
    se = "se",
    study_id = "study_id"
  )

  withr::local_options(list("artma.verbose" = 2))

  # The warning must name the column that stays unmapped.
  expect_message(
    result <- interactive_column_mapping(
      df = df,
      auto_mapping = auto_mapping,
      required_only = TRUE,
      show_detected_first = TRUE,
      is_interactive = FALSE
    ),
    "n_obs"
  )

  # The missing required column stays unmapped instead of falling back to the
  # first menu entry, which used to silently map n_obs to effect.
  expect_equal(result, auto_mapping)
  expect_false("n_obs" %in% names(result))
})


test_that("confirm_column_mapping returns mapping unchanged", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect",
    se = "se",
    n_obs = "n_obs"
  )

  required_cols <- c("study_id", "effect", "se", "n_obs")

  withr::local_options(list("artma.verbose" = 1))
  result <- confirm_column_mapping(mapping, required_cols)

  expect_equal(result, mapping)
})


test_that("confirm_column_mapping handles empty mapping", {
  mapping <- list()
  required_cols <- c("study_id", "effect", "se", "n_obs")

  withr::local_options(list("artma.verbose" = 1))
  result <- confirm_column_mapping(mapping, required_cols)

  expect_equal(result, mapping)
})


test_that("confirm_column_mapping handles partial mapping", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect"
  )

  required_cols <- c("study_id", "effect", "se", "n_obs")

  withr::local_options(list("artma.verbose" = 1))
  result <- confirm_column_mapping(mapping, required_cols)

  expect_equal(result, mapping)
})


test_that("save_column_mapping_to_options sets session options correctly", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect_size",
    se = "std_error"
  )

  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.columns" = list()
  ))

  # Save without options file (just to session)
  save_column_mapping_to_options(mapping, options_file_name = NULL)

  # Check that role records were written to the unified store
  store <- getOption("artma.data.columns")
  expect_equal(store$study_id$source_name, "study_name")
  expect_equal(store$effect$source_name, "effect_size")
  expect_equal(store$se$source_name, "std_error")
})


test_that("save_column_mapping_to_options handles empty mapping", {
  mapping <- list()

  withr::local_options(list("artma.verbose" = 1))

  expect_no_error(
    save_column_mapping_to_options(mapping, options_file_name = NULL)
  )
})


test_that("save_column_mapping_to_options handles multiple columns", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect",
    se = "se",
    n_obs = "n_obs",
    t_stat = "t_statistic",
    obs_id = "sid"
  )

  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.columns" = list()
  ))

  save_column_mapping_to_options(mapping, options_file_name = NULL)

  # Genuine renames are stored; identity mappings (effect, se, n_obs) are not,
  # since a column already carrying the standard name needs no record.
  store <- getOption("artma.data.columns")
  expect_equal(store$study_id$source_name, "study_name")
  expect_equal(store$t_stat$source_name, "t_statistic")
  expect_equal(store$obs_id$source_name, "sid")
  expect_null(store$effect)
  expect_null(store$se)
  expect_null(store$n_obs)
})


test_that("confirm_column_mapping works with verbose output", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect"
  )

  required_cols <- c("study_id", "effect", "se", "n_obs")

  withr::local_options(list("artma.verbose" = 4))

  # Should not error even with verbose output
  expect_no_error(
    confirm_column_mapping(mapping, required_cols)
  )
})


test_that("save_column_mapping_to_options works with verbose output", {
  mapping <- list(
    study_id = "study_name"
  )

  withr::local_options(list(
    "artma.verbose" = 4,
    "artma.data.columns" = list()
  ))

  expect_no_error(
    save_column_mapping_to_options(mapping, options_file_name = NULL)
  )
})


test_that("save_column_mapping_to_options preserves existing analysis fields", {
  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.columns" = list(
      effect = list(bma = TRUE)
    )
  ))

  save_column_mapping_to_options(
    list(effect = "effect_size"),
    options_file_name = NULL
  )

  store <- getOption("artma.data.columns")
  expect_equal(store$effect$source_name, "effect_size")
  expect_true(store$effect$bma)
})


test_that("format_mapping_display correctly separates required and optional", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect_size",
    se = "se",
    n_obs = "n_obs",
    t_stat = "t_statistic"
  )

  required_cols <- get_required_column_names()

  result <- format_mapping_display(mapping, required_cols)

  expect_true(is.list(result))
  expect_true("required" %in% names(result))
  expect_true("optional" %in% names(result))

  # All required columns should be in required list
  expect_true(all(required_cols %in% names(result$required)))

  # Optional columns should be in optional list
  expect_true("t_stat" %in% names(result$optional))
  expect_false("t_stat" %in% names(result$required))
})


test_that("format_mapping_display handles empty optional columns", {
  mapping <- list(
    study_id = "study_name",
    effect = "effect_size",
    se = "se",
    n_obs = "n_obs"
  )

  required_cols <- get_required_column_names()

  result <- format_mapping_display(mapping, required_cols)

  expect_equal(length(result$optional), 0)
  expect_equal(length(result$required), 4)
})


test_that("read_stored_columns seeds from the options file, not the session", {
  box::use(
    artma / data_config / column_mapping[read_stored_columns],
    artma / options / files[write_options_file]
  )

  tmp_dir <- withr::local_tempdir()
  file_name <- "seed-test.yaml"

  write_options_file(
    file.path(tmp_dir, file_name),
    list(data = list(columns = list(gdp_growth = list(bma = TRUE))))
  )

  # The session store is empty, as it is during options file creation.
  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.columns" = list()
  ))

  store <- read_stored_columns(file_name, options_dir = tmp_dir)

  expect_true(store$gdp_growth$bma)
})


test_that("save_column_mapping_to_options keeps records already in the file", {
  box::use(
    artma / data_config / column_mapping[save_column_mapping_to_options],
    artma / options / files[options_file_path, read_options_file, write_options_file]
  )

  tmp_dir <- withr::local_tempdir()
  file_name <- "keep-records.yaml"
  path <- options_file_path(tmp_dir, file_name)

  # Build a file valid against the real template, then seed it with records
  # a user would have configured before the mapping flow runs.
  artma::options_create(
    options_file_name = file_name,
    options_dir = tmp_dir,
    user_input = list("data.source_path" = "some-data.csv"),
    should_validate = FALSE,
    should_overwrite = TRUE
  )

  seeded <- read_options_file(path)
  seeded$data$columns <- list(
    gdp_growth = list(bma = TRUE),
    effect = list(bma = FALSE)
  )
  write_options_file(path, seeded)

  # Empty session store, as during options file creation.
  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.columns" = list()
  ))

  save_column_mapping_to_options(
    list(effect = "effect_size"),
    options_file_name = file_name,
    options_dir = tmp_dir
  )

  written <- read_options_file(path)$data$columns

  # The moderator record survives the write
  expect_true(written$gdp_growth$bma)

  # The mapped role record gains source_name without losing its other fields
  expect_equal(written$effect$source_name, "effect_size")
  expect_false(written$effect$bma)
})


test_that("read_stored_columns falls back to the session store when absent", {
  box::use(artma / data_config / column_mapping[read_stored_columns])

  withr::local_options(list(
    "artma.verbose" = 1,
    "artma.data.columns" = list(effect = list(source_name = "b"))
  ))

  store <- read_stored_columns("no-such-file-here.yaml")

  expect_equal(store$effect$source_name, "b")
})


# --- Confirming sub-threshold candidates ------------------------------------
# `select_fn` and `is_interactive` make the confirm path drivable without a
# terminal: the menu is the only part that needs one.

#' One provisional candidate of the family the benchmark keeps missing: a
#' column whose values and pair consistency say "effect" while its name says
#' nothing at all.
nameless_effect_candidate <- function(column = "eis", kind = "unmapped", alternatives = character(0)) {
  list(
    kind = kind,
    role = "effect",
    column = column,
    score = 0.65,
    evidence = 0.95,
    name_score = 0,
    pair_consistency = 0.98,
    pair_with = "se",
    runner_up = "idstudy",
    margin = 0.3,
    alternatives = alternatives,
    summary = list(
      column = column,
      coverage = 1,
      n_distinct = 140L,
      non_integer_share = 1,
      has_both_signs = TRUE,
      median_abs = 0.3
    )
  )
}

pick_first <- function(choices, prompt) choices[1]
pick_last <- function(choices, prompt) choices[length(choices)]


test_that("confirming a provisional candidate maps the role", {
  withr::local_options(list("artma.verbose" = 1))
  mapping <- list(se = "se", study_id = "study", n_obs = "nobs")

  result <- confirm_provisional_mappings(
    mapping = mapping,
    provisional = list(effect = nameless_effect_candidate()),
    select_fn = pick_first,
    is_interactive = TRUE
  )

  expect_equal(result$effect, "eis")
})


test_that("declining a provisional candidate leaves the role unmapped", {
  withr::local_options(list("artma.verbose" = 1))
  mapping <- list(se = "se")

  result <- confirm_provisional_mappings(
    mapping = mapping,
    provisional = list(effect = nameless_effect_candidate()),
    select_fn = pick_last,
    is_interactive = TRUE
  )

  expect_equal(result, mapping)
  expect_false("effect" %in% names(result))

  # Cancelling the menu is a decline, not an error.
  cancelled <- confirm_provisional_mappings(
    mapping = mapping,
    provisional = list(effect = nameless_effect_candidate()),
    select_fn = function(choices, prompt) NULL,
    is_interactive = TRUE
  )
  expect_equal(cancelled, mapping)
})


test_that("a non-interactive session is never asked", {
  withr::local_options(list("artma.verbose" = 1))
  mapping <- list(se = "se")

  result <- confirm_provisional_mappings(
    mapping = mapping,
    provisional = list(effect = nameless_effect_candidate()),
    select_fn = function(choices, prompt) stop("must not prompt"),
    is_interactive = FALSE
  )

  expect_equal(result, mapping)
})


test_that("a near-tie asks which twin holds the role", {
  withr::local_options(list("artma.verbose" = 1))
  mapping <- list(effect = "effect", se = "se")
  tie <- nameless_effect_candidate(column = "effect", kind = "tie", alternatives = "effect_M")

  swapped <- confirm_provisional_mappings(
    mapping = mapping,
    provisional = list(effect = tie),
    select_fn = function(choices, prompt) choices[2],
    is_interactive = TRUE
  )
  expect_equal(swapped$effect, "effect_M")

  kept <- confirm_provisional_mappings(
    mapping = mapping,
    provisional = list(effect = tie),
    select_fn = pick_first,
    is_interactive = TRUE
  )
  expect_equal(kept$effect, "effect")

  # Ties are the non-critical half of this: they are skipped where the autonomy
  # level does not ask about such choices.
  ignored <- confirm_provisional_mappings(
    mapping = mapping,
    provisional = list(effect = tie),
    allow_ties = FALSE,
    select_fn = function(choices, prompt) stop("must not prompt"),
    is_interactive = TRUE
  )
  expect_equal(ignored, mapping)
})


test_that("candidates too close to call are offered as one pick-one question", {
  withr::local_options(list("artma.verbose" = 1))
  mapping <- list(se = "se")
  candidate <- nameless_effect_candidate(alternatives = c("eis_w", "realrate"))
  candidate$alternative_summaries <- list(
    list(column = "eis_w", evidence = 0.95, pair_consistency = 0.9),
    list(column = "realrate", evidence = 0.9, pair_consistency = 0.75)
  )

  seen <- NULL
  result <- confirm_provisional_mappings(
    mapping = mapping,
    provisional = list(effect = candidate),
    select_fn = function(choices, prompt) {
      seen <<- choices
      choices[2]
    },
    is_interactive = TRUE
  )

  expect_equal(seen[seq_len(3)], c("eis", "eis_w", "realrate"))
  expect_true(grepl("None of these", seen[length(seen)], fixed = TRUE))
  expect_equal(result$effect, "eis_w")

  # Picking the decline entry still leaves the role unmapped.
  declined <- confirm_provisional_mappings(
    mapping = mapping,
    provisional = list(effect = candidate),
    select_fn = pick_last,
    is_interactive = TRUE
  )
  expect_false("effect" %in% names(declined))
})


test_that("a column another role already holds is not offered", {
  withr::local_options(list("artma.verbose" = 1))
  mapping <- list(se = "eis", study_id = "study")

  result <- confirm_provisional_mappings(
    mapping = mapping,
    provisional = list(effect = nameless_effect_candidate()),
    select_fn = function(choices, prompt) stop("must not prompt"),
    is_interactive = TRUE
  )

  expect_equal(result, mapping)
})


test_that("interactive_column_mapping confirms provisional candidates before prompting", {
  withr::local_options(list("artma.verbose" = 1))
  df <- data.frame(
    study = rep(c("A", "B"), each = 3),
    eis = c(0.1, -0.2, 0.3, 0.15, -0.25, 0.35),
    se = c(0.05, 0.08, 0.06, 0.04, 0.09, 0.07),
    nobs = rep(c(120, 340), each = 3)
  )
  auto_mapping <- list(se = "se", study_id = "study", n_obs = "nobs")
  attr(auto_mapping, "provisional") <- list(effect = nameless_effect_candidate())

  result <- interactive_column_mapping(
    df = df,
    auto_mapping = auto_mapping,
    required_only = TRUE,
    show_detected_first = FALSE,
    is_interactive = TRUE,
    select_fn = pick_first
  )

  # The confirmed column fills the missing required role, so the column menus
  # below it never run.
  expect_equal(result$effect, "eis")
})


test_that("interactive_column_mapping leaves the mapping alone non-interactively", {
  withr::local_options(list("artma.verbose" = 1))
  df <- data.frame(
    study = rep(c("A", "B"), each = 3),
    eis = c(0.1, -0.2, 0.3, 0.15, -0.25, 0.35),
    se = c(0.05, 0.08, 0.06, 0.04, 0.09, 0.07),
    nobs = rep(c(120, 340), each = 3)
  )
  auto_mapping <- list(se = "se", study_id = "study", n_obs = "nobs")
  attr(auto_mapping, "provisional") <- list(effect = nameless_effect_candidate())

  result <- interactive_column_mapping(
    df = df,
    auto_mapping = auto_mapping,
    required_only = TRUE,
    show_detected_first = FALSE,
    is_interactive = FALSE,
    select_fn = function(choices, prompt) stop("must not prompt")
  )

  expect_false("effect" %in% names(result))
})


test_that("format_provisional_evidence explains why the candidate is plausible", {
  lines <- format_provisional_evidence(nameless_effect_candidate())

  expect_true(any(grepl("effect values", lines, fixed = TRUE)))
  expect_true(any(grepl("rows populated", lines, fixed = TRUE)))
  expect_true(any(grepl("consistent with the mapped column se", lines, fixed = TRUE)))
  expect_true(any(grepl("name carries no signal", lines, fixed = TRUE)))
  expect_true(any(grepl("idstudy", lines, fixed = TRUE)))
})
