box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_null,
    expect_true,
    test_that
  ]
)

# Shared option set for reconcile tests: no file persistence, minimal verbosity
base_opts <- function(extra = list()) {
  utils::modifyList(
    list(
      "artma.data.colnames.obs_id" = NA_character_,
      "artma.data.colnames.effect" = "effect_size",
      "artma.data.colnames.n_obs" = NA_character_,
      "artma.data.colnames.precision" = NA_character_,
      "artma.data.colnames.reg_dof" = NA_character_,
      "artma.data.colnames.se" = "se_col",
      "artma.data.colnames.study_id" = "study",
      "artma.data.colnames.study_size" = NA_character_,
      "artma.data.colnames.t_stat" = NA_character_,
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study"),
      "artma.data.config" = list(),
      "artma.temp.file_name" = NULL,
      "artma.temp.dir_name" = NULL,
      "artma.verbose" = 1L
    ),
    extra
  )
}

# -- Group 1: Baseline / strict mode -------------------------------------------

# T1: columns match exactly -> silent NULL
test_that("reconcile_schema auto: returns NULL when columns match stored map", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    base_opts(),
    expect_null(reconcile_schema(df, mode = "auto"))
  )
})

# T2: strict + missing required column -> structured error
test_that("reconcile_schema strict: structured error when required column is missing", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "es" present instead of "effect_size"
  df <- data.frame(es = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    base_opts(),
    expect_error(
      reconcile_schema(df, mode = "strict"),
      regexp = "effect_size",
      class = "rlang_error"
    )
  )
})

# T3: strict + missing moderator column -> structured error
test_that("reconcile_schema strict: structured error when moderator column is missing", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A") # no method_iv

  withr::with_options(
    base_opts(list("artma.data.config" = list(method_iv = list(bma = TRUE)))),
    expect_error(
      reconcile_schema(df, mode = "strict"),
      regexp = "method_iv",
      class = "rlang_error"
    )
  )
})

# T4: strict + added column -> structured error
test_that("reconcile_schema strict: structured error when new column exists in data", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A", region = "EU")

  withr::with_options(
    base_opts(),
    expect_error(
      reconcile_schema(df, mode = "strict"),
      regexp = "region",
      class = "rlang_error"
    )
  )
})

# -- Group 2: First-time user gap (Bug 1) --------------------------------------

# T5: empty colnames map -> should return NULL, not trigger false drift
test_that("reconcile_schema auto: skips without error when no colnames are configured yet", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    base_opts(list(
      "artma.data.colnames.effect" = NA_character_,
      "artma.data.colnames.se" = NA_character_,
      "artma.data.colnames.study_id" = NA_character_,
      "artma.data.expected_schema_columns" = NA_character_
    )),
    expect_null(reconcile_schema(df, mode = "auto"))
  )
})

test_that("reconcile_schema auto: initializes schema baseline on first run", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A", obs_n = 1:3)

  withr::with_options(
    base_opts(list("artma.data.expected_schema_columns" = NA_character_)),
    {
      expect_null(reconcile_schema(df, mode = "auto"))
      expect_equal(
        sort(getOption("artma.data.expected_schema_columns")),
        sort(make.names(colnames(df)))
      )
    }
  )
})

test_that("reconcile_schema logs only completion when schema is unchanged", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    base_opts(list("artma.verbose" = 3L)),
    {
      logs <- testthat::capture_messages(reconcile_schema(df, mode = "auto"))
      expect_true(any(grepl("Schema reconciliation complete\\.", logs)))
      expect_false(any(grepl("artma detected dataset changes", logs)))
    }
  )
})

# -- Group 3: Auto mode, required column renames ------------------------------

# T6: high-confidence rename (>=0.75) -> accepted, in-memory colnames updated
test_that("reconcile_schema auto: accepts high-confidence rename and updates colnames in memory", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "effect_sise" vs "effect_size": 1-char typo, score ~ 0.91
  df <- data.frame(effect_sise = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    base_opts(),
    {
      reconcile_schema(df, mode = "auto")
      expect_equal(getOption("artma.data.colnames.effect"), "effect_sise")
    }
  )
})

# T7: low-confidence rename (<0.75) -> abort with hint to use "ask"
test_that("reconcile_schema auto: aborts with hint when rename confidence is too low", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "es" vs "effect_size": score ~ 0.27
  df <- data.frame(es = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    base_opts(),
    expect_error(
      reconcile_schema(df, mode = "auto"),
      regexp = "ask",
      class = "rlang_error"
    )
  )
})

# -- Group 4: Auto mode, moderator columns (Bug 2) ----------------------------

# T8: missing moderator with no close match -> dropped from in-memory config
test_that("reconcile_schema auto: drops unmatched moderator from in-memory config", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # df has no method_iv column at all
  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    base_opts(list("artma.data.config" = list(method_iv = list(bma = TRUE)))),
    {
      reconcile_schema(df, mode = "auto")
      expect_null(getOption("artma.data.config")[["method_iv"]])
    }
  )
})

# T9: missing moderator with high-confidence match -> remapped in in-memory config
test_that("reconcile_schema auto: remaps high-confidence moderator rename in in-memory config", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "method_ib" vs "method_iv": 1-char diff (b<->v), score ~ 0.89
  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A", method_ib = c(1, 0, 1))

  withr::with_options(
    base_opts(list("artma.data.config" = list(method_iv = list(bma = TRUE)))),
    {
      reconcile_schema(df, mode = "auto")
      expect_null(getOption("artma.data.config")[["method_iv"]])
      expect_true(!is.null(getOption("artma.data.config")[["method_ib"]]))
    }
  )
})

# -- Group 5: Auto mode, added columns ----------------------------------------

# T10: added column is informational only, analysis is not blocked
test_that("reconcile_schema auto: added column is informational only, returns NULL", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A", region = "EU")

  withr::with_options(
    base_opts(),
    expect_null(reconcile_schema(df, mode = "auto"))
  )
})

# -- Group 6: Multiple simultaneous changes ------------------------------------

# T11: required rename + moderator drop + added column -> all handled atomically
test_that("reconcile_schema auto: handles simultaneous rename, moderator drop, and added column", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # effect_size -> effect_sise (high confidence), method_iv gone, region is new
  df <- data.frame(effect_sise = 1:3, se_col = 0.1, study = "A", region = "EU")

  withr::with_options(
    base_opts(list("artma.data.config" = list(method_iv = list(bma = TRUE)))),
    {
      reconcile_schema(df, mode = "auto")
      expect_equal(getOption("artma.data.colnames.effect"), "effect_sise")
      expect_null(getOption("artma.data.config")[["method_iv"]])
    }
  )
})

# -- Group 7: Full pipeline integration ----------------------------------------

# T12: reconcile + standardize produces df with correct standard column names
test_that("reconcile then standardize: produces df with standard column names", {
  box::use(
    artma / data / schema_reconcile[reconcile_schema],
    artma / data / utils[standardize_column_names]
  )

  # df has typo column name ("effect_sise" instead of "effect_size")
  # All four required columns must be present (study_id, effect, se, n_obs)
  df_raw <- data.frame(
    effect_sise = c(0.1, 0.2, 0.3),
    se_col      = c(0.01, 0.02, 0.03),
    study       = c("A", "B", "C"),
    n_obs       = c(100L, 200L, 300L)
  )

  # Full colnames map covering all required columns
  full_opts <- list(
    "artma.data.colnames.effect" = "effect_size",
    "artma.data.colnames.se" = "se_col",
    "artma.data.colnames.study_id" = "study",
    "artma.data.colnames.n_obs" = "n_obs",
    "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs"),
    "artma.data.config" = list(),
    "artma.temp.file_name" = NULL,
    "artma.temp.dir_name" = NULL,
    "artma.verbose" = 1L
  )

  withr::with_options(
    full_opts,
    {
      # Step 1: reconcile detects the drift and updates in-memory colnames map
      reconcile_schema(df_raw, mode = "auto")

      # Step 2: standardize uses the now-updated map to rename the column
      df_std <- standardize_column_names(df_raw, auto_detect = FALSE)

      # Column "effect_sise" should be renamed to the standard name "effect"
      expect_true("effect" %in% colnames(df_std))
      expect_false("effect_sise" %in% colnames(df_std))
    }
  )
})
