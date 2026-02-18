test_that <- getFromNamespace("test_that", "testthat")
expect_null <- getFromNamespace("expect_null", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_error <- getFromNamespace("expect_error", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_false <- getFromNamespace("expect_false", "testthat")

# Shared option set for reconcile tests <U+2014> no file persistence, minimal verbosity
base_opts <- function(extra = list()) {
  utils::modifyList(
    list(
      "artma.data.colnames.effect"   = "effect_size",
      "artma.data.colnames.se"       = "se_col",
      "artma.data.colnames.study_id" = "study",
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study"),
      "artma.data.config"            = list(),
      "artma.temp.file_name"         = NULL,
      "artma.temp.dir_name"          = NULL,
      "artma.verbose"                = 1L
    ),
    extra
  )
}

# <U+2500><U+2500> Group 1: Baseline / strict mode <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

# T1 <U+2014> columns match exactly <U+2192> silent NULL
test_that("reconcile_schema auto: returns NULL when columns match stored map", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    base_opts(),
    expect_null(reconcile_schema(df, mode = "auto"))
  )
})

# T2 <U+2014> strict + missing required column <U+2192> structured error
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

# T3 <U+2014> strict + missing moderator column <U+2192> structured error
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

# T4 <U+2014> strict + added column <U+2192> structured error
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

# <U+2500><U+2500> Group 2: First-time user gap (Bug 1) <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

# T5 <U+2014> empty colnames map <U+2192> should return NULL, not trigger false drift
test_that("reconcile_schema auto: skips without error when no colnames are configured yet", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    list(
      "artma.data.colnames.effect"   = NA,
      "artma.data.colnames.se"       = NA,
      "artma.data.colnames.study_id" = NA,
      "artma.data.expected_schema_columns" = NA_character_,
      "artma.data.config"            = list(),
      "artma.temp.file_name"         = NULL,
      "artma.temp.dir_name"          = NULL,
      "artma.verbose"                = 1L
    ),
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

# <U+2500><U+2500> Group 3: Auto mode <U+2014> required column renames <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

# T6 <U+2014> high-confidence rename (<U+2265>0.75) <U+2192> accepted, in-memory colnames updated
test_that("reconcile_schema auto: accepts high-confidence rename and updates colnames in memory", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "effect_sise" vs "effect_size": 1-char typo, score <U+2248> 0.91
  df <- data.frame(effect_sise = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    base_opts(),
    {
      reconcile_schema(df, mode = "auto")
      expect_equal(getOption("artma.data.colnames.effect"), "effect_sise")
    }
  )
})

# T7 <U+2014> low-confidence rename (<0.75) <U+2192> abort with hint to use "ask"
test_that("reconcile_schema auto: aborts with hint when rename confidence is too low", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "es" vs "effect_size": score <U+2248> 0.27
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

# <U+2500><U+2500> Group 4: Auto mode <U+2014> moderator columns (Bug 2) <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

# T8 <U+2014> missing moderator with no close match <U+2192> dropped from in-memory config
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

# T9 <U+2014> missing moderator with high-confidence match <U+2192> remapped in in-memory config
test_that("reconcile_schema auto: remaps high-confidence moderator rename in in-memory config", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "method_ib" vs "method_iv": 1-char diff (b<U+2194>v), score <U+2248> 0.89
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

# <U+2500><U+2500> Group 5: Auto mode <U+2014> added columns <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

# T10 <U+2014> added column is informational only, analysis is not blocked
test_that("reconcile_schema auto: added column is informational only, returns NULL", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A", region = "EU")

  withr::with_options(
    base_opts(),
    expect_null(reconcile_schema(df, mode = "auto"))
  )
})

# <U+2500><U+2500> Group 6: Multiple simultaneous changes <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

# T11 <U+2014> required rename + moderator drop + added column <U+2192> all handled atomically
test_that("reconcile_schema auto: handles simultaneous rename, moderator drop, and added column", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # effect_size <U+2192> effect_sise (high confidence), method_iv gone, region is new
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

# <U+2500><U+2500> Group 7: Full pipeline integration <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

# T12 <U+2014> reconcile + standardize produces df with correct standard column names
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
    "artma.data.colnames.effect"   = "effect_size",
    "artma.data.colnames.se"       = "se_col",
    "artma.data.colnames.study_id" = "study",
    "artma.data.colnames.n_obs"    = "n_obs",
    "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs"),
    "artma.data.config"            = list(),
    "artma.temp.file_name"         = NULL,
    "artma.temp.dir_name"          = NULL,
    "artma.verbose"                = 1L
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
