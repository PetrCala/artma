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

# Shared option set for reconcile tests: no file persistence, minimal verbosity.
# The unified per-column store holds both the name mapping (role records with
# source_name) and the moderator config.
base_store <- function(extra = list()) {
  utils::modifyList(
    list(
      effect = list(source_name = "effect_size"),
      se = list(source_name = "se_col"),
      study_id = list(source_name = "study")
    ),
    extra
  )
}

base_opts <- function(extra = list()) {
  utils::modifyList(
    list(
      "artma.data.columns" = base_store(),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs"),
      "artma.temp.file_name" = NULL,
      "artma.temp.dir_name" = NULL,
      "artma.verbose" = 1L
    ),
    extra
  )
}

base_df <- function(...) {
  data.frame(effect_size = 1:3, se_col = 0.1, study = "A", n_obs = 10L, ...)
}

# -- Group 1: Baseline / strict mode -------------------------------------------

# T1: columns match exactly -> silent NULL
test_that("reconcile_schema auto: returns NULL when columns match stored map", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_opts(),
    expect_null(reconcile_schema(base_df(), mode = "auto"))
  )
})

# T2: strict + missing required column -> structured error
test_that("reconcile_schema strict: structured error when required column is missing", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "es" present instead of "effect_size"
  df <- data.frame(es = 1:3, se_col = 0.1, study = "A", n_obs = 10L)

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

  withr::with_options(
    base_opts(list(
      "artma.data.columns" = base_store(list(method_iv = list(bma = TRUE)))
    )),
    expect_error(
      reconcile_schema(base_df(), mode = "strict"), # no method_iv column
      regexp = "method_iv",
      class = "rlang_error"
    )
  )
})

# T4: strict + added column -> structured error
test_that("reconcile_schema strict: structured error when new column exists in data", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_opts(),
    expect_error(
      reconcile_schema(base_df(region = "EU"), mode = "strict"),
      regexp = "region",
      class = "rlang_error"
    )
  )
})

# -- Group 2: First-time user gap ----------------------------------------------

# T5: empty store and no baseline -> should return NULL, not trigger false drift
test_that("reconcile_schema auto: skips without error when no columns are configured yet", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_opts(list(
      "artma.data.columns" = list(),
      "artma.data.expected_schema_columns" = NA_character_
    )),
    expect_null(reconcile_schema(base_df(), mode = "auto"))
  )
})

test_that("reconcile_schema auto: initializes schema baseline on first run", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- base_df(obs_n = 1:3)

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

  withr::with_options(
    base_opts(list("artma.verbose" = 3L)),
    {
      logs <- testthat::capture_messages(reconcile_schema(base_df(), mode = "auto"))
      expect_true(any(grepl("Schema reconciliation complete\\.", logs)))
      expect_false(any(grepl("artma detected dataset changes", logs)))
    }
  )
})

# -- Group 3: Auto mode, required column renames ------------------------------

# T6: high-confidence rename -> accepted, the single record is updated in memory
test_that("reconcile_schema auto: accepts high-confidence rename and updates the column record", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "effect_sise" vs "effect_size": 1-char typo, string score ~ 0.91
  df <- data.frame(effect_sise = 1:3, se_col = 0.1, study = "A", n_obs = 10L)

  withr::with_options(
    base_opts(),
    {
      reconcile_schema(df, mode = "auto")
      store <- getOption("artma.data.columns")
      expect_equal(store$effect$source_name, "effect_sise")
    }
  )
})

# T6b: the pattern engine resolves renames that pure string similarity cannot
test_that("reconcile_schema auto: pattern engine maps a recognizable column name", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "beta" is dissimilar to "effect_size" as a string, but the recognition
  # patterns identify it as an effect column, so auto mode can resolve it.
  df <- data.frame(beta = c(0.5, 0.7, 0.9), se_col = 0.1, study = "A", n_obs = 10L)

  withr::with_options(
    base_opts(),
    {
      reconcile_schema(df, mode = "auto")
      store <- getOption("artma.data.columns")
      expect_equal(store$effect$source_name, "beta")
    }
  )
})

# T7: unresolvable rename -> abort with hint to use "ask"
test_that("reconcile_schema auto: aborts with hint when rename confidence is too low", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "xyz_qq" matches neither the stored name nor any effect pattern
  df <- data.frame(xyz_qq = 1:3, se_col = 0.1, study = "A", n_obs = 10L)

  withr::with_options(
    base_opts(),
    expect_error(
      reconcile_schema(df, mode = "auto"),
      regexp = "ask",
      class = "rlang_error"
    )
  )
})

# -- Group 4: Auto mode, moderator columns ------------------------------------

# T8: missing moderator with no close match -> dropped from the store
test_that("reconcile_schema auto: drops unmatched moderator from the column store", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_opts(list(
      "artma.data.columns" = base_store(list(method_iv = list(bma = TRUE)))
    )),
    {
      reconcile_schema(base_df(), mode = "auto") # df has no method_iv at all
      expect_null(getOption("artma.data.columns")[["method_iv"]])
    }
  )
})

# T9: missing moderator with high-confidence match -> record moved to new key
test_that("reconcile_schema auto: remaps high-confidence moderator rename in the store", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "method_ib" vs "method_iv": 1-char diff (b<->v), score ~ 0.89
  df <- base_df(method_ib = c(1, 0, 1))

  withr::with_options(
    base_opts(list(
      "artma.data.columns" = base_store(list(method_iv = list(bma = TRUE)))
    )),
    {
      reconcile_schema(df, mode = "auto")
      store <- getOption("artma.data.columns")
      expect_null(store[["method_iv"]])
      expect_true(!is.null(store[["method_ib"]]))
      expect_true(store[["method_ib"]]$bma)
    }
  )
})

# -- Group 5: Auto mode, added columns ----------------------------------------

# T10: added column is informational only, analysis is not blocked
test_that("reconcile_schema auto: added column is informational only, returns NULL", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_opts(),
    expect_null(reconcile_schema(base_df(region = "EU"), mode = "auto"))
  )
})

# -- Group 6: Multiple simultaneous changes ------------------------------------

# T11: required rename + moderator drop + added column -> all handled atomically
test_that("reconcile_schema auto: handles simultaneous rename, moderator drop, and added column", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # effect_size -> effect_sise (high confidence), method_iv gone, region is new
  df <- data.frame(
    effect_sise = 1:3, se_col = 0.1, study = "A", n_obs = 10L, region = "EU"
  )

  withr::with_options(
    base_opts(list(
      "artma.data.columns" = base_store(list(method_iv = list(bma = TRUE)))
    )),
    {
      reconcile_schema(df, mode = "auto")
      store <- getOption("artma.data.columns")
      expect_equal(store$effect$source_name, "effect_sise")
      expect_null(store[["method_iv"]])
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
  df_raw <- data.frame(
    effect_sise = c(0.1, 0.2, 0.3),
    se_col      = c(0.01, 0.02, 0.03),
    study       = c("A", "B", "C"),
    n_obs       = c(100L, 200L, 300L)
  )

  withr::with_options(
    base_opts(list(
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs")
    )),
    {
      # Step 1: reconcile detects the drift and updates the column record
      reconcile_schema(df_raw, mode = "auto")

      # Step 2: standardize uses the now-updated record to rename the column
      df_std <- standardize_column_names(df_raw)

      # Column "effect_sise" should be renamed to the standard name "effect"
      expect_true("effect" %in% colnames(df_std))
      expect_false("effect_sise" %in% colnames(df_std))
    }
  )
})
