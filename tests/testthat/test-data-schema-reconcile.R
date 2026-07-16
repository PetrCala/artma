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

# Unified per-column store used across the reconcile tests: role records carry
# the source column mapping; moderators are keyed by their own name.
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

base_reconcile_opts <- function(extra = list()) {
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

# detect_schema_drift

test_that("detect_schema_drift reports no drift when all columns match", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  result <- detect_schema_drift(base_df(), base_store())

  expect_false(result$has_drift)
  expect_equal(length(result$missing_roles), 0L)
  expect_equal(length(result$missing_moderators), 0L)
  expect_equal(length(result$added), 0L)
})

test_that("detect_schema_drift detects a renamed required column", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # effect_size was renamed to es in the df
  raw_df <- data.frame(es = 1:3, se_col = 0.1, study = "A", n_obs = 10L)

  result <- detect_schema_drift(raw_df, base_store())

  expect_true(result$has_drift)
  expect_true("effect" %in% names(result$missing_roles))
  expect_equal(result$missing_roles[["effect"]], "effect_size")
  expect_equal(length(result$missing_moderators), 0L)
})

test_that("detect_schema_drift tracks required roles with no record as identity mappings", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # n_obs has no record in the store; when the df lacks the column under its
  # standard name, that is drift.
  raw_df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  result <- detect_schema_drift(raw_df, base_store())

  expect_true(result$has_drift)
  expect_true("n_obs" %in% names(result$missing_roles))
  expect_equal(result$missing_roles[["n_obs"]], "n_obs")
})

test_that("detect_schema_drift detects a missing moderator column", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # method_iv was removed from df, but its record is in the store
  store <- base_store(list(method_iv = list(bma = TRUE)))

  result <- detect_schema_drift(base_df(), store)

  expect_true(result$has_drift)
  expect_true("method_iv" %in% result$missing_moderators)
  expect_equal(length(result$missing_roles), 0L)
})

test_that("detect_schema_drift detects newly added columns", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # region is new in the df
  result <- detect_schema_drift(base_df(region = "EU"), base_store())

  expect_true(result$has_drift)
  expect_true("region" %in% result$added)
  expect_equal(length(result$missing_roles), 0L)
  expect_equal(length(result$missing_moderators), 0L)
})

test_that("detect_schema_drift ignores role records with an NA source_name", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # An optional role record with NA source_name should not be tracked
  store <- base_store(list(t_stat = list(source_name = NA)))

  result <- detect_schema_drift(base_df(), store)

  expect_false(result$has_drift)
})

test_that("detect_schema_drift skips computed column records", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # Computed columns are added by the pipeline, never present in the raw df
  store <- base_store(list(
    precision = list(var_name = "precision", is_computed = TRUE),
    study_label = list(var_name = "study_label", is_computed = TRUE)
  ))

  result <- detect_schema_drift(base_df(), store)

  expect_false(result$has_drift)
})

test_that("detect_schema_drift handles multiple simultaneous drifts", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  raw_df <- data.frame(es = 1:3, se_col = 0.1, study = "A", n_obs = 10L, region = "EU")
  store <- base_store(list(method_iv = list(bma = TRUE)))

  result <- detect_schema_drift(raw_df, store)

  expect_true(result$has_drift)
  expect_true("effect" %in% names(result$missing_roles))
  expect_true("method_iv" %in% result$missing_moderators)
  expect_true("region" %in% result$added)
})

# Rename scoring (shared matching engine)

test_that("score_rename_candidate finds obvious renames via string similarity", {
  box::use(artma / data / column_recognition[MATCH_THRESHOLDS, score_rename_candidate])

  score_pub <- score_rename_candidate("publication_year", "pub_year")
  score_qq <- score_rename_candidate("effect_size", "xyz_qq")

  # pub_year is a plausible rename of publication_year; xyz_qq is not
  expect_true(score_pub > score_qq)
  expect_true(score_pub >= MATCH_THRESHOLDS$rename_suggest)
  expect_true(score_qq < MATCH_THRESHOLDS$rename_suggest)
})

test_that("score_rename_candidate uses the pattern engine when the role is known", {
  box::use(artma / data / column_recognition[MATCH_THRESHOLDS, score_rename_candidate])

  # "beta" is nothing like "effect_size" as a string, but the recognition
  # patterns identify it as an effect column.
  without_role <- score_rename_candidate("effect_size", "beta")
  with_role <- score_rename_candidate("effect_size", "beta", std_name = "effect")

  expect_true(with_role > without_role)
  expect_true(with_role >= MATCH_THRESHOLDS$rename_auto)
})

# reconcile_schema (strict mode)

test_that("reconcile_schema in strict mode aborts when required column is missing", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  raw_df <- data.frame(zzz = 1:3, se_col = 0.1, study = "A", n_obs = 10L)

  withr::with_options(
    base_reconcile_opts(),
    expect_error(
      reconcile_schema(raw_df, mode = "strict"),
      class = "rlang_error"
    )
  )
})

test_that("reconcile_schema in strict mode passes when no drift exists", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  result <- withr::with_options(
    base_reconcile_opts(),
    reconcile_schema(base_df(), mode = "strict")
  )

  expect_null(result)
})

test_that("reconcile_schema in strict mode aborts on missing moderator", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_reconcile_opts(list(
      "artma.data.columns" = base_store(list(method_iv = list(bma = TRUE)))
    )),
    expect_error(
      reconcile_schema(base_df(), mode = "strict"),
      class = "rlang_error"
    )
  )
})

test_that("reconcile_schema in strict mode aborts on added column", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_reconcile_opts(),
    expect_error(
      reconcile_schema(base_df(region = "EU"), mode = "strict"),
      class = "rlang_error"
    )
  )
})
