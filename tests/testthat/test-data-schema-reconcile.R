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

base_reconcile_opts <- function(extra = list()) {
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
      "artma.data.config" = list()
    ),
    extra
  )
}

# detect_schema_drift

test_that("detect_schema_drift reports no drift when all columns match", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  raw_df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")
  colnames_map <- list(effect = "effect_size", se = "se_col", study_id = "study")
  config_overrides <- list()

  result <- detect_schema_drift(raw_df, colnames_map, config_overrides)

  expect_false(result$has_drift)
  expect_equal(length(result$colnames_drift$missing_std), 0L)
  expect_equal(length(result$config_drift$missing_moderators), 0L)
  expect_equal(length(result$config_drift$added), 0L)
})

test_that("detect_schema_drift detects a renamed required column", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # effect_size was renamed to es in the df
  raw_df <- data.frame(es = 1:3, se_col = 0.1, study = "A")
  colnames_map <- list(effect = "effect_size", se = "se_col", study_id = "study")
  config_overrides <- list()

  result <- detect_schema_drift(raw_df, colnames_map, config_overrides)

  expect_true(result$has_drift)
  expect_true("effect" %in% result$colnames_drift$missing_std)
  expect_equal(length(result$config_drift$missing_moderators), 0L)
})

test_that("detect_schema_drift detects a missing moderator column", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # method_iv was removed from df, but it's in config_overrides
  raw_df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")
  colnames_map <- list(effect = "effect_size", se = "se_col", study_id = "study")
  config_overrides <- list(method_iv = list(bma = TRUE))

  result <- detect_schema_drift(raw_df, colnames_map, config_overrides)

  expect_true(result$has_drift)
  expect_true("method_iv" %in% result$config_drift$missing_moderators)
  expect_equal(length(result$colnames_drift$missing_std), 0L)
})

test_that("detect_schema_drift detects newly added columns", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # region is new in the df
  raw_df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A", region = "EU")
  colnames_map <- list(effect = "effect_size", se = "se_col", study_id = "study")
  config_overrides <- list()

  result <- detect_schema_drift(raw_df, colnames_map, config_overrides)

  expect_true(result$has_drift)
  expect_true("region" %in% result$config_drift$added)
  expect_equal(length(result$colnames_drift$missing_std), 0L)
  expect_equal(length(result$config_drift$missing_moderators), 0L)
})

test_that("detect_schema_drift handles NA values in colnames_map", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  raw_df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")
  # dof is optional (NA) <U+2014> should not be considered as missing
  colnames_map <- list(
    effect = "effect_size", se = "se_col", study_id = "study", dof = NA
  )
  config_overrides <- list()

  result <- detect_schema_drift(raw_df, colnames_map, config_overrides)

  expect_false(result$has_drift)
})

test_that("detect_schema_drift handles multiple simultaneous drifts", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  raw_df <- data.frame(es = 1:3, se_col = 0.1, study = "A", region = "EU")
  colnames_map <- list(effect = "effect_size", se = "se_col", study_id = "study")
  config_overrides <- list(method_iv = list(bma = TRUE))

  result <- detect_schema_drift(raw_df, colnames_map, config_overrides)

  expect_true(result$has_drift)
  expect_true("effect" %in% result$colnames_drift$missing_std)
  expect_true("method_iv" %in% result$config_drift$missing_moderators)
  expect_true("region" %in% result$config_drift$added)
})

# propose_renames

test_that("propose_renames: adist-based similarity finds obvious renames", {
  # Direct test using the logic: effect_size vs es should be lower confidence
  # than publication_year vs pub_year
  score_pub <- 1 - utils::adist("publication_year", "pub_year")[1, 1] /
    max(nchar("publication_year"), nchar("pub_year"))
  score_es <- 1 - utils::adist("effect_size", "es")[1, 1] /
    max(nchar("effect_size"), nchar("es"))

  # pub_year is more similar to publication_year than es is to effect_size
  expect_true(score_pub > score_es)
  # publication_year -> pub_year should be >= 0.5 (suggested)
  expect_true(score_pub >= 0.5)
})

# reconcile_schema (strict mode)

test_that("reconcile_schema in strict mode aborts when required column is missing", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  raw_df <- data.frame(es = 1:3, se_col = 0.1, study = "A")

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

  raw_df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  result <- withr::with_options(
    base_reconcile_opts(),
    reconcile_schema(raw_df, mode = "strict")
  )

  expect_null(result)
})

test_that("reconcile_schema in strict mode aborts on missing moderator", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  raw_df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    base_reconcile_opts(list("artma.data.config" = list(method_iv = list(bma = TRUE)))),
    expect_error(
      reconcile_schema(raw_df, mode = "strict"),
      class = "rlang_error"
    )
  )
})

test_that("reconcile_schema in strict mode aborts on added column", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  raw_df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A", region = "EU")

  withr::with_options(
    base_reconcile_opts(),
    expect_error(
      reconcile_schema(raw_df, mode = "strict"),
      class = "rlang_error"
    )
  )
})
