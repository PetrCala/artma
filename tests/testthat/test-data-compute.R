box::use(
  testthat[expect_equal, expect_true, test_that]
)

box::use(
  artma / data / compute[compute_optional_columns],
  artma / data / utils[get_reserved_colnames]
)

computed_config_overrides <- list(
  obs_id = list(var_name = "obs_id", is_computed = TRUE),
  study_id = list(var_name = "study_id", is_computed = TRUE),
  study_label = list(var_name = "study_label", is_computed = TRUE),
  t_stat = list(var_name = "t_stat", is_computed = TRUE),
  study_size = list(var_name = "study_size", is_computed = TRUE),
  reg_dof = list(var_name = "reg_dof", is_computed = TRUE),
  precision = list(var_name = "precision", is_computed = TRUE)
)

# Options every compute_optional_columns test needs: mark the optional columns
# as computed, skip result persistence, and fix the precision definition.
local_compute_options <- function(.env = parent.frame()) {
  withr::local_options(list(
    "artma.data.columns" = computed_config_overrides,
    "artma.output.save_results" = FALSE,
    "artma.calc.precision_type" = "1/SE",
    "artma.verbose" = 1
  ), .local_envir = .env)
}

# A three-row frame with a repeated study id (rows 1 and 3 share a study).
sample_study_df <- function() {
  data.frame(
    study_id = c("Albeigh (2008)", "Baker (2009)", "Albeigh (2008)"),
    effect = c(0.2, 0.5, 0.1),
    se = c(0.1, 0.2, 0.1),
    n_obs = c(120, 150, 120),
    stringsAsFactors = FALSE
  )
}


test_that("compute_optional_columns preserves study labels while normalizing study_id", {
  df <- sample_study_df()
  local_compute_options()

  result <- compute_optional_columns(df)

  expect_true("study_label" %in% names(result))
  expect_equal(
    result$study_label,
    c("Albeigh (2008)", "Baker (2009)", "Albeigh (2008)")
  )
  expect_true(is.integer(result$study_id))
  expect_equal(result$study_id, c(1L, 2L, 1L))
})


test_that("compute_optional_columns overwrites conflicting existing study_label", {
  df <- data.frame(
    study_id = c("Study A", "Study B", "Study A"),
    study_label = c("old", "old", "old"),
    effect = c(0.2, 0.5, 0.1),
    se = c(0.1, 0.2, 0.1),
    n_obs = c(120, 150, 120),
    stringsAsFactors = FALSE
  )

  local_compute_options()

  result <- compute_optional_columns(df)

  expect_equal(result$study_label, c("Study A", "Study B", "Study A"))
  expect_equal(result$study_id, c(1L, 2L, 1L))
})


test_that("compute_optional_columns recomputes a user-supplied precision column when winsorization is active", {
  df <- data.frame(
    study_id = c("Albeigh (2008)", "Baker (2009)", "Albeigh (2008)"),
    effect = c(0.2, 0.5, 0.1),
    se = c(0.1, 0.2, 0.1),
    n_obs = c(120, 150, 120),
    precision = c(999, 999, 999),
    stringsAsFactors = FALSE
  )

  withr::local_options(list("artma.data.winsorization_level" = 0.01))
  local_compute_options()

  result <- compute_optional_columns(df)

  expect_equal(result$precision, 1 / result$se)
})


test_that("compute_optional_columns keeps a user-supplied precision column when winsorization is disabled", {
  df <- data.frame(
    study_id = c("Albeigh (2008)", "Baker (2009)", "Albeigh (2008)"),
    effect = c(0.2, 0.5, 0.1),
    se = c(0.1, 0.2, 0.1),
    n_obs = c(120, 150, 120),
    precision = c(999, 999, 999),
    stringsAsFactors = FALSE
  )

  local_compute_options()

  result <- compute_optional_columns(df)

  expect_equal(result$precision, c(999, 999, 999))
})


test_that("get_reserved_colnames covers every column compute_optional_columns can add", {
  df <- sample_study_df()
  local_compute_options()

  cols_before <- names(df)
  result <- compute_optional_columns(df)
  cols_added <- setdiff(names(result), cols_before)

  expect_true(length(cols_added) > 0)
  expect_true(all(cols_added %in% get_reserved_colnames()))
})
