box::use(
  artma / data / compute[compute_optional_columns]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")

computed_config_overrides <- list(
  obs_id = list(var_name = "obs_id", is_computed = TRUE),
  study_id = list(var_name = "study_id", is_computed = TRUE),
  study_label = list(var_name = "study_label", is_computed = TRUE),
  t_stat = list(var_name = "t_stat", is_computed = TRUE),
  study_size = list(var_name = "study_size", is_computed = TRUE),
  reg_dof = list(var_name = "reg_dof", is_computed = TRUE),
  precision = list(var_name = "precision", is_computed = TRUE)
)


test_that("compute_optional_columns preserves study labels while normalizing study_id", {
  df <- data.frame(
    study_id = c("Albeigh (2008)", "Baker (2009)", "Albeigh (2008)"),
    effect = c(0.2, 0.5, 0.1),
    se = c(0.1, 0.2, 0.1),
    n_obs = c(120, 150, 120),
    stringsAsFactors = FALSE
  )

  withr::local_options(list(
    "artma.data.config" = computed_config_overrides,
    "artma.output.save_results" = FALSE,
    "artma.calc.precision_type" = "1/SE",
    "artma.verbose" = 1
  ))

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

  withr::local_options(list(
    "artma.data.config" = computed_config_overrides,
    "artma.output.save_results" = FALSE,
    "artma.calc.precision_type" = "1/SE",
    "artma.verbose" = 1
  ))

  result <- compute_optional_columns(df)

  expect_equal(result$study_label, c("Study A", "Study B", "Study A"))
  expect_equal(result$study_id, c(1L, 2L, 1L))
})

