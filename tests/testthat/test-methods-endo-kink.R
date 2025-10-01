box::use(testthat[test_that, expect_equal, test_path, expect_true, skip_on_cran])

skip_on_cran()

legacy_run_endokink <- function(data) {
  env <- new.env()
  if (requireNamespace("lmtest", quietly = TRUE)) {
    env$coeftest <- get("coeftest", envir = asNamespace("lmtest"))
  } else {
    env$coeftest <- function(model) summary(model)$coefficients
  }
  sys.source(test_path("..", "..", "utils", "src", "methods", "endo_kink_master_thesis_cala.R"), envir = env)
  env$runEndoKink(data, verbose = FALSE)
}

create_sample_dataset <- function() {
  data.frame(
    effect = c(0.2, 0.15, -0.05, 0.3, 0.1),
    se = c(0.1, 0.12, 0.09, 0.11, 0.15)
  )
}

test_that("refactored endo-kink matches legacy implementation", {
  box::use(artma / calc / methods / endo_kink[run_endogenous_kink]) # nolint
  data <- create_sample_dataset()
  legacy <- legacy_run_endokink(data)
  modern <- run_endogenous_kink(data, verbose = FALSE)
  expect_equal(modern, legacy)
})

test_that("helper pipeline reproduces intermediate calculations", {
  box::use(artma / calc / methods / endo_kink[prepare_endokink_columns, fit_auxiliary_lm, select_combined_regression, compute_variance_component, compute_cutoff]) # nolint
  data <- create_sample_dataset()
  prepared <- prepare_endokink_columns(data)
  expect_equal(names(prepared), c("bs", "sebs", "ones", "sebs2", "wis", "bs_sebs", "ones_sebs", "bswis"))
  pet <- fit_auxiliary_lm(bs_sebs ~ 0 + ones_sebs + ones, prepared, "ones_sebs")
  peese <- fit_auxiliary_lm(bs_sebs ~ 0 + ones_sebs + sebs, prepared, "ones_sebs")
  selected <- select_combined_regression(pet$estimate, pet$std_error, peese$estimate, peese$model, pet$model)
  variance <- compute_variance_component(selected$residual_sum, prepared, peese$model)
  cutoff <- compute_cutoff(selected$estimate, variance$standard_deviation)
  expect_true(is.numeric(cutoff))
})
