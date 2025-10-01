#!/usr/bin/env Rscript

box::use(
  cli[cli_inform, cli_alert_success],
  artma / calc / methods / endo_kink[
    run_endogenous_kink,
    prepare_endokink_columns,
    fit_auxiliary_lm,
    select_combined_regression,
    compute_variance_component,
    compute_cutoff
  ],
  artma / calc / methods / selection_model[
    compute_tpowers,
    variation_variance_loglikelihood,
    robust_variance,
    metastudies_estimation,
    estimates_table
  ],
  artma / calc / methods / stem[
    stem,
    stem_converge,
    stem_compute,
    variance_b,
    variance_0,
    weighted_mean,
    weighted_mean_squared
  ],
  artma / calc / methods / elliott[
    binomial_test,
    fisher_test,
    lambda2,
    bound0,
    bound1,
    bound2
  ]
)

legacy_script_path <- function(filename) {
  normalizePath(file.path("..", "..", "utils", "src", "methods", filename), mustWork = TRUE)
}

assert_equal <- function(actual, expected, info) {
  comparison <- all.equal(actual, expected)
  if (!isTRUE(comparison)) {
    details <- paste(comparison, collapse = "\n")
    cli::cli_abort(sprintf("%s:\n%s", info, details))
  }
}

assert_true <- function(condition, info) {
  if (!isTRUE(condition)) {
    cli::cli_abort(info)
  }
}

cli_inform("Validating endo-kink implementation against legacy reference...")
legacy_run_endokink <- function(data) {
  env <- new.env()
  if (requireNamespace("lmtest", quietly = TRUE)) {
    env$coeftest <- get("coeftest", envir = asNamespace("lmtest"))
  } else {
    env$coeftest <- function(model) summary(model)$coefficients
  }
  sys.source(legacy_script_path("endo_kink_master_thesis_cala.R"), envir = env)
  env$runEndoKink(data, verbose = FALSE)
}

create_endokink_dataset <- function() {
  data.frame(
    effect = c(0.2, 0.15, -0.05, 0.3, 0.1),
    se = c(0.1, 0.12, 0.09, 0.11, 0.15)
  )
}

data <- create_endokink_dataset()
legacy <- legacy_run_endokink(data)
modern <- run_endogenous_kink(data, verbose = FALSE)
assert_equal(modern, legacy, "Endo-kink results diverge from legacy implementation")

prepared <- prepare_endokink_columns(data)
assert_equal(
  names(prepared),
  c("bs", "sebs", "ones", "sebs2", "wis", "bs_sebs", "ones_sebs", "bswis"),
  "Prepared endo-kink columns mismatch"
)
pet <- fit_auxiliary_lm(bs_sebs ~ 0 + ones_sebs + ones, prepared, "ones_sebs")
peese <- fit_auxiliary_lm(bs_sebs ~ 0 + ones_sebs + sebs, prepared, "ones_sebs")
selected <- select_combined_regression(pet$estimate, pet$std_error, peese$estimate, peese$model, pet$model)
variance <- compute_variance_component(selected$residual_sum, prepared, peese$model)
cutoff <- compute_cutoff(selected$estimate, variance$standard_deviation)
assert_true(is.numeric(cutoff), "Endo-kink cutoff should be numeric")

cli_inform("Validating selection model implementation against legacy reference...")
legacy_selection_env <- function() {
  env <- new.env()
  sys.source(legacy_script_path("selection_model_master_thesis_cala.R"), envir = env)
  env
}

create_selection_fixture <- function() {
  set.seed(123)
  X <- rnorm(6, 0.2, 0.4)
  sigma <- runif(6, 0.1, 0.3)
  list(X = X, sigma = sigma, cutoffs = c(1.96, 2.58), symmetric = TRUE)
}

selection_env <- legacy_selection_env()
selection_fixture <- create_selection_fixture()
tpowers <- compute_tpowers(
  selection_fixture$X / selection_fixture$sigma,
  selection_fixture$cutoffs,
  selection_fixture$symmetric
)
legacy_tpowers <- selection_env$Tpowers_fun(
  selection_fixture$X / selection_fixture$sigma,
  selection_fixture$cutoffs,
  selection_fixture$symmetric
)
assert_equal(tpowers, legacy_tpowers, "Selection model tpowers mismatch")

variation <- variation_variance_loglikelihood(
  0.1,
  0.2,
  c(0.5, 0.5, 1),
  selection_fixture$cutoffs,
  selection_fixture$symmetric,
  selection_fixture$X,
  selection_fixture$sigma,
  tpowers
)
legacy_variation <- selection_env$VariationVarianceLogLikelihood(
  0.1,
  0.2,
  c(0.5, 0.5, 1),
  selection_fixture$cutoffs,
  selection_fixture$symmetric,
  selection_fixture$X,
  selection_fixture$sigma,
  tpowers
)
assert_equal(variation$LLH, legacy_variation$LLH, "Selection model LLH mismatch")
assert_equal(variation$logL, legacy_variation$logL, "Selection model logL mismatch")

stepsize <- 1e-6
llh <- function(theta) {
  variation_variance_loglikelihood( # nolint: object_name_linter.
    theta[1],
    theta[2],
    c(theta[-c(1, 2)], 1),
    selection_fixture$cutoffs,
    selection_fixture$symmetric,
    selection_fixture$X,
    selection_fixture$sigma,
    tpowers
  )
}
robust <- robust_variance(stepsize, length(selection_fixture$X), c(0.1, 0.2, 0.8), llh, seq_along(selection_fixture$X))
legacy_robust <- selection_env$RobustVariance(
  stepsize,
  length(selection_fixture$X),
  c(0.1, 0.2, 0.8),
  llh,
  seq_along(selection_fixture$X)
)
assert_equal(robust, legacy_robust, "Selection model robust variance mismatch")

modern_estimates <- metastudies_estimation(
  selection_fixture$X,
  selection_fixture$sigma,
  selection_fixture$cutoffs,
  selection_fixture$symmetric,
  model = "normal"
)
legacy_estimates <- selection_env$metastudies_estimation(
  selection_fixture$X,
  selection_fixture$sigma,
  selection_fixture$cutoffs,
  selection_fixture$symmetric,
  model = "normal"
)
assert_equal(modern_estimates$Psihat, legacy_estimates$Psihat, "Selection model Psihat mismatch")
assert_equal(modern_estimates$SE, legacy_estimates$SE, "Selection model SE mismatch")
modern_table <- estimates_table(
  modern_estimates$Psihat,
  modern_estimates$SE,
  selection_fixture$cutoffs,
  selection_fixture$symmetric,
  "normal"
)
legacy_table <- selection_env$estimatestable(
  legacy_estimates$Psihat,
  legacy_estimates$SE,
  selection_fixture$cutoffs,
  selection_fixture$symmetric,
  "normal"
)
assert_equal(modern_table, legacy_table, "Selection model estimates table mismatch")

cli_inform("Validating STEM implementation against legacy reference...")
legacy_stem_env <- function() {
  env <- new.env()
  env$load_packages <- function(...) NULL
  sys.source(legacy_script_path("stem_method_master_thesis_cala.R"), envir = env)
  env
}

create_stem_fixture <- function() {
  beta <- c(0.2, 0.15, 0.1, 0.05, 0)
  se <- c(0.1, 0.12, 0.11, 0.13, 0.14)
  list(beta = beta, se = se, param = c(1e-4, 100))
}

stem_env <- legacy_stem_env()
stem_fixture <- create_stem_fixture()
stem_legacy <- stem_env$stem(stem_fixture$beta, stem_fixture$se, stem_fixture$param)
stem_modern <- stem(stem_fixture$beta, stem_fixture$se, stem_fixture$param)
assert_equal(stem_modern$estimates, stem_legacy$estimates, "STEM estimates mismatch")
assert_equal(stem_modern$MSE, stem_legacy$MSE, "STEM MSE mismatch")

sorted <- cbind(stem_fixture$beta, stem_fixture$se)[order(stem_fixture$se), ]
beta_sorted <- sorted[, 1]
se_sorted <- sorted[, 2]
assert_equal(
  weighted_mean(beta_sorted, se_sorted, 0.1),
  stem_env$weighted_mean(beta_sorted, se_sorted, 0.1),
  "STEM weighted mean mismatch"
)
assert_equal(
  weighted_mean_squared(beta_sorted, se_sorted, 0.1),
  stem_env$weighted_mean_squared(beta_sorted, se_sorted, 0.1),
  "STEM weighted mean squared mismatch"
)
assert_equal(variance_b(se_sorted, 0.1), stem_env$variance_b(se_sorted, 0.1), "STEM variance_b mismatch")
assert_equal(
  variance_0(length(beta_sorted), beta_sorted, se_sorted, mean(beta_sorted)),
  stem_env$variance_0(length(beta_sorted), beta_sorted, se_sorted, mean(beta_sorted)),
  "STEM variance_0 mismatch"
)
stem_compute_modern <- stem_compute(beta_sorted, se_sorted, 0.1)
stem_compute_legacy <- stem_env$stem_compute(beta_sorted, se_sorted, 0.1)
assert_equal(stem_compute_modern$estimates, stem_compute_legacy$estimates, "STEM compute estimates mismatch")
assert_equal(stem_compute_modern$MSE, stem_compute_legacy$MSE, "STEM compute MSE mismatch")
stem_converge_modern <- stem_converge(0.1, beta_sorted, se_sorted, stem_fixture$param)
stem_converge_legacy <- stem_env$stem_converge(0.1, beta_sorted, se_sorted, stem_fixture$param)
assert_equal(stem_converge_modern$estimates, stem_converge_legacy$estimates, "STEM converge estimates mismatch")
assert_equal(stem_converge_modern$MSE, stem_converge_legacy$MSE, "STEM converge MSE mismatch")

cli_inform("Validating Elliott implementation against legacy reference...")
legacy_elliott_env <- function() {
  env <- new.env()
  env$linspace <- function(start_, stop_, n) seq(from = start_, to = stop_, length.out = n)
  sys.source(legacy_script_path("elliott_master_thesis_cala.R"), envir = env)
  env
}

fixture_pvalues <- function() {
  c(0.01, 0.02, 0.03, 0.04, 0.05)
}

elliott_env <- legacy_elliott_env()
P <- fixture_pvalues()
assert_equal(
  binomial_test(P, 0, 0.05, "c"),
  elliott_env$Binomial(P, 0, 0.05, "c"),
  "Elliott binomial test mismatch (closed)"
)
assert_equal(
  binomial_test(P, 0, 0.05, "o"),
  elliott_env$Binomial(P, 0, 0.05, "o"),
  "Elliott binomial test mismatch (open)"
)
assert_equal(fisher_test(P, 0, 0.05), elliott_env$Fisher(P, 0, 0.05), "Elliott fisher test mismatch")
h <- seq(0, 0.5, by = 0.1)
assert_equal(lambda2(0.01, 0.02, h), elliott_env$lambda2(0.01, 0.02, h), "Elliott lambda2 mismatch")
assert_equal(bound0(0.05, 5), elliott_env$Bound0(0.05, 5), "Elliott bound0 mismatch")
assert_equal(bound1(0.05, 5), elliott_env$Bound1(0.05, 5), "Elliott bound1 mismatch")
assert_equal(bound2(0.05, 6), elliott_env$Bound2(0.05, 6), "Elliott bound2 mismatch")

cli_alert_success("Legacy parity checks completed successfully.")
