box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_named,
    expect_false,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / methods / bma[bma],
  artma / methods / best_practice_estimate[best_practice_estimate]
)

make_bpe_demo_data <- function() {
  set.seed(321)
  n_studies <- 16L
  rows_per_study <- 4L
  n <- n_studies * rows_per_study

  study_id <- rep(seq_len(n_studies), each = rows_per_study)
  citations <- stats::runif(n, min = 5, max = 150)
  top_journal <- stats::rbinom(n, size = 1, prob = 0.35)
  first_lag_instrument <- stats::rbinom(n, size = 1, prob = 0.5)
  se <- stats::runif(n, min = 0.05, max = 0.2)

  effect <- 0.1 +
    0.08 * top_journal -
    0.12 * first_lag_instrument +
    0.02 * scale(citations)[, 1] +
    stats::rnorm(n, sd = 0.08)

  data.frame(
    study_id = study_id,
    study_label = paste0("Study_", study_id),
    effect = effect,
    se = se,
    citations = citations,
    top_journal = top_journal,
    first_lag_instrument = first_lag_instrument,
    stringsAsFactors = FALSE
  )
}

make_bpe_demo_config <- function() {
  list(
    effect = list(var_name = "effect", var_name_verbose = "Effect", bma = FALSE, bpe = NA),
    se = list(var_name = "se", var_name_verbose = "SE", bma = TRUE, bpe = NA),
    citations = list(var_name = "citations", var_name_verbose = "Citations", bma = TRUE, bpe = NA),
    top_journal = list(var_name = "top_journal", var_name_verbose = "Top Journal", bma = TRUE, bpe = NA),
    first_lag_instrument = list(
      var_name = "first_lag_instrument",
      var_name_verbose = "First Lag Instrument",
      bma = TRUE,
      bpe = NA
    )
  )
}

test_that("best_practice_estimate uses provided BMA result and returns structured output", {
  skip_if_not_installed("BMS")

  df <- make_bpe_demo_data()

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.data.columns = make_bpe_demo_config(),
    artma.visualization.export_graphics = FALSE,
    artma.methods.bma.burn = 50L,
    artma.methods.bma.iter = 300L,
    artma.methods.bma.nmodel = 20L,
    artma.methods.bma.g = "UIP",
    artma.methods.bma.mprior = "uniform",
    artma.methods.bma.mcmc = "bd",
    artma.methods.best_practice_estimate.include_study_rows = FALSE
  ))

  bma_result <- bma(df)
  result <- best_practice_estimate(df, bma_result = bma_result)

  expect_named(result, c("tables", "plots", "meta"))
  expect_named(result$tables, c("summary", "economic_significance"), ignore.order = TRUE)
  expect_named(
    result$meta,
    c("formula", "overrides", "bma_formula", "bma_source", "autonomy_level"),
    ignore.order = TRUE
  )
  expect_equal(result$meta$bma_source, "provided")
  expect_true(is.data.frame(result$tables$summary))
  expect_true(nrow(result$tables$summary) >= 1)
  expect_true("estimate" %in% colnames(result$tables$summary))
  expect_true(is.data.frame(result$meta$overrides))

  overrides <- stats::setNames(result$meta$overrides$override, result$meta$overrides$variable)
  expect_equal(overrides[["se"]], "0")
  expect_equal(overrides[["citations"]], "max")
  expect_equal(overrides[["first_lag_instrument"]], "0")

  econ_sig <- result$tables$economic_significance
  expect_true(is.data.frame(econ_sig))
  expect_named(
    econ_sig,
    c("variable", "var_label", "pip", "sd_change", "sd_change_pct", "range_change", "range_change_pct")
  )
  expect_equal(sort(econ_sig$variable), sort(c("citations", "first_lag_instrument", "se", "top_journal")))
})

test_that("best_practice_estimate filters economic significance by PIP threshold", {
  skip_if_not_installed("BMS")

  df <- make_bpe_demo_data()

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.data.columns = make_bpe_demo_config(),
    artma.visualization.export_graphics = FALSE,
    artma.methods.bma.burn = 50L,
    artma.methods.bma.iter = 300L,
    artma.methods.bma.nmodel = 20L,
    artma.methods.bma.g = "UIP",
    artma.methods.bma.mprior = "uniform",
    artma.methods.bma.mcmc = "bd",
    artma.methods.best_practice_estimate.include_study_rows = FALSE,
    artma.methods.best_practice_estimate.economic_significance_pip_threshold = 0.999
  ))

  bma_result <- bma(df)
  result <- best_practice_estimate(df, bma_result = bma_result)

  econ_sig <- result$tables$economic_significance
  expect_true(nrow(econ_sig) <= 4L)
  expect_true(all(econ_sig$pip >= 0.999))
})

test_that("best_practice_estimate fails early when BMA is missing and auto-run is disabled", {
  skip_if_not_installed("BMS")

  df <- make_bpe_demo_data()

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.data.columns = make_bpe_demo_config(),
    artma.methods.best_practice_estimate.run_bma_if_missing = FALSE
  ))

  expect_error(
    best_practice_estimate(df, bma_result = NULL),
    "requires a BMA result"
  )
})

test_that("best_practice_estimate accepts logical BPE overrides from config", {
  skip_if_not_installed("BMS")

  df <- make_bpe_demo_data()
  config <- make_bpe_demo_config()
  config$top_journal$bpe <- TRUE
  config$first_lag_instrument$bpe <- FALSE

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "ask_more",
    artma.data.columns = config,
    artma.visualization.export_graphics = FALSE,
    artma.methods.bma.burn = 50L,
    artma.methods.bma.iter = 300L,
    artma.methods.bma.nmodel = 20L,
    artma.methods.bma.g = "UIP",
    artma.methods.bma.mprior = "uniform",
    artma.methods.bma.mcmc = "bd",
    artma.methods.best_practice_estimate.include_study_rows = FALSE
  ))

  bma_result <- bma(df)
  result <- best_practice_estimate(df, bma_result = bma_result)

  overrides <- stats::setNames(result$meta$overrides$override, result$meta$overrides$variable)
  expect_equal(overrides[["top_journal"]], "1")
  expect_equal(overrides[["first_lag_instrument"]], "0")
  expect_false(is.na(overrides[["se"]]))
})
