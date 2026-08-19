box::use(
  testthat[
    expect_equal,
    expect_true,
    test_that
  ]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")

#' Characterization test on a real published dataset
#'
#' The fixture is every 10th row of the long-run subsample (srun == 0) of the
#' Armington elasticity dataset of Bajzik, Havranek, Irsova & Schwarz (2020),
#' Journal of International Economics 127, 103383, using the 2.5%-winsorized
#' effect and standard error columns the authors publish at
#' https://meta-analysis.cz/data/v1/armington/armington.csv. The full-sample
#' analogues of these regressions reproduce the paper's Table 2 digit for
#' digit (see scripts/replication/); this trimmed copy exists so the linear
#' battery's point estimates stay pinned to a real effect/SE distribution on
#' every test run, without network access.
#'
#' The expected values are characterization numbers computed from this fixture,
#' not the paper's (the paper uses the full subsample). If a deliberate numeric
#' change moves them, re-run the block at the bottom of this file's history to
#' regenerate, and re-run scripts/replication/ to re-judge the real thing.

fixture_path <- testthat::test_path("fixtures", "armington_longrun_sample.csv")

load_fixture <- function() {
  df <- utils::read.csv(fixture_path, stringsAsFactors = FALSE)
  df$precision <- 1 / df$se
  df
}

linear_opts <- list(
  add_significance_marks = FALSE,
  bootstrap_replications = 0L,
  conf_level = 0.95,
  round_to = 3L
)

expected <- data.frame(
  model = c(
    "ols", "ols",
    "fe", "fe",
    "be", "be",
    "re", "re",
    "ols_precision_weighted", "ols_precision_weighted"
  ),
  term = rep(c("effect", "publication_bias"), 5),
  estimate = c(
    0.7950149638, 0.9812291826,
    0.9779283119, 0.7731442085,
    0.6875580651, 1.3917822059,
    1.0466165424, 0.8513163967,
    1.5997166390, -2.5579027580
  ),
  std_error = c(
    0.1682774322, 0.1103957277,
    0.0688038062, 0.0781399156,
    0.2390436977, 0.1978269201,
    0.1939583463, 0.0748556310,
    0.3195925185, 1.7593117608
  ),
  stringsAsFactors = FALSE
)

test_that("linear battery point estimates are pinned on real published data", {
  testthat::skip_if_not_installed("plm")
  box::use(artma / econometric / linear[run_linear_models])

  res <- run_linear_models(load_fixture(), linear_opts)
  co <- res$coefficients

  for (i in seq_len(nrow(expected))) {
    row <- co[co$model == expected$model[i] & co$term == expected$term[i], , drop = FALSE]
    expect_equal(nrow(row), 1L)
    expect_equal(
      row$estimate,
      expected$estimate[i],
      tolerance = 1e-8,
      label = sprintf("%s/%s estimate", expected$model[i], expected$term[i])
    )
    expect_equal(
      row$std_error,
      expected$std_error[i],
      tolerance = 1e-8,
      label = sprintf("%s/%s std_error", expected$model[i], expected$term[i])
    )
  }
})

test_that("the vendored fixture itself has not drifted", {
  df <- utils::read.csv(fixture_path, stringsAsFactors = FALSE)
  expect_equal(nrow(df), 297L)
  expect_equal(length(unique(df$study_id)), 36L)
  # Winsorized column bounds from the published dataset.
  expect_equal(min(df$effect), -0.996, tolerance = 1e-3)
  expect_true(max(df$effect) <= 8.51)
  expect_true(all(df$se > 0))
})
