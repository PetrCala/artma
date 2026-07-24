box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  artma / econometric / vcov[robust_vcov]
)

# Deterministic fixture matching the shape used across the linear tests: an
# intercept + `se` slope model with a repeating `study_id` cluster.
make_vcov_fixture <- function() {
  set.seed(42)
  n_studies <- 6L
  per_study <- 5L
  study_ids <- rep(seq_len(n_studies), each = per_study)
  se_vals <- runif(n_studies * per_study, 0.05, 0.15)
  data.frame(
    study_id = study_ids,
    effect = rnorm(n_studies * per_study, 0.2, 0.05),
    se = se_vals,
    study_size = sample(20:80, n_studies * per_study, replace = TRUE),
    precision = 1 / se_vals
  )
}

# --- sandwich engine: exogeneity get_robust_vcov ladder --------------------

test_that("robust_vcov reproduces the get_robust_vcov clustered HC1 ladder", {
  skip_if_not_installed("sandwich")
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)

  result <- robust_vcov(
    model = model,
    cluster = df$study_id,
    engine = "sandwich",
    clustered_type = "HC1",
    fallback_types = c("HC1", "HC0"),
    require_cluster = TRUE,
    suppress_warnings = TRUE
  )

  oracle <- suppressWarnings(
    sandwich::vcovCL(model, cluster = df$study_id, type = "HC1")
  )
  expect_equal(result, oracle)

  # Pinned golden values captured from the original ladder.
  expect_equal(unname(result["se", "se"]), 0.092270063, tolerance = 1e-7)
  expect_equal(unname(result["(Intercept)", "(Intercept)"]), 0.0009163101, tolerance = 1e-7)
})

test_that("get_robust_vcov ladder falls back to non-clustered HC1 on cluster error", {
  skip_if_not_installed("sandwich")
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)

  # A wrong-length cluster makes vcovCL error; the ladder should fall through
  # to the non-clustered HC1 step.
  result <- robust_vcov(
    model = model,
    cluster = c(1, 2, 3),
    engine = "sandwich",
    clustered_type = "HC1",
    fallback_types = c("HC1", "HC0"),
    require_cluster = TRUE,
    suppress_warnings = TRUE
  )

  oracle <- suppressWarnings(sandwich::vcovHC(model, type = "HC1"))
  expect_equal(result, oracle)
})

test_that("robust_vcov errors when a required cluster is NULL", {
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)
  expect_error(
    robust_vcov(model = model, cluster = NULL, require_cluster = TRUE)
  )
})

# --- sandwich engine: resolve_bpe_vcov ladder ------------------------------

test_that("robust_vcov reproduces resolve_bpe_vcov clustered HC0 (matching length)", {
  skip_if_not_installed("sandwich")
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)

  result <- robust_vcov(
    model = model,
    cluster = df$study_id,
    engine = "sandwich",
    clustered_type = "HC0",
    match_cluster_length = TRUE
  )

  oracle <- sandwich::vcovCL(model, cluster = df$study_id, type = "HC0")
  expect_equal(result, oracle)
  expect_equal(unname(result["se", "se"]), 0.089088337, tolerance = 1e-7)
})

test_that("resolve_bpe_vcov ladder uses non-clustered HC0 when cluster length mismatches", {
  skip_if_not_installed("sandwich")
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)

  # Wrong-length cluster: the length guard rejects it and the primary step is
  # the non-clustered HC0 vcov.
  result <- robust_vcov(
    model = model,
    cluster = c(1, 2, 3),
    engine = "sandwich",
    clustered_type = "HC0",
    match_cluster_length = TRUE
  )

  oracle <- sandwich::vcovHC(model, type = "HC0")
  expect_equal(result, oracle)
  expect_equal(unname(result["se", "se"]), 0.1105633, tolerance = 1e-6)

  # NULL cluster follows the same non-clustered branch.
  result_null <- robust_vcov(
    model = model,
    cluster = NULL,
    engine = "sandwich",
    clustered_type = "HC0",
    match_cluster_length = TRUE
  )
  expect_equal(result_null, oracle)
})

# --- sandwich engine: tidy_lm_model ladder ---------------------------------

test_that("robust_vcov reproduces tidy_lm_model HC1 ladder without a vcov last resort", {
  skip_if_not_installed("sandwich")
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)

  result <- robust_vcov(
    model = model,
    cluster = df$study_id,
    engine = "sandwich",
    clustered_type = "HC1",
    fallback_types = "HC1",
    final_vcov_fallback = FALSE
  )

  oracle <- tryCatch(
    sandwich::vcovCL(model, cluster = df$study_id, type = "HC1"),
    error = function(e) sandwich::vcovHC(model, type = "HC1")
  )
  expect_equal(result, oracle)
})

# --- plm engine: tidy_plm_generic / tidy_plm_within ladders ----------------

test_that("robust_vcov reproduces the tidy_plm_generic HC1/HC0 ladder", {
  skip_if_not_installed("plm")
  df <- make_vcov_fixture()
  model <- plm::plm(effect ~ se, data = df, model = "random", index = "study_id")

  result <- robust_vcov(
    model = model,
    cluster = "group",
    engine = "plm",
    clustered_type = "HC1",
    fallback_types = "HC0"
  )

  oracle <- tryCatch(
    plm::vcovHC(model, type = "HC1", cluster = "group"),
    error = function(e) {
      tryCatch(
        plm::vcovHC(model, type = "HC0"),
        error = function(e2) stats::vcov(model)
      )
    }
  )
  expect_equal(result, oracle)
  expect_equal(unname(result["se", "se"]), 0.086204523, tolerance = 1e-7)
})

test_that("robust_vcov reproduces the tidy_plm_within HC1/HC0 ladder", {
  skip_if_not_installed("plm")
  df <- make_vcov_fixture()
  model <- plm::plm(effect ~ se, data = df, model = "within", index = "study_id")

  result <- robust_vcov(
    model = model,
    cluster = "group",
    engine = "plm",
    clustered_type = "HC1",
    fallback_types = "HC0",
    final_vcov_fallback = FALSE
  )

  oracle <- tryCatch(
    plm::vcovHC(model, type = "HC1", cluster = "group"),
    error = function(e) plm::vcovHC(model, type = "HC0")
  )
  expect_equal(result, oracle)
  expect_equal(unname(result["se", "se"]), 0.1058499, tolerance = 1e-6)
})
