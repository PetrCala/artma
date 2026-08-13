box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_message,
    expect_no_message,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options],
  artma / econometric / vcov[robust_vcov, vcov_type]
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
  expect_equal(result, oracle, ignore_attr = "vcov_type")

  # Pinned golden values captured from the original ladder.
  expect_equal(unname(result["se", "se"]), 0.092270063, tolerance = 1e-7)
  expect_equal(unname(result["(Intercept)", "(Intercept)"]), 0.0009163101, tolerance = 1e-7)
})

test_that("get_robust_vcov ladder falls back to non-clustered HC1 on cluster error", {
  skip_if_not_installed("sandwich")
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)

  # A wrong-length cluster makes vcovCL error; the ladder should fall through
  # to the non-clustered HC1 step, warning about the downgrade along the way -
  # not itself under test here; the test session's global sink (see
  # setup.R) keeps it off the log.
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
  expect_equal(result, oracle, ignore_attr = "vcov_type")
})

test_that("robust_vcov warns when clustering is silently lost", {
  skip_if_not_installed("sandwich")
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)

  # Clustered step fails on a wrong-length cluster: the fallback must announce
  # that the returned SEs are not clustered. The alert is real signal here
  # (it's exactly what's under test); the test session's global sink (see
  # setup.R) keeps it off the log while expect_message()'s own inner handler
  # still sees the signaled condition.
  expect_message(
    robust_vcov(
      model = model,
      cluster = c(1, 2, 3),
      engine = "sandwich",
      clustered_type = "HC1",
      fallback_types = c("HC1", "HC0"),
      suppress_warnings = TRUE
    ),
    regexp = "NOT clustered"
  )

  # The match_cluster_length guard is the same downgrade and must also warn.
  expect_message(
    robust_vcov(
      model = model,
      cluster = c(1, 2, 3),
      engine = "sandwich",
      clustered_type = "HC0",
      match_cluster_length = TRUE
    ),
    regexp = "NOT clustered"
  )

  # A working clustered step stays quiet.
  expect_no_message(
    robust_vcov(
      model = model,
      cluster = df$study_id,
      engine = "sandwich",
      clustered_type = "HC1"
    )
  )

  # Verbosity 1 keeps the run errors-only.
  local_options("artma.verbose" = 1)
  expect_no_message(
    robust_vcov(
      model = model,
      cluster = c(1, 2, 3),
      engine = "sandwich",
      clustered_type = "HC1",
      fallback_types = c("HC1", "HC0")
    )
  )
})

test_that("robust_vcov warns on the no-cluster single-step downgrade to stats::vcov()", {
  skip_if_not_installed("sandwich")
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)

  # No cluster and no fallback_types: the sole (non-clustered) HC step is
  # forced to fail with a bogus type, so the ladder falls through to
  # stats::vcov(). This downgrade must still be warned about even though
  # clustering was never requested and there is only one robust step.
  expect_message(
    robust_vcov(
      model = model,
      cluster = NULL,
      engine = "sandwich",
      clustered_type = "BOGUS",
      fallback_types = character(0)
    ),
    regexp = "NOT clustered"
  )
})

test_that("robust_vcov attributes the downgrade warning to the named model", {
  skip_if_not_installed("sandwich")
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)

  # A run estimates several models at once, so the warning has to name the one
  # it applies to; an unattributed line reads as if every column were affected.
  expect_message(
    robust_vcov(
      model = model,
      cluster = c(1, 2, 3),
      engine = "sandwich",
      clustered_type = "HC1",
      fallback_types = "HC1",
      label = "Between Effects"
    ),
    regexp = "Between Effects: standard errors are NOT clustered"
  )
})

test_that("robust_vcov tags the result with the estimator that produced it", {
  skip_if_not_installed("sandwich")
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)

  clustered <- robust_vcov(
    model = model,
    cluster = df$study_id,
    engine = "sandwich",
    clustered_type = "HC1"
  )
  expect_equal(vcov_type(clustered), "Cluster-robust (HC1)")

  downgraded <- robust_vcov(
    model = model,
    cluster = c(1, 2, 3),
    engine = "sandwich",
    clustered_type = "HC1",
    fallback_types = "HC0"
  )
  expect_equal(vcov_type(downgraded), "Heteroskedasticity-robust (HC0)")

  classical <- robust_vcov(
    model = model,
    cluster = NULL,
    engine = "sandwich",
    clustered_type = "BOGUS"
  )
  expect_equal(vcov_type(classical), "Classical (non-robust)")

  # An untagged matrix (e.g. one built outside the ladder) reports NA.
  expect_true(is.na(vcov_type(stats::vcov(model))))
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
  expect_equal(result, oracle, ignore_attr = "vcov_type")
  expect_equal(unname(result["se", "se"]), 0.089088337, tolerance = 1e-7)
})

test_that("resolve_bpe_vcov ladder uses non-clustered HC0 when cluster length mismatches", {
  skip_if_not_installed("sandwich")
  df <- make_vcov_fixture()
  model <- stats::lm(effect ~ se, data = df)

  # Wrong-length cluster: the length guard rejects it and the primary step is
  # the non-clustered HC0 vcov, warning about the downgrade along the way -
  # not itself under test here; the test session's global sink (see
  # setup.R) keeps it off the log.
  result <- robust_vcov(
    model = model,
    cluster = c(1, 2, 3),
    engine = "sandwich",
    clustered_type = "HC0",
    match_cluster_length = TRUE
  )

  oracle <- sandwich::vcovHC(model, type = "HC0")
  expect_equal(result, oracle, ignore_attr = "vcov_type")
  expect_equal(unname(result["se", "se"]), 0.1105633, tolerance = 1e-6)

  # NULL cluster follows the same non-clustered branch.
  result_null <- robust_vcov(
    model = model,
    cluster = NULL,
    engine = "sandwich",
    clustered_type = "HC0",
    match_cluster_length = TRUE
  )
  expect_equal(result_null, oracle, ignore_attr = "vcov_type")
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
  expect_equal(result, oracle, ignore_attr = "vcov_type")
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
  expect_equal(result, oracle, ignore_attr = "vcov_type")
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
  expect_equal(result, oracle, ignore_attr = "vcov_type")
  expect_equal(unname(result["se", "se"]), 0.1058499, tolerance = 1e-6)
})
