box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_gt,
    expect_named,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / econometric / linear[
    linear_model_specs,
    resample_cluster_rows,
    run_linear_models
  ],
  artma / methods / linear_tests[linear_tests]
)

make_demo_data <- function() {
  set.seed(42)
  n_studies <- 6L
  per_study <- 5L
  study_ids <- rep(paste0("S", seq_len(n_studies)), each = per_study)
  se_vals <- runif(n_studies * per_study, min = 0.05, max = 0.15)
  data.frame(
    study_id = study_ids,
    effect = rnorm(n_studies * per_study, mean = 0.2, sd = 0.05),
    se = se_vals,
    study_size = sample(20:80, n_studies * per_study, replace = TRUE),
    precision = 1 / se_vals,
    check.names = FALSE
  )
}

test_that("linear tests return tidy coefficients and summary", {
  skip_if_not_installed("plm")

  df <- make_demo_data()

  local_options(
    "artma.methods.add_significance_marks" = TRUE,
    "artma.methods.linear_tests.bootstrap_replications" = 10L,
    "artma.methods.linear_tests.conf_level" = 0.9,
    "artma.output.number_of_decimals" = 2,
    "artma.verbose" = 1
  )

  res <- linear_tests(df)

  expect_named(res, c("tables", "plots", "meta"))
  expect_named(res$tables, "summary")
  expect_named(
    res$meta,
    c("coefficients", "skipped", "options"),
    ignore.order = TRUE
  )

  expect_equal(
    sort(unique(res$meta$coefficients$model)),
    sort(c(
      "ols", "fe", "be", "re", "ols_study_weighted", "ols_precision_weighted"
    ))
  )

  expect_equal(nrow(res$meta$coefficients), 12L)
  expect_named(
    res$meta$coefficients,
    c(
      "estimate", "std_error", "statistic", "p_value", "term", "model",
      "model_label", "n_obs", "term_label", "bootstrap_lower",
      "bootstrap_upper", "significance", "estimate_rounded",
      "std_error_rounded", "estimate_formatted", "std_error_formatted",
      "bootstrap_formatted"
    )
  )

  expect_gt(nrow(res$tables$summary), 0)
  expect_equal(
    rownames(res$tables$summary),
    c(
      "Publication Bias", "(Std. Error)", "Bootstrap CI (PB)",
      "Effect Beyond Bias", "(Std. Error)", "Bootstrap CI (Effect)",
      "Total Observations"
    )
  )
  expect_true(all(res$meta$coefficients$significance %in% c("", "*", "**", "***")))
  expect_equal(res$meta$options$bootstrap_replications, 10L)
})

test_that("linear tests gracefully skip models with missing columns", {
  df <- make_demo_data()
  df$precision <- NULL

  local_options(
    "artma.methods.add_significance_marks" = FALSE,
    "artma.methods.linear_tests.bootstrap_replications" = 0L,
    "artma.methods.linear_tests.conf_level" = 0.95,
    "artma.output.number_of_decimals" = 3,
    "artma.verbose" = 1
  )

  res <- linear_tests(df)

  expect_false("ols_precision_weighted" %in% res$meta$coefficients$model)
  expect_true("ols_precision_weighted" %in% names(res$meta$skipped))
  expect_true(grepl("Missing required columns", res$meta$skipped$ols_precision_weighted$reason))
})

test_that("bootstrap CIs are deterministic and seed-identical to the tidy-path implementation", {
  skip_if_not_installed("plm")

  df <- make_demo_data()
  opts <- list(
    add_significance_marks = FALSE,
    bootstrap_replications = 50L,
    conf_level = 0.9,
    round_to = 3L
  )

  set.seed(123)
  res <- run_linear_models(df, options = opts)

  set.seed(123)
  res_repeat <- run_linear_models(df, options = opts)

  ci_cols <- c("model", "term", "bootstrap_lower", "bootstrap_upper")
  ci <- res$coefficients[, ci_cols]
  rownames(ci) <- NULL
  expect_equal(ci, res_repeat$coefficients[, ci_cols], ignore_attr = TRUE)

  finite_rows <- is.finite(ci$bootstrap_lower) & is.finite(ci$bootstrap_upper)
  expect_true(all(ci$bootstrap_lower[finite_rows] <= ci$bootstrap_upper[finite_rows]))

  # Reference values computed with the pre-optimization implementation, which
  # ran the full clustered-vcov tidy path in every bootstrap replication. The
  # fast boot_coefs path must stay seed-identical to it.
  expected <- data.frame(
    model = rep(
      c("ols", "fe", "be", "re", "ols_study_weighted", "ols_precision_weighted"),
      each = 2L
    ),
    term = rep(c("effect", "publication_bias"), times = 6L),
    bootstrap_lower = c(
      0.160739545997458, -0.601087904579698,
      0.140819890400399, -0.600311615629511,
      NA, NA,
      0.155088688976474, -0.649012667394041,
      0.162905232119029, -0.812806189499674,
      0.120950452943169, -0.593431488826163
    ),
    bootstrap_upper = c(
      0.237035405467370, 0.165658257380718,
      0.243966037478644, 0.440057444128715,
      NA, NA,
      0.251343193299353, 0.228828857125176,
      0.259964458196330, 0.177322819406577,
      0.249764223666147, 0.467372767218415
    ),
    stringsAsFactors = FALSE
  )

  expect_equal(ci, expected, tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("bootstrap fast paths match slow-path refits on resampled data", {
  skip_if_not_installed("plm")

  set.seed(42)
  n_studies <- 12L
  per_study <- 5L
  study_ids <- rep(paste0("S", seq_len(n_studies)), each = per_study)
  se_vals <- runif(n_studies * per_study, min = 0.05, max = 0.15)
  df <- data.frame(
    study_id = droplevels(factor(study_ids)),
    effect = rnorm(n_studies * per_study, mean = 0.2, sd = 0.05),
    se = se_vals,
    study_size = sample(20:80, n_studies * per_study, replace = TRUE),
    precision = 1 / se_vals
  )
  cluster_splits <- split(seq_len(nrow(df)), df$study_id)

  specs <- linear_model_specs()
  names(specs) <- vapply(specs, function(spec) spec$name, character(1))
  fast_path_specs <- c("ols", "fe", "re", "ols_study_weighted", "ols_precision_weighted")
  compared <- stats::setNames(integer(length(fast_path_specs)), fast_path_specs)

  set.seed(1234)
  for (replication in seq_len(5L)) {
    rows <- resample_cluster_rows(nrow(df), cluster_splits)
    boot_data <- df[rows, , drop = FALSE]

    for (spec_name in fast_path_specs) {
      spec <- specs[[spec_name]]
      fast <- tryCatch(spec$boot_estimate(df, rows), error = function(e) NULL)
      slow_model <- tryCatch(spec$fit(boot_data), error = function(e) NULL)

      if (is.null(slow_model)) {
        # A degenerate resample (e.g. too few distinct clusters) must fail
        # identically on both paths.
        expect_true(is.null(fast), info = paste(spec_name, "replication", replication))
        next
      }

      slow_tidy <- spec$tidy(slow_model, boot_data)
      slow <- stats::setNames(slow_tidy$estimate, slow_tidy$term)

      expect_equal(
        fast[c("effect", "publication_bias")],
        slow[c("effect", "publication_bias")],
        tolerance = 1e-12,
        info = paste(spec_name, "replication", replication)
      )
      compared[spec_name] <- compared[spec_name] + 1L
    }
  }

  expect_true(all(compared > 0L))
})

test_that("within fast path merges duplicated resampled clusters like plm", {
  skip_if_not_installed("plm")

  df <- make_demo_data()
  df$study_id <- droplevels(factor(df$study_id))
  cluster_splits <- split(seq_len(nrow(df)), df$study_id)

  specs <- linear_model_specs()
  names(specs) <- vapply(specs, function(spec) spec$name, character(1))
  fe_spec <- specs[["fe"]]

  # Force duplicated clusters so the merge semantics are actually exercised.
  rows <- unlist(
    cluster_splits[c("S1", "S1", "S2", "S3", "S3", "S3")],
    use.names = FALSE
  )
  boot_data <- df[rows, , drop = FALSE]

  fast <- fe_spec$boot_estimate(df, rows)
  model <- plm::plm(effect ~ se, data = boot_data, model = "within", index = "study_id")

  expect_equal(
    fast[["publication_bias"]],
    unname(stats::coef(model)[["se"]]),
    tolerance = 1e-12
  )
  expect_equal(
    fast[["effect"]],
    unname(plm::within_intercept(model)[[1L]]),
    tolerance = 1e-12
  )
})

test_that("random effects fast path matches plm on duplicated resampled clusters", {
  skip_if_not_installed("plm")

  df <- make_demo_data()
  df$study_id <- droplevels(factor(df$study_id))
  cluster_splits <- split(seq_len(nrow(df)), df$study_id)

  specs <- linear_model_specs()
  names(specs) <- vapply(specs, function(spec) spec$name, character(1))
  re_spec <- specs[["re"]]

  # Force duplicated clusters so the merge semantics are actually exercised.
  rows <- unlist(
    cluster_splits[c("S1", "S1", "S2", "S3", "S3", "S3", "S5")],
    use.names = FALSE
  )
  boot_data <- df[rows, , drop = FALSE]

  fast <- re_spec$boot_estimate(df, rows)
  model <- plm::plm(effect ~ se, data = boot_data, model = "random", index = "study_id")

  expect_equal(
    fast[["effect"]],
    unname(stats::coef(model)[["(Intercept)"]]),
    tolerance = 1e-12
  )
  expect_equal(
    fast[["publication_bias"]],
    unname(stats::coef(model)[["se"]]),
    tolerance = 1e-12
  )
})

test_that("random effects fast path matches plm on unbalanced clusters", {
  skip_if_not_installed("plm")

  set.seed(7)
  sizes <- c(2L, 3L, 5L, 8L, 13L, 21L, 4L, 30L, 2L, 9L)
  study_ids <- rep(paste0("U", seq_along(sizes)), times = sizes)
  se_vals <- runif(sum(sizes), min = 0.05, max = 0.15)
  df <- data.frame(
    study_id = droplevels(factor(study_ids)),
    effect = rnorm(sum(sizes), mean = 0.2, sd = 0.05),
    se = se_vals
  )
  cluster_splits <- split(seq_len(nrow(df)), df$study_id)

  specs <- linear_model_specs()
  names(specs) <- vapply(specs, function(spec) spec$name, character(1))
  re_spec <- specs[["re"]]

  set.seed(99)
  for (replication in seq_len(5L)) {
    rows <- resample_cluster_rows(nrow(df), cluster_splits)
    boot_data <- df[rows, , drop = FALSE]

    fast <- tryCatch(re_spec$boot_estimate(df, rows), error = function(e) NULL)
    model <- tryCatch(
      plm::plm(effect ~ se, data = boot_data, model = "random", index = "study_id"),
      error = function(e) NULL
    )

    if (is.null(model)) {
      expect_true(is.null(fast), info = paste("replication", replication))
      next
    }

    expect_equal(
      fast[c("effect", "publication_bias")],
      stats::setNames(
        unname(stats::coef(model)[c("(Intercept)", "se")]),
        c("effect", "publication_bias")
      ),
      tolerance = 1e-12,
      info = paste("replication", replication)
    )
  }
})

test_that("random effects fast path fails on degenerate resamples exactly like plm", {
  skip_if_not_installed("plm")

  df <- make_demo_data()
  df$study_id <- droplevels(factor(df$study_id))
  cluster_splits <- split(seq_len(nrow(df)), df$study_id)

  specs <- linear_model_specs()
  names(specs) <- vapply(specs, function(spec) spec$name, character(1))
  re_spec <- specs[["re"]]

  degenerate_draws <- list(
    single_cluster = c("S2", "S2", "S2"),
    two_clusters = c("S2", "S5")
  )

  for (case in names(degenerate_draws)) {
    rows <- unlist(cluster_splits[degenerate_draws[[case]]], use.names = FALSE)
    boot_data <- df[rows, , drop = FALSE]

    fast <- tryCatch(re_spec$boot_estimate(df, rows), error = function(e) NULL)
    # plm warns about a perfect between fit before erroring on these draws.
    model <- suppressWarnings(tryCatch(
      plm::plm(effect ~ se, data = boot_data, model = "random", index = "study_id"),
      error = function(e) NULL
    ))

    expect_true(is.null(model), info = case)
    expect_true(is.null(fast), info = case)
  }

  # All-singleton clusters: plm rejects the within model as empty.
  singleton_df <- data.frame(
    study_id = droplevels(factor(paste0("P", 1:8))),
    effect = rnorm(8, mean = 0.2, sd = 0.05),
    se = runif(8, min = 0.05, max = 0.15)
  )
  rows <- seq_len(nrow(singleton_df))

  fast <- tryCatch(re_spec$boot_estimate(singleton_df, rows), error = function(e) NULL)
  model <- tryCatch(
    plm::plm(effect ~ se, data = singleton_df, model = "random", index = "study_id"),
    error = function(e) NULL
  )

  expect_true(is.null(model))
  expect_true(is.null(fast))
})

test_that("panel models are skipped with a clear message when plm is unavailable", {
  df <- make_demo_data()

  res <- run_linear_models(
    df,
    options = list(
      add_significance_marks = FALSE,
      bootstrap_replications = 0L,
      conf_level = 0.95,
      round_to = 3L
    ),
    is_pkg_available = function(pkg) pkg != "plm"
  )

  panel_models <- c("fe", "be", "re")

  expect_false(any(panel_models %in% res$coefficients$model))
  expect_true(all(panel_models %in% names(res$skipped)))

  reasons <- vapply(res$skipped[panel_models], function(item) item$reason, character(1))
  expect_true(all(grepl("plm", reasons, fixed = TRUE)))
  expect_true(all(grepl("install.packages", reasons, fixed = TRUE)))

  expect_true("ols" %in% res$coefficients$model)
})
