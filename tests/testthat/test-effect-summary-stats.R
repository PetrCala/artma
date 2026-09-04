box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_identical,
    expect_named,
    expect_true,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / methods / effect_summary_stats[effect_summary_stats]
)

make_config_entry <- function(name, verbose, data_type, equal = NA_real_, gltl = NA_real_) {
  list(
    var_name = name,
    var_name_verbose = verbose,
    data_type = data_type,
    effect_sum_stats = TRUE,
    effect_summary_stats = TRUE,
    equal = equal,
    gltl = gltl,
    gtlt = gltl
  )
}

test_that("effect summary stats computes segmented summaries", {
  local_options(
    "artma.data.columns" = list(
      group = make_config_entry("group", "Group", "int", equal = 1),
      score = make_config_entry("score", "Score", "float", gltl = "median")
    ),
    "artma.methods.effect_summary_stats.conf_level" = 0.95,
    "artma.methods.effect_summary_stats.formal_output" = FALSE,
    "artma.output.number_of_decimals" = 3,
    "artma.verbose" = 1
  )

  df <- data.frame(
    effect = c(0.1, 0.2, 0.3, 0.4, NA),
    study_size = c(10, 10, 20, 20, 30),
    study_id = c("s1", "s1", "s2", "s3", "s3"),
    group = c(1, 1, 2, 2, 1),
    score = c(1, 2, 3, 4, 5)
  )

  result <- effect_summary_stats(df)$tables$summary

  expect_named(result, c(
    "Var Name", "Var Class", "Mean", "CI lower", "CI upper",
    "Weighted Mean", "WM CI lower", "WM CI upper",
    "Median", "Min", "Max", "SD", "Obs"
  ))
  expect_equal(result$`Var Name`, c(
    "All Data",
    "Group = 1",
    "Score >= 2.5",
    "Score < 2.5"
  ))
  # Weighted mean weights each estimate by 1/study_size (equal weight per
  # study); its CI uses the CR1 cluster-robust SE clustered on study_id,
  # sqrt(G / (G - 1) * sum_g (sum_{i in g} w_i (x_i - xbar_w))^2). The
  # "Group = 1" and "Score < 2.5" subsets hold a single study, so their
  # weighted-mean interval is NA. The unweighted CI uses the unrounded SD.
  expect_equal(result$Mean, c(0.25, 0.15, 0.35, 0.15))
  expect_equal(result$`CI lower`, c(0.123, 0.052, 0.252, 0.052))
  expect_equal(result$`CI upper`, c(0.377, 0.248, 0.448, 0.248))
  expect_equal(result$`Weighted Mean`, c(0.217, 0.15, 0.35, 0.15))
  expect_equal(result$`WM CI lower`, c(0.083, NA, 0.252, NA))
  expect_equal(result$`WM CI upper`, c(0.350, NA, 0.448, NA))
  expect_equal(result$Median, c(0.25, 0.15, 0.35, 0.15))
  expect_equal(result$Min, c(0.1, 0.1, 0.3, 0.1))
  expect_equal(result$Max, c(0.4, 0.2, 0.4, 0.2))
  expect_equal(result$SD, c(0.129, 0.071, 0.071, 0.071))
  expect_identical(result$Obs, c(4L, 2L, 2L, 2L))
})

test_that("formal output hides presentation columns", {
  local_options(
    "artma.data.columns" = list(
      group = make_config_entry("group", "Group", "int", equal = 1)
    ),
    "artma.methods.effect_summary_stats.conf_level" = 0.9,
    "artma.methods.effect_summary_stats.formal_output" = TRUE,
    "artma.output.number_of_decimals" = 2,
    "artma.verbose" = 1
  )

  df <- data.frame(
    effect = c(0.2, 0.2, 0.3),
    study_size = c(5, 5, 10),
    study_id = c("s1", "s1", "s2"),
    group = c(1, 1, 0)
  )

  result <- effect_summary_stats(df)$tables$summary

  expect_named(result, c(
    "Var Name", "Mean", "CI lower", "CI upper",
    "Weighted Mean", "WM CI lower", "WM CI upper", "Obs"
  ))
  expect_identical(result$`Var Name`, c("All Data", "Group = 1"))
})

# Clustered weighted-mean interval --------------------------------------------
# https://github.com/PetrCala/artma/issues/523: estimates are nested in
# studies, so the weighted mean's SE clusters on study_id like linear_tests.

pooled_only_options <- function(conf_level = 0.95) {
  local_options(
    "artma.data.columns" = list(
      effect = list(var_name = "effect", effect_sum_stats = NA, equal = NA, gltl = NA)
    ),
    "artma.methods.effect_summary_stats.conf_level" = conf_level,
    "artma.methods.effect_summary_stats.formal_output" = FALSE,
    "artma.output.number_of_decimals" = 3,
    "artma.verbose" = 1,
    .local_envir = parent.frame()
  )
}

test_that("weighted-mean interval matches sandwich::vcovCL clustered on study_id", {
  pooled_only_options()

  set.seed(523)
  n_studies <- 12
  study_size <- sample(1:8, n_studies, replace = TRUE)
  study_id <- rep(paste0("s", seq_len(n_studies)), times = study_size)
  study_effect <- stats::rnorm(n_studies, 0.3, 0.4)
  df <- data.frame(
    effect = study_effect[match(study_id, unique(study_id))] + stats::rnorm(length(study_id), 0, 0.1),
    study_size = rep(study_size, times = study_size),
    study_id = study_id,
    stringsAsFactors = FALSE
  )

  estimates <- effect_summary_stats(df)$estimates
  weighted <- estimates[estimates$term == "weighted_mean", , drop = FALSE]

  # The same estimator linear_tests reports: CR1 on an intercept-only weighted
  # regression, clustered on study_id.
  weights <- 1 / df$study_size
  fit <- stats::lm(effect ~ 1, data = df, weights = weights)
  se <- sqrt(sandwich::vcovCL(fit, cluster = df$study_id, type = "HC1")[1, 1])
  z <- stats::qnorm(0.975)

  expect_equal(weighted$estimate, unname(stats::coef(fit)[1]))
  expect_equal(weighted$conf_low, unname(stats::coef(fit)[1]) - z * se)
  expect_equal(weighted$conf_high, unname(stats::coef(fit)[1]) + z * se)
  expect_identical(unique(estimates$n_clusters), as.integer(n_studies))
  expect_identical(weighted$note, "Standard errors: Cluster-robust (HC1)")
  expect_true(all(is.na(estimates$note[estimates$term != "weighted_mean"])))
})

test_that("clustered weighted-mean interval widens against the independent one", {
  pooled_only_options()

  # Two studies whose estimates sit tightly around study-specific means: the
  # independent-observation SE sees many near-identical points, the clustered
  # SE sees two.
  df <- data.frame(
    effect = c(0.10, 0.11, 0.09, 0.10, 0.50, 0.51, 0.49, 0.50),
    study_size = rep(4, 8),
    study_id = rep(c("a", "b"), each = 4),
    stringsAsFactors = FALSE
  )

  estimates <- effect_summary_stats(df)$estimates
  weighted <- estimates[estimates$term == "weighted_mean", , drop = FALSE]

  norm_weights <- rep(1 / 8, 8)
  independent_se <- sqrt(sum(norm_weights^2 * (df$effect - weighted$estimate)^2))
  clustered_half_width <- (weighted$conf_high - weighted$conf_low) / 2

  expect_equal(weighted$estimate, 0.3)
  expect_true(clustered_half_width > stats::qnorm(0.975) * independent_se)
})

test_that("a single-study subset reports no weighted-mean interval", {
  pooled_only_options()

  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    study_size = c(3, 3, 3),
    study_id = c("only", "only", "only"),
    stringsAsFactors = FALSE
  )

  result <- effect_summary_stats(df)
  summary <- result$tables$summary
  weighted <- result$estimates[result$estimates$term == "weighted_mean", , drop = FALSE]

  expect_equal(summary$`Weighted Mean`, 0.2)
  expect_identical(summary$`WM CI lower`, NA_real_)
  expect_identical(summary$`WM CI upper`, NA_real_)
  expect_identical(weighted$n_clusters, 1L)
  expect_identical(weighted$conf_low, NA_real_)
})

test_that("effect summary stats requires the study_id column", {
  pooled_only_options()

  df <- data.frame(effect = c(0.1, 0.2), study_size = c(1, 1))

  expect_error(effect_summary_stats(df), "study_id")
})
