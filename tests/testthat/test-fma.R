box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_named,
    expect_null,
    expect_true,
    skip_if_not_installed,
    test_that
  ]
)

box::use(
  artma / econometric / bma[get_bma_data, run_bma],
  artma / econometric / fma[run_fma],
  artma / methods / fma[resolve_fma_cluster_ids]
)

make_demo_fma_data <- function() {
  set.seed(123)
  n <- 40L
  data.frame(
    effect = rnorm(n, mean = 0.2, sd = 0.1),
    se = runif(n, min = 0.05, max = 0.15),
    moderator1 = rnorm(n),
    moderator2 = rbinom(n, size = 1, prob = 0.5),
    stringsAsFactors = FALSE
  )
}

test_that("run_fma returns coefficients and weights", {
  skip_if_not_installed("BMS")
  skip_if_not_installed("quadprog")

  df <- make_demo_fma_data()

  var_list <- data.frame(
    var_name = c("effect", "se", "moderator1", "moderator2"),
    var_name_verbose = c("Effect", "SE", "Moderator 1", "Moderator 2"),
    bma = c(TRUE, TRUE, TRUE, TRUE),
    to_log_for_bma = c(FALSE, FALSE, FALSE, FALSE),
    bma_reference_var = c(FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  bma_data <- get_bma_data(
    df,
    var_list,
    variable_info = c("effect", "se", "moderator1", "moderator2"),
    scale_data = FALSE,
    from_vector = TRUE,
    include_reference_groups = FALSE
  )

  params <- list(
    burn = 100L,
    iter = 500L,
    nmodel = 10L,
    g = "UIP",
    mprior = "uniform",
    mcmc = "bd"
  )

  bma_model <- run_bma(bma_data, params)

  result <- run_fma(
    bma_data = bma_data,
    bma_model = bma_model,
    input_var_list = var_list,
    print_results = "none"
  )

  expect_named(result, c("coefficients", "weights"))
  expect_true(is.data.frame(result$coefficients))
  expect_true(all(c("variable", "coefficient", "se", "p_value") %in% colnames(result$coefficients)))
  expect_true("Intercept" %in% result$coefficients$variable)

  expect_equal(length(result$weights), ncol(bma_data))
  expect_true(abs(sum(result$weights) - 1) < 1e-06)
  expect_true(all(result$weights >= -1e-08))
})

test_that("Mallows penalty favors small models on pure-noise predictors", {
  skip_if_not_installed("BMS")
  skip_if_not_installed("quadprog")

  set.seed(42)
  n <- 200L
  df <- data.frame(
    effect = rnorm(n),
    noise1 = rnorm(n),
    noise2 = rnorm(n),
    noise3 = rnorm(n),
    noise4 = rnorm(n),
    noise5 = rnorm(n),
    stringsAsFactors = FALSE
  )

  var_list <- data.frame(
    var_name = colnames(df),
    var_name_verbose = colnames(df),
    bma = rep(TRUE, ncol(df)),
    to_log_for_bma = rep(FALSE, ncol(df)),
    bma_reference_var = rep(FALSE, ncol(df)),
    stringsAsFactors = FALSE
  )

  bma_data <- get_bma_data(
    df,
    var_list,
    variable_info = colnames(df),
    scale_data = FALSE,
    from_vector = TRUE,
    include_reference_groups = FALSE
  )

  params <- list(
    burn = 100L, iter = 500L, nmodel = 10L,
    g = "UIP", mprior = "uniform", mcmc = "bd"
  )
  bma_model <- run_bma(bma_data, params)

  result <- run_fma(
    bma_data = bma_data,
    bma_model = bma_model,
    input_var_list = var_list,
    print_results = "none"
  )

  # None of the predictors carry signal, so the complexity penalty must pull
  # weight onto the smallest models. The sign-flipped criterion concentrated
  # nearly all weight on the largest model instead.
  m <- length(result$weights)
  small_half <- sum(result$weights[seq_len(ceiling(m / 2))])
  expect_true(small_half > 0.5)
  expect_true(result$weights[m] < 0.5)
})

# Study-clustered DGP: shared study shocks make cluster-robust and iid SEs
# diverge visibly. Every predictor carries signal so the full model keeps
# Mallows weight; a predictor excluded from all weighted models would get an
# exact-zero SE and trip the positivity checks below.
make_clustered_fma_inputs <- function() {
  set.seed(321)
  n_studies <- 12L
  per_study <- 5L
  n <- n_studies * per_study
  study <- rep(seq_len(n_studies), each = per_study)
  study_shock <- stats::rnorm(n_studies, sd = 0.1)

  se_col <- stats::runif(n, min = 0.05, max = 0.15)
  moderator1 <- stats::rnorm(n)
  moderator2 <- stats::rbinom(n, size = 1, prob = 0.5)
  df <- data.frame(
    effect = 0.2 + 2 * se_col + 0.8 * moderator1 + 0.6 * moderator2 +
      study_shock[study] + stats::rnorm(n, sd = 0.05),
    se = se_col,
    moderator1 = moderator1,
    moderator2 = moderator2,
    stringsAsFactors = FALSE
  )

  var_list <- data.frame(
    var_name = c("effect", "se", "moderator1", "moderator2"),
    var_name_verbose = c("effect", "se", "moderator1", "moderator2"),
    bma = rep(TRUE, 4L),
    to_log_for_bma = rep(FALSE, 4L),
    bma_reference_var = rep(FALSE, 4L),
    stringsAsFactors = FALSE
  )

  bma_data <- get_bma_data(
    df,
    var_list,
    variable_info = c("effect", "se", "moderator1", "moderator2"),
    scale_data = FALSE,
    from_vector = TRUE,
    include_reference_groups = FALSE
  )

  params <- list(
    burn = 100L, iter = 500L, nmodel = 10L,
    g = "UIP", mprior = "uniform", mcmc = "bd"
  )

  list(
    study = study,
    var_list = var_list,
    bma_data = bma_data,
    bma_model = run_bma(bma_data, params)
  )
}

test_that("cluster_ids changes standard errors but not coefficients or weights", {
  skip_if_not_installed("BMS")
  skip_if_not_installed("quadprog")

  inputs <- make_clustered_fma_inputs()

  iid <- run_fma(
    bma_data = inputs$bma_data,
    bma_model = inputs$bma_model,
    input_var_list = inputs$var_list,
    print_results = "none"
  )
  clustered <- run_fma(
    bma_data = inputs$bma_data,
    bma_model = inputs$bma_model,
    input_var_list = inputs$var_list,
    cluster_ids = inputs$study,
    print_results = "none"
  )

  # Clustering only reweights the score contributions inside the vcov; the
  # point estimates and the Mallows weights must be untouched.
  expect_equal(clustered$coefficients$coefficient, iid$coefficients$coefficient)
  expect_equal(clustered$weights, iid$weights)

  expect_true(all(is.finite(clustered$coefficients$se)))
  expect_true(all(clustered$coefficients$se > 0))
  expect_false(isTRUE(all.equal(clustered$coefficients$se, iid$coefficients$se)))
})

test_that("clustered FMA standard errors match a sandwich::vcovCL reference", {
  skip_if_not_installed("BMS")
  skip_if_not_installed("quadprog")

  inputs <- make_clustered_fma_inputs()

  result <- run_fma(
    bma_data = inputs$bma_data,
    bma_model = inputs$bma_model,
    input_var_list = inputs$var_list,
    cluster_ids = inputs$study,
    print_results = "none"
  )

  # var_name_verbose equals var_name here, so the output rows reveal the
  # predictor order run_fma actually used.
  predictor_order <- setdiff(result$coefficients$variable, "Intercept")
  x_data <- cbind(1, as.matrix(inputs$bma_data[predictor_order]))
  scale_vector <- apply(abs(x_data), 2, max)
  x_scaled <- sweep(x_data, 2, scale_vector, "/")
  y <- inputs$bma_data$effect

  m <- ncol(x_scaled)
  beta <- matrix(0, nrow = m, ncol = m)
  var_matrix <- matrix(0, nrow = m, ncol = m)
  for (i in seq_len(m)) {
    x_i <- x_scaled[, seq_len(i), drop = FALSE]
    fit_i <- stats::lm(y ~ 0 + x_i)
    beta[seq_len(i), i] <- stats::coef(fit_i)
    var_matrix[seq_len(i), i] <- diag(
      sandwich::vcovCL(fit_i, cluster = inputs$study, type = "HC1")
    )
  }

  weights <- result$weights
  beta_avg <- beta %*% weights
  bias_sq <- (beta - as.numeric(beta_avg))^2
  expected_coef <- as.numeric(beta_avg / scale_vector)
  expected_se <- as.numeric((sqrt(var_matrix + bias_sq) %*% weights) / scale_vector)

  expect_equal(result$coefficients$coefficient, expected_coef, tolerance = 1e-6)
  expect_equal(result$coefficients$se, expected_se, tolerance = 1e-6)
})

test_that("run_fma validates cluster_ids", {
  skip_if_not_installed("BMS")
  skip_if_not_installed("quadprog")

  inputs <- make_clustered_fma_inputs()
  n <- nrow(inputs$bma_data)

  run_with_cluster <- function(cluster_ids) {
    run_fma(
      bma_data = inputs$bma_data,
      bma_model = inputs$bma_model,
      input_var_list = inputs$var_list,
      cluster_ids = cluster_ids,
      print_results = "none"
    )
  }

  expect_error(run_with_cluster(rep("a", n)), "at least two distinct clusters")
  expect_error(run_with_cluster(inputs$study[-1]))
  expect_error(run_with_cluster(c(NA, inputs$study[-1])))
})

test_that("resolve_fma_cluster_ids aligns study_id through na.omit row names", {
  df <- data.frame(
    effect = c(0.1, NA, 0.3, 0.4, 0.5, 0.6),
    se = c(0.05, 0.06, 0.07, NA, 0.09, 0.1),
    study_id = c("s1", "s1", "s2", "s2", "s3", "s3"),
    stringsAsFactors = FALSE
  )
  bma_data <- stats::na.omit(df[c("effect", "se")])

  ids <- resolve_fma_cluster_ids(df, bma_data)
  expect_equal(ids, c("s1", "s2", "s3", "s3"))
  expect_equal(length(ids), nrow(bma_data))
})

test_that("resolve_fma_cluster_ids downgrades to NULL on unusable clusters", {
  bma_data <- data.frame(effect = c(0.1, 0.2), se = c(0.05, 0.06))

  no_study <- data.frame(effect = c(0.1, 0.2), se = c(0.05, 0.06))
  expect_null(resolve_fma_cluster_ids(no_study, bma_data))

  single_cluster <- cbind(no_study, study_id = c("s1", "s1"))
  expect_null(resolve_fma_cluster_ids(single_cluster, bma_data))

  na_ids <- cbind(no_study, study_id = c("s1", NA))
  expect_null(resolve_fma_cluster_ids(na_ids, bma_data))

  # Rows that cannot be matched back to df resolve to NA and downgrade too.
  foreign_rows <- data.frame(effect = 0.1, se = 0.05, study_id = "s1", row.names = "99")
  expect_null(resolve_fma_cluster_ids(foreign_rows, bma_data))
})
