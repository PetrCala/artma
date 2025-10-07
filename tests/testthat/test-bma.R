box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_gt,
    expect_length,
    expect_named,
    expect_true,
    expect_type,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / libs / bma[
    get_bma_formula,
    handle_bma_params,
    run_vif_test,
    get_bma_data
  ],
  artma / methods / bma[bma]
)

make_demo_bma_data <- function() {
  set.seed(42)
  n_studies <- 10L
  per_study <- 5L
  study_ids <- rep(paste0("S", seq_len(n_studies)), each = per_study)

  data.frame(
    study_id = study_ids,
    effect = rnorm(n_studies * per_study, mean = 0.3, sd = 0.1),
    se = runif(n_studies * per_study, min = 0.05, max = 0.15),
    study_size = sample(20:80, n_studies * per_study, replace = TRUE),
    moderator1 = rnorm(n_studies * per_study, mean = 0, sd = 1),
    moderator2 = rbinom(n_studies * per_study, size = 1, prob = 0.5),
    moderator3 = rnorm(n_studies * per_study, mean = 5, sd = 2),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("get_bma_formula creates valid formula from variable vector", {
  df <- make_demo_bma_data()
  vars <- c("effect", "se", "moderator1", "moderator2")

  formula <- get_bma_formula(vars, df, get_var_vector_instead = FALSE)

  expect_true(inherits(formula, "formula"))
  expect_equal(all.vars(formula), c("effect", "se", "moderator1", "moderator2"))
})

test_that("get_bma_formula returns variable vector when requested", {
  df <- make_demo_bma_data()
  vars <- c("effect", "se", "moderator1", "moderator2")

  var_vector <- get_bma_formula(vars, df, get_var_vector_instead = TRUE)

  expect_type(var_vector, "character")
  expect_equal(var_vector, c("effect", "se", "moderator1", "moderator2"))
})

test_that("get_bma_formula removes constant variables", {
  df <- make_demo_bma_data()
  df$constant_var <- 1
  vars <- c("effect", "se", "moderator1", "constant_var")

  formula <- get_bma_formula(vars, df, get_var_vector_instead = FALSE)
  formula_vars <- all.vars(formula)

  expect_false("constant_var" %in% formula_vars)
  expect_true(all(c("effect", "se", "moderator1") %in% formula_vars))
})

test_that("handle_bma_params returns list of parameter lists", {
  params <- list(
    burn = 1000L,
    iter = 5000L,
    g = "UIP",
    mprior = "uniform"
  )

  result <- handle_bma_params(params)

  expect_type(result, "list")
  expect_length(result, 1)
  expect_equal(result[[1]]$burn, 1000L)
  expect_equal(result[[1]]$iter, 5000L)
})

test_that("handle_bma_params splits multiple model configurations", {
  params <- list(
    burn = c(1000L, 2000L),
    iter = 5000L,
    g = "UIP",
    mprior = c("uniform", "random")
  )

  result <- handle_bma_params(params)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(result[[1]]$burn, 1000L)
  expect_equal(result[[2]]$burn, 2000L)
  expect_equal(result[[1]]$mprior, "uniform")
  expect_equal(result[[2]]$mprior, "random")
})

test_that("run_vif_test calculates VIF coefficients from formula", {
  df <- make_demo_bma_data()
  vars <- c("effect", "se", "moderator1", "moderator2")

  local_options("artma.verbose" = 1)

  vif_coefs <- run_vif_test(vars, df, print_all_coefs = FALSE, verbose = FALSE)

  expect_type(vif_coefs, "double")
  expect_gt(length(vif_coefs), 0)
  expect_true(all(is.finite(vif_coefs)))
})

test_that("get_bma_data subsets and scales data correctly", {
  df <- make_demo_bma_data()
  var_list <- data.frame(
    var_name = c("effect", "se", "moderator1", "moderator2"),
    var_name_verbose = c("Effect", "SE", "Mod1", "Mod2"),
    bma = c(TRUE, TRUE, TRUE, TRUE),
    to_log_for_bma = c(FALSE, FALSE, FALSE, FALSE),
    bma_reference_var = c(FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  variable_info <- c("effect", "se", "moderator1", "moderator2")

  bma_data <- get_bma_data(
    df,
    var_list,
    variable_info,
    scale_data = TRUE,
    from_vector = TRUE,
    include_reference_groups = FALSE
  )

  expect_true(is.data.frame(bma_data))
  expect_equal(ncol(bma_data), 4)
  expect_equal(nrow(bma_data), nrow(df))
  expect_true("effect" %in% colnames(bma_data))
})

test_that("get_bma_data scales non-binary variables", {
  df <- make_demo_bma_data()
  var_list <- data.frame(
    var_name = c("effect", "se", "moderator1", "moderator2"),
    var_name_verbose = c("Effect", "SE", "Mod1", "Mod2"),
    bma = c(TRUE, TRUE, TRUE, TRUE),
    to_log_for_bma = c(FALSE, FALSE, FALSE, FALSE),
    bma_reference_var = c(FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  variable_info <- c("effect", "se", "moderator1", "moderator2")

  bma_data <- get_bma_data(
    df,
    var_list,
    variable_info,
    scale_data = TRUE,
    from_vector = TRUE,
    include_reference_groups = FALSE
  )

  expect_true(abs(mean(bma_data$moderator1)) < 1e-10)
  expect_true(abs(sd(bma_data$moderator1) - 1) < 1e-10)

  expect_false(abs(mean(bma_data$moderator2)) < 1e-10)
})

test_that("run_bma executes without errors", {
  box::use(artma / libs / bma[run_bma])

  df <- make_demo_bma_data()
  bma_data <- df[c("effect", "se", "moderator1", "moderator2")]

  params <- list(
    burn = 100L,
    iter = 500L,
    nmodel = 10L,
    g = "UIP",
    mprior = "uniform",
    mcmc = "bd"
  )

  local_options("artma.verbose" = 1)

  result <- run_bma(bma_data, params)

  expect_true(inherits(result, "bma"))
  expect_true(!is.null(result$topmod))
})
