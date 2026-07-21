box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_length,
    expect_named,
    expect_true,
    expect_type,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options, local_tempdir]
)

box::use(
  artma / econometric / bma[
    get_bma_formula,
    handle_bma_params,
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

test_that("get_bma_data subsets and scales non-binary variables", {
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

  # Structure: the requested columns are subset out, one row per observation.
  expect_true(is.data.frame(bma_data))
  expect_equal(ncol(bma_data), 4)
  expect_equal(nrow(bma_data), nrow(df))
  expect_true("effect" %in% colnames(bma_data))

  # Scaling: the continuous moderator is centred and unit-scaled; the binary
  # moderator is left untouched.
  expect_true(abs(mean(bma_data$moderator1)) < 1e-10)
  expect_true(abs(sd(bma_data$moderator1) - 1) < 1e-10)
  expect_false(abs(mean(bma_data$moderator2)) < 1e-10)
})

test_that("run_bma executes without errors", {
  box::use(artma / econometric / bma[run_bma])

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

test_that("build_bma_model_labels labels only the varying parameters", {
  box::use(artma / econometric / bma[build_bma_model_labels])

  params_list <- list(
    list(burn = 100L, iter = 500L, g = "UIP", mprior = "uniform"),
    list(burn = 100L, iter = 500L, g = "BRIC", mprior = "uniform")
  )

  labels <- build_bma_model_labels(params_list)

  expect_equal(labels, c("g=UIP", "g=BRIC"))
})

test_that("build_bma_model_labels falls back to generic names when nothing varies", {
  box::use(artma / econometric / bma[build_bma_model_labels])

  params_list <- list(
    list(burn = 100L, g = "UIP"),
    list(burn = 100L, g = "UIP")
  )

  labels <- build_bma_model_labels(params_list)

  expect_equal(labels, c("Model 1", "Model 2"))
})

test_that("build_bma_model_labels returns a single label for one model", {
  box::use(artma / econometric / bma[build_bma_model_labels])

  labels <- build_bma_model_labels(list(list(burn = 100L, g = "UIP")))

  expect_equal(labels, "Model 1")
})

test_that("render_bma_comparison_plot exports a comparison png for multiple models", {
  skip_if_not_installed("BMS")
  box::use(
    artma / econometric / bma[run_bma, render_bma_comparison_plot]
  )

  df <- make_demo_bma_data()
  bma_data <- df[c("effect", "se", "moderator1", "moderator2")]

  params <- list(burn = 100L, iter = 500L, nmodel = 10L, g = "UIP", mprior = "uniform", mcmc = "bd")

  local_options("artma.verbose" = 1)

  model_1 <- run_bma(bma_data, params)
  model_2 <- run_bma(bma_data, utils::modifyList(params, list(mprior = "random")))

  export_dir <- local_tempdir()

  render_bma_comparison_plot(
    list(`mprior=uniform` = model_1, `mprior=random` = model_2),
    export_graphics = TRUE,
    export_path = export_dir
  )

  expect_true(file.exists(file.path(export_dir, "bma_comparison.png")))
})

test_that("bma exports a comparison plot when multiple parameter sets are run", {
  skip_if_not_installed("BMS")

  df <- make_demo_bma_data()
  config <- list(
    effect = list(var_name = "effect", var_name_verbose = "Effect", bma = FALSE),
    se = list(var_name = "se", var_name_verbose = "SE", bma = TRUE),
    moderator1 = list(var_name = "moderator1", var_name_verbose = "Mod1", bma = TRUE),
    moderator2 = list(var_name = "moderator2", var_name_verbose = "Mod2", bma = TRUE)
  )

  export_dir <- local_tempdir()

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.data.columns = config,
    artma.output.save_results = FALSE,
    artma.visualization.export_graphics = TRUE,
    artma.visualization.export_path = export_dir,
    artma.methods.bma.burn = 100L,
    artma.methods.bma.iter = 500L,
    artma.methods.bma.nmodel = 10L,
    artma.methods.bma.g = "UIP",
    artma.methods.bma.mprior = c("uniform", "random"),
    artma.methods.bma.mcmc = "bd"
  ))

  result <- bma(df)

  expect_length(result$meta$all, 2)
  expect_true(file.exists(file.path(export_dir, "bma_comparison.png")))
})

test_that("bma does not export a comparison plot for a single parameter set", {
  skip_if_not_installed("BMS")

  df <- make_demo_bma_data()
  config <- list(
    effect = list(var_name = "effect", var_name_verbose = "Effect", bma = FALSE),
    se = list(var_name = "se", var_name_verbose = "SE", bma = TRUE),
    moderator1 = list(var_name = "moderator1", var_name_verbose = "Mod1", bma = TRUE),
    moderator2 = list(var_name = "moderator2", var_name_verbose = "Mod2", bma = TRUE)
  )

  export_dir <- local_tempdir()

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.data.columns = config,
    artma.output.save_results = FALSE,
    artma.visualization.export_graphics = TRUE,
    artma.visualization.export_path = export_dir,
    artma.methods.bma.burn = 100L,
    artma.methods.bma.iter = 500L,
    artma.methods.bma.nmodel = 10L,
    artma.methods.bma.g = "UIP",
    artma.methods.bma.mprior = "uniform",
    artma.methods.bma.mcmc = "bd"
  ))

  result <- bma(df)

  expect_length(result$meta$all, 1)
  expect_false(file.exists(file.path(export_dir, "bma_comparison.png")))
})
