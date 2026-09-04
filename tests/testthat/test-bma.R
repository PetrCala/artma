box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_length,
    expect_match,
    expect_message,
    expect_named,
    expect_null,
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
    get_bma_data,
    find_optimal_bma_formula,
    rename_bma_model,
    run_bma,
    unscale_bma_coefs
  ],
  artma / methods / bma[bma, prepare_bma_inputs],
  artma / options / template[get_option_defs]
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

test_that("handle_bma_params treats uniformly multi-valued parameters as multiple models", {
  params <- list(
    burn = c(100L, 200L),
    iter = c(500L, 1000L),
    g = c("UIP", "BRIC")
  )

  result <- handle_bma_params(params)

  expect_length(result, 2)
  expect_equal(result[[1]]$burn, 100L)
  expect_equal(result[[1]]$g, "UIP")
  expect_equal(result[[2]]$burn, 200L)
  expect_equal(result[[2]]$iter, 1000L)
  expect_equal(result[[2]]$g, "BRIC")
})

test_that("handle_bma_params aborts on mixed multi-value lengths", {
  params <- list(
    burn = c(100L, 200L),
    mprior = c("uniform", "random", "fixed"),
    g = "UIP"
  )

  expect_error(handle_bma_params(params), "1 or n values")
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

test_that("unscale_bma_coefs inverts the z-scoring of slopes and intercept", {
  bma_data <- data.frame(effect = 1:4, x_cont = c(1, 3, 5, 9), x_bin = c(0, 1, 0, 1))
  attr(bma_data, "bpe_scale_centers") <- c(effect = 2.5, x_cont = 4.5, x_bin = 0)
  attr(bma_data, "bpe_scale_scales") <- c(effect = 2, x_cont = 4, x_bin = 1)

  coefs <- matrix(
    c(
      0.9, 0.5, 0.1, 1, 1,
      0.8, -0.25, 0.05, 0, 2,
      1, 0.3, NA, NA, 0
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(
      c("x_cont", "x_bin", "(Intercept)"),
      c("PIP", "Post Mean", "Post SD", "Cond.Pos.Sign", "Idx")
    )
  )

  unscaled <- unscale_bma_coefs(coefs, bma_data, c("x_cont", "x_bin"))

  # Slopes: b * sd(y) / sd(x); the dummy only picks up sd(y).
  expect_equal(unscaled["x_cont", "Post Mean"], 0.5 * 2 / 4)
  expect_equal(unscaled["x_cont", "Post SD"], 0.1 * 2 / 4)
  expect_equal(unscaled["x_bin", "Post Mean"], -0.25 * 2)
  expect_equal(unscaled["x_bin", "Post SD"], 0.05 * 2)
  # Intercept: mean(y) + sd(y) * a - sum(raw slope * mean(x)).
  expect_equal(unscaled["(Intercept)", "Post Mean"], 2.5 + 2 * 0.3 - 0.25 * 4.5)
  expect_true(is.na(unscaled["(Intercept)", "Post SD"]))
  # PIPs and the bookkeeping columns are untouched.
  expect_equal(unscaled[, c("PIP", "Cond.Pos.Sign", "Idx")], coefs[, c("PIP", "Cond.Pos.Sign", "Idx")])

  # A frame that was never scaled is passed through unchanged.
  expect_equal(unscale_bma_coefs(coefs, data.frame(effect = 1:4), c("x_cont", "x_bin")), coefs)
})

test_that("unscaled BMA coefficients match BMS fitted on the raw data", {
  skip_if_not_installed("BMS")
  local_options("artma.verbose" = 1)

  df <- make_demo_bma_data()
  vars <- c("effect", "se", "moderator1", "moderator2", "moderator3")
  var_list <- data.frame(
    var_name = vars,
    var_name_verbose = vars,
    bma = TRUE,
    to_log_for_bma = FALSE,
    bma_reference_var = FALSE,
    stringsAsFactors = FALSE
  )
  scaled <- get_bma_data(df, var_list, vars, scale_data = TRUE, from_vector = TRUE, include_reference_groups = FALSE)
  raw <- get_bma_data(df, var_list, vars, scale_data = FALSE, from_vector = TRUE, include_reference_groups = FALSE)

  # Enumerating the model space removes sampling noise, so the g-prior's scale
  # invariance makes the two fits agree to numerical precision.
  params <- list(burn = 10L, iter = 100L, nmodel = 16L, g = "UIP", mprior = "uniform", mcmc = "enumerate")
  coef_args <- list(order.by.pip = FALSE, exact = TRUE, include.constant = TRUE)

  scaled_model <- run_bma(scaled, params)
  raw_model <- run_bma(raw, params)
  scaled_coefs <- do.call(stats::coef, c(list(scaled_model), coef_args))
  raw_coefs <- do.call(stats::coef, c(list(raw_model), coef_args))

  unscaled <- unscale_bma_coefs(scaled_coefs, scaled, scaled_model$reg.names)

  expect_equal(rownames(unscaled), rownames(raw_coefs))
  expect_equal(unscaled[, "PIP"], raw_coefs[, "PIP"], tolerance = 1e-8)
  expect_equal(unscaled[, "Post Mean"], raw_coefs[, "Post Mean"], tolerance = 1e-8)
  slopes <- setdiff(rownames(unscaled), "(Intercept)")
  expect_equal(unscaled[slopes, "Post SD"], raw_coefs[slopes, "Post SD"], tolerance = 1e-8)
  # The test is only meaningful if the scaled fit differed from the raw one.
  expect_false(isTRUE(all.equal(scaled_coefs[slopes, "Post Mean"], raw_coefs[slopes, "Post Mean"])))
})

test_that("bma reports posterior means on the data scale", {
  skip_if_not_installed("BMS")

  df <- make_demo_bma_data()
  config <- list(
    effect = list(var_name = "effect", var_name_verbose = "Effect", bma = FALSE),
    se = list(var_name = "se", var_name_verbose = "SE", bma = TRUE),
    moderator1 = list(var_name = "moderator1", var_name_verbose = "Mod1", bma = TRUE),
    moderator3 = list(var_name = "moderator3", var_name_verbose = "Mod3", bma = TRUE)
  )

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.data.columns = config,
    artma.output.save_results = FALSE,
    artma.visualization.export_graphics = FALSE,
    artma.methods.bma.burn = 10L,
    artma.methods.bma.iter = 100L,
    artma.methods.bma.nmodel = 8L,
    artma.methods.bma.g = "UIP",
    artma.methods.bma.mprior = "uniform",
    artma.methods.bma.mcmc = "enumerate"
  ))

  result <- bma(df)

  raw <- df[c("effect", "se", "moderator1", "moderator3")]
  raw_model <- run_bma(raw, list(
    burn = 10L, iter = 100L, nmodel = 8L, g = "UIP", mprior = "uniform", mcmc = "enumerate"
  ))
  raw_coefs <- stats::coef(raw_model, order.by.pip = FALSE, exact = TRUE, include.constant = TRUE)

  # The model still ran on the z-scored frame ...
  expect_true(abs(mean(result$meta$data$effect)) < 1e-10)
  # ... but the reported numbers are on the data scale, intercept included.
  expected <- stats::setNames(raw_coefs[, "Post Mean"], c("SE", "Mod1", "Mod3", "Intercept"))
  reported <- stats::setNames(result$estimates$estimate, result$estimates$term)
  expect_equal(reported[names(expected)], expected, tolerance = 1e-8)
  expect_equal(result$tables$coefficients$post_mean, result$estimates$estimate)
  expect_equal(
    result$estimates$std_error[result$estimates$term != "Intercept"],
    unname(raw_coefs[rownames(raw_coefs) != "(Intercept)", "Post SD"]),
    tolerance = 1e-8
  )
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

test_that("run_bma accepts the documented dilut model prior", {
  skip_if_not_installed("BMS")

  df <- make_demo_bma_data()
  bma_data <- df[c("effect", "se", "moderator1", "moderator2")]
  params <- list(
    burn = 10L,
    iter = 100L,
    nmodel = 8L,
    g = "UIP",
    mprior = "dilut",
    mcmc = "enumerate"
  )

  local_options("artma.verbose" = 1)

  dilut_model <- run_bma(bma_data, params)
  uniform_model <- run_bma(bma_data, utils::modifyList(params, list(mprior = "uniform")))

  expect_true(inherits(dilut_model, "bma"))
  expect_equal(dilut_model$mprior.info$origargs$mpmode, "dilut")
  # The dilution prior downweights collinear models, so the posterior
  # inclusion probabilities move away from the uniform ones.
  dilut_pip <- stats::coef(dilut_model, order.by.pip = FALSE)[, "PIP"]
  uniform_pip <- stats::coef(uniform_model, order.by.pip = FALSE)[, "PIP"]
  expect_false(isTRUE(all.equal(dilut_pip, uniform_pip)))
})

test_that("the mprior help lists every model prior artma passes to bms", {
  def <- get_option_defs(opt_path = "methods.bma.mprior")[[1]]

  for (prior in c("uniform", "random", "fixed", "dilut")) {
    expect_match(def$help, prior, fixed = TRUE)
  }
})

test_that("run_bma leaves the graphics device untouched", {
  # `BMS::bms` defaults to `user.int = TRUE`, which plots the fitted model on
  # the way out. In a forked method worker on macOS that plot opens quartz and
  # kills the child, so `run_bma()` must never let it draw.
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

  # Other tests in this file leave devices open, so start from a clean slate:
  # otherwise a device `bms` opens is indistinguishable from one already there.
  while (!is.null(grDevices::dev.list())) grDevices::dev.off()

  run_bma(bma_data, params)

  expect_null(grDevices::dev.list())
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

test_that("prepare_bma_inputs skips gracefully when config selects a single moderator variable", {
  # BMS::bms() crashes with "subscript out of bounds" for a model space with
  # exactly one candidate regressor; a single selected moderator must be
  # caught here instead of reaching run_bma().
  df <- make_demo_bma_data()
  config <- list(
    effect = list(var_name = "effect", var_name_verbose = "Effect", bma = FALSE),
    se = list(var_name = "se", var_name_verbose = "SE", bma = TRUE)
  )

  prepared <- prepare_bma_inputs(
    df = df,
    config = config,
    use_vif_optimization = FALSE,
    max_groups_to_remove = 30L,
    scale_data = TRUE,
    verbosity = 0
  )

  expect_null(prepared$bma_data)
  expect_match(prepared$skipped, "at least 2 candidate moderator variables")
  expect_match(prepared$skipped, "se")
})

test_that("bma skips with an explanatory reason instead of crashing on a single moderator", {
  df <- make_demo_bma_data()
  config <- list(
    effect = list(var_name = "effect", var_name_verbose = "Effect", bma = FALSE),
    se = list(var_name = "se", var_name_verbose = "SE", bma = TRUE)
  )

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.data.columns = config,
    artma.output.save_results = FALSE
  ))

  result <- bma(df)

  expect_equal(nrow(result$tables$coefficients), 0)
  expect_true(is.null(result$meta$model))
  expect_match(result$meta$skip_reason, "at least 2 candidate moderator variables")
})

test_that("prepare_bma_inputs excludes constant moderators before scaling", {
  local_options(list("artma.verbose" = 1))

  df <- make_demo_bma_data()
  # A constant column (the shape an over-imputed, mostly-missing column takes)
  # would scale to all-NaN and let na.omit drop every observation.
  df$const_mod <- 1
  config <- list(
    se = list(var_name = "se", bma = TRUE),
    moderator1 = list(var_name = "moderator1", bma = TRUE),
    const_mod = list(var_name = "const_mod", bma = TRUE)
  )

  prepared <- prepare_bma_inputs(
    df, config,
    use_vif_optimization = FALSE,
    max_groups_to_remove = 3,
    verbosity = 1
  )

  expect_true(is.null(prepared$skipped))
  expect_false("const_mod" %in% colnames(prepared$bma_data))
  expect_true(all(c("effect", "se", "moderator1") %in% colnames(prepared$bma_data)))
  expect_true(nrow(prepared$bma_data) > 0)
})

make_collinear_pair_data <- function() {
  set.seed(7)
  n <- 100L
  x1 <- rnorm(n)
  data.frame(
    effect = rnorm(n),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.01),
    y1 = rnorm(n),
    y2 = rnorm(n),
    stringsAsFactors = FALSE
  )
}

make_ungrouped_var_list <- function(var_names) {
  data.frame(
    var_name = var_names,
    bma = TRUE,
    group_category = "other",
    stringsAsFactors = FALSE
  )
}

test_that("find_optimal_bma_formula aligns the constancy mask with var-list rows", {
  skip_if_not_installed("car")

  set.seed(11)
  n <- 60L
  # Data columns are deliberately ordered differently from the var-list rows,
  # with the constant column first, so a mask computed in data order would
  # pair with the wrong rows.
  df <- data.frame(
    const_var = 1,
    effect = rnorm(n),
    mod2 = rnorm(n),
    mod1 = rnorm(n),
    stringsAsFactors = FALSE
  )
  var_list <- make_ungrouped_var_list(c("effect", "mod1", "mod2", "const_var"))

  result <- find_optimal_bma_formula(
    df,
    var_list,
    return_variable_vector_instead = TRUE,
    verbose = FALSE
  )

  expect_equal(result$result, c("mod1", "mod2"))
  expect_equal(result$removed_groups, 0)
})

test_that("find_optimal_bma_formula removes 'other' variables one at a time", {
  skip_if_not_installed("car")

  df <- make_collinear_pair_data()
  var_list <- make_ungrouped_var_list(c("effect", "x1", "x2", "y1", "y2"))

  result <- find_optimal_bma_formula(
    df,
    var_list,
    return_variable_vector_instead = TRUE,
    verbose = FALSE
  )

  # Only one member of the collinear pair is dropped; the unrelated
  # moderators sharing the "other" category survive.
  expect_length(result$result, 3)
  expect_true(all(c("y1", "y2") %in% result$result))
  expect_length(result$removed_groups_verbose, 1)
})

test_that("find_optimal_bma_formula still removes genuine dummy groups wholesale", {
  skip_if_not_installed("car")

  set.seed(13)
  n <- 120L
  # Near dummy trap: the three region dummies cover all but one observation,
  # so their VIFs blow up without the model becoming aliased.
  category <- rep(c("a", "b", "c", "base"), times = c(55L, 56L, 8L, 1L))
  df <- data.frame(
    effect = rnorm(n),
    reg_a = as.integer(category == "a"),
    reg_b = as.integer(category == "b"),
    reg_c = as.integer(category == "c"),
    y1 = rnorm(n),
    y2 = rnorm(n),
    stringsAsFactors = FALSE
  )
  var_list <- data.frame(
    var_name = c("effect", "reg_a", "reg_b", "reg_c", "y1", "y2"),
    bma = TRUE,
    group_category = c("other", "region", "region", "region", "other", "other"),
    stringsAsFactors = FALSE
  )

  result <- find_optimal_bma_formula(
    df,
    var_list,
    return_variable_vector_instead = TRUE,
    verbose = FALSE
  )

  expect_equal(result$result, c("y1", "y2"))
  expect_equal(result$removed_groups, 1)
  expect_equal(sort(result$removed_groups_verbose), c("reg_a", "reg_b", "reg_c"))
})

test_that("find_optimal_bma_formula succeeds when the last allowed removal fixes VIF", {
  skip_if_not_installed("car")

  df <- make_collinear_pair_data()
  var_list <- make_ungrouped_var_list(c("effect", "x1", "x2", "y1", "y2"))

  result <- find_optimal_bma_formula(
    df,
    var_list,
    max_groups_to_remove = 1,
    return_variable_vector_instead = TRUE,
    verbose = FALSE
  )

  expect_equal(result$removed_groups, 1)
  expect_true(all(c("y1", "y2") %in% result$result))
})

test_that("find_optimal_bma_formula aborts only when removals are exhausted and VIFs remain high", {
  skip_if_not_installed("car")

  set.seed(17)
  n <- 100L
  x1 <- rnorm(n)
  z1 <- rnorm(n)
  df <- data.frame(
    effect = rnorm(n),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.01),
    z1 = z1,
    z2 = z1 + rnorm(n, sd = 0.01),
    y1 = rnorm(n),
    stringsAsFactors = FALSE
  )
  var_list <- make_ungrouped_var_list(c("effect", "x1", "x2", "z1", "z2", "y1"))

  expect_error(
    find_optimal_bma_formula(
      df,
      var_list,
      max_groups_to_remove = 1,
      return_variable_vector_instead = TRUE,
      verbose = FALSE
    ),
    "Optimal BMA formula not found"
  )
})

test_that("rename_bma_model leaves unmatched regressor names untouched", {
  model <- structure(
    list(reg.names = c("mod1", "not_in_list")),
    class = "bma"
  )
  var_list <- data.frame(
    var_name = c("effect", "mod1"),
    var_name_verbose = c("Effect", "Moderator 1"),
    stringsAsFactors = FALSE
  )

  renamed <- rename_bma_model(model, var_list)

  expect_equal(renamed$reg.names, c("Moderator 1", "not_in_list"))
})

test_that("prepare_bma_inputs drops non-numeric moderators instead of dying inside scale()", {
  local_options(list("artma.verbose" = 1))

  # A character moderator used to reach `scale()`, which aborts with a bare
  # "'x' must be numeric" naming no column.
  df <- make_demo_bma_data()
  df$reg_num <- sample(c("a", "b", "c"), nrow(df), replace = TRUE)
  config <- list(
    moderator1 = list(var_name = "moderator1", bma = TRUE),
    moderator2 = list(var_name = "moderator2", bma = TRUE),
    reg_num = list(var_name = "reg_num", bma = TRUE)
  )

  expect_message(
    prepared <- prepare_bma_inputs(
      df, config,
      use_vif_optimization = FALSE,
      max_groups_to_remove = 3,
      verbosity = 2
    ),
    "reg_num"
  )

  expect_null(prepared$skipped)
  expect_false("reg_num" %in% colnames(prepared$bma_data))
  expect_true(all(vapply(prepared$bma_data, is.numeric, logical(1))))
})

test_that("prepare_bma_inputs keeps logical moderators, which scale() handles", {
  local_options(list("artma.verbose" = 1))

  df <- make_demo_bma_data()
  df$flag_mod <- rep(c(TRUE, FALSE), length.out = nrow(df))
  config <- list(
    moderator1 = list(var_name = "moderator1", bma = TRUE),
    flag_mod = list(var_name = "flag_mod", bma = TRUE)
  )

  prepared <- prepare_bma_inputs(
    df, config,
    use_vif_optimization = FALSE,
    max_groups_to_remove = 3,
    verbosity = 0
  )

  expect_true("flag_mod" %in% colnames(prepared$bma_data))
})

test_that("prepare_bma_inputs warns about the excluded constant moderator", {
  local_options(list("artma.verbose" = 1))

  df <- make_demo_bma_data()
  df$const_mod <- 1
  config <- list(
    moderator1 = list(var_name = "moderator1", bma = TRUE),
    const_mod = list(var_name = "const_mod", bma = TRUE)
  )

  expect_message(
    prepare_bma_inputs(
      df, config,
      use_vif_optimization = FALSE,
      max_groups_to_remove = 3,
      verbosity = 2
    ),
    "constant"
  )
})
