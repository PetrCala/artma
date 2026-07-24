box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_identical,
    expect_match,
    expect_named,
    expect_null,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / econometric / bma[get_bma_data, run_bma],
  artma / methods / best_practice_estimate[resolve_bma_input_for_bpe],
  artma / methods / fma[fma]
)

# resolve_bma_input_for_bpe accepts either the standard method-result shape
# (fields nested under $meta) or a bare list carrying model/data/var_list
# directly. These fixtures avoid running BMS: resolve_bma_input_for_bpe only
# checks `inherits(model, "bma")`, it never calls a BMS S3 method itself.
make_fake_bma_model <- function() {
  structure(list(), class = "bma")
}

make_fake_bma_data <- function() {
  data.frame(effect = c(0.1, 0.2, 0.3), moderator1 = c(1, 0, 1))
}

test_that("resolve_bma_input_for_bpe accepts the $meta bundle shape", {
  model <- make_fake_bma_model()
  bma_data <- make_fake_bma_data()

  bma_result <- list(
    meta = list(
      model = model,
      data = bma_data,
      var_list = data.frame(var_name = c("effect", "moderator1")),
      params = list(g = "UIP")
    )
  )

  resolved <- resolve_bma_input_for_bpe(df = bma_data, bma_result = bma_result, run_bma_if_missing = TRUE)

  expect_identical(resolved$model, model)
  expect_identical(resolved$data, bma_data)
  expect_equal(resolved$source, "provided")
  expect_true(inherits(resolved$formula, "formula"))
})

test_that("resolve_bma_input_for_bpe accepts a bare model/data/var_list list", {
  model <- make_fake_bma_model()
  bma_data <- make_fake_bma_data()

  bma_result <- list(
    model = model,
    data = bma_data,
    var_list = data.frame(var_name = c("effect", "moderator1"))
  )

  resolved <- resolve_bma_input_for_bpe(df = bma_data, bma_result = bma_result, run_bma_if_missing = TRUE)

  expect_identical(resolved$model, model)
  expect_identical(resolved$data, bma_data)
  expect_equal(resolved$source, "provided")
})

test_that("resolve_bma_input_for_bpe aborts on a malformed list with run_bma_if_missing = FALSE", {
  bma_data <- make_fake_bma_data()

  expect_error(
    resolve_bma_input_for_bpe(df = bma_data, bma_result = list(unrelated = 1), run_bma_if_missing = FALSE),
    "Best-practice estimate requires a BMA result and run_bma_if_missing is FALSE"
  )
})

test_that("resolve_bma_input_for_bpe aborts on a non-list bma_result with run_bma_if_missing = FALSE", {
  bma_data <- make_fake_bma_data()

  expect_error(
    resolve_bma_input_for_bpe(df = bma_data, bma_result = "not-a-list", run_bma_if_missing = FALSE),
    "Best-practice estimate requires a BMA result and run_bma_if_missing is FALSE"
  )
})

# fma()'s local resolve_bma_result has the same "$meta bundle or bare list"
# contract. A malformed bma_result should fall back to computing BMA inputs
# from df, exactly as if no bma_result had been provided at all.
test_that("fma treats a malformed bma_result identically to no bma_result at all", {
  skip_if_not_installed("BMS")
  skip_if_not_installed("quadprog")

  n <- 20L
  df <- data.frame(
    effect = rnorm(n, mean = 0.2, sd = 0.1),
    se = runif(n, min = 0.05, max = 0.15),
    moderator1 = rnorm(n),
    stringsAsFactors = FALSE
  )
  config <- list(
    effect = list(var_name = "effect", var_name_verbose = "Effect", bma = FALSE),
    se = list(var_name = "se", var_name_verbose = "SE", bma = TRUE),
    moderator1 = list(var_name = "moderator1", var_name_verbose = "Moderator 1", bma = TRUE)
  )

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.data.columns = config,
    artma.methods.bma.burn = 100L,
    artma.methods.bma.iter = 500L,
    artma.methods.bma.nmodel = 10L,
    artma.methods.bma.g = "UIP",
    artma.methods.bma.mprior = "uniform",
    artma.methods.bma.mcmc = "bd"
  ))

  set.seed(99)
  result_no_bma_result <- fma(df, bma_result = NULL)
  set.seed(99)
  result_malformed <- fma(df, bma_result = "not-a-list")

  expect_equal(result_malformed$tables$coefficients, result_no_bma_result$tables$coefficients)
  expect_equal(result_malformed$meta$weights, result_no_bma_result$meta$weights)
})

test_that("fma reuses a provided BMA result in both the $meta bundle and bare-list shapes", {
  skip_if_not_installed("BMS")
  skip_if_not_installed("quadprog")

  set.seed(11)
  n <- 30L
  df <- data.frame(
    effect = rnorm(n, mean = 0.2, sd = 0.1),
    se = runif(n, min = 0.05, max = 0.15),
    moderator1 = rnorm(n),
    stringsAsFactors = FALSE
  )

  var_list <- data.frame(
    var_name = c("effect", "se", "moderator1"),
    var_name_verbose = c("Effect", "SE", "Moderator 1"),
    bma = c(TRUE, TRUE, TRUE),
    to_log_for_bma = c(FALSE, FALSE, FALSE),
    bma_reference_var = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  bma_data <- get_bma_data(
    df,
    var_list,
    variable_info = c("effect", "se", "moderator1"),
    scale_data = FALSE,
    from_vector = TRUE,
    include_reference_groups = FALSE
  )

  params <- list(burn = 100L, iter = 500L, nmodel = 10L, g = "UIP", mprior = "uniform", mcmc = "bd")
  model <- run_bma(bma_data, params)

  config <- list(
    effect = list(var_name = "effect", var_name_verbose = "Effect", bma = FALSE),
    se = list(var_name = "se", var_name_verbose = "SE", bma = TRUE),
    moderator1 = list(var_name = "moderator1", var_name_verbose = "Moderator 1", bma = TRUE)
  )

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.data.columns = config,
    artma.methods.bma.burn = 100L,
    artma.methods.bma.iter = 500L,
    artma.methods.bma.nmodel = 10L,
    artma.methods.bma.g = "UIP",
    artma.methods.bma.mprior = "uniform",
    artma.methods.bma.mcmc = "bd"
  ))

  meta_bundle <- list(meta = list(model = model, data = bma_data, var_list = var_list))
  bare_list <- list(model = model, data = bma_data, var_list = var_list)

  result_meta_bundle <- fma(df, bma_result = meta_bundle)
  result_bare_list <- fma(df, bma_result = bare_list)

  expect_identical(result_meta_bundle$meta$model, model)
  expect_identical(result_bare_list$meta$model, model)
  expect_named(result_meta_bundle$tables$coefficients, names(result_bare_list$tables$coefficients))
})

# BMS::bms() crashes with "subscript out of bounds" for a model space with
# exactly one candidate regressor; prepare_bma_inputs() now catches this
# before run_bma() is ever reached, and every consumer should surface the
# reason instead of propagating a NULL model.
make_single_moderator_df <- function() {
  set.seed(7)
  n <- 20L
  data.frame(
    effect = rnorm(n, mean = 0.2, sd = 0.1),
    se = runif(n, min = 0.05, max = 0.15),
    stringsAsFactors = FALSE
  )
}

test_that("fma skips gracefully instead of crashing when only one moderator is available", {
  df <- make_single_moderator_df()
  config <- list(
    effect = list(var_name = "effect", var_name_verbose = "Effect", bma = FALSE),
    se = list(var_name = "se", var_name_verbose = "SE", bma = TRUE)
  )

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.data.columns = config
  ))

  result <- fma(df, bma_result = NULL)

  expect_equal(nrow(result$tables$coefficients), 0)
  expect_null(result$meta$model)
  expect_match(result$meta$skipped, "at least 2 candidate moderator variables")
})

test_that("resolve_bma_input_for_bpe surfaces the BMA skip reason when BMA cannot run", {
  df <- make_single_moderator_df()
  config <- list(
    effect = list(var_name = "effect", var_name_verbose = "Effect", bma = FALSE),
    se = list(var_name = "se", var_name_verbose = "SE", bma = TRUE)
  )

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.data.columns = config
  ))

  expect_error(
    resolve_bma_input_for_bpe(df = df, bma_result = NULL, run_bma_if_missing = TRUE),
    "BMA was skipped"
  )
})
