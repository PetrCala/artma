box::use(
  testthat[
    expect_equal,
    expect_named,
    expect_true,
    skip_if_not_installed,
    test_that
  ]
)

box::use(
  artma / econometric / bma[get_bma_data, run_bma],
  artma / econometric / fma[run_fma]
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
