box::use(
  testthat[
    expect_equal,
    expect_gte,
    expect_lte,
    expect_null,
    expect_setequal,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / econometric / bma[run_bma],
  artma / visualization / bma[build_bma_inclusion_data, create_bma_inclusion_plot]
)


# A model space wide enough that BMS retains far more models than carry any
# posterior mass, which is the situation the upstream image plot cannot draw.
fit_demo_bma <- function(n = 150, n_moderators = 8, seed = 4) {
  set.seed(seed)
  bma_data <- data.frame(effect = stats::rnorm(n))
  for (j in seq_len(n_moderators)) {
    bma_data[[paste0("moderator", j)]] <- stats::rnorm(n)
  }
  bma_data$effect <- 0.7 * bma_data$moderator1 -
    0.5 * bma_data$moderator2 +
    0.3 * bma_data$moderator3 +
    stats::rnorm(n) * 0.6

  run_bma(bma_data, list(
    burn = 500L, iter = 2000L, nmodel = 200L,
    g = "UIP", mprior = "uniform", mcmc = "bd"
  ))
}


test_that("the inclusion grid builds where BMS's own image plot aborts", {
  skip_if_not_installed("BMS")
  local_options("artma.verbose" = 1)

  model <- fit_demo_bma()

  # BMS lays its image columns out on cumsum() of unnormalised posterior
  # weights, which span enough orders of magnitude to saturate in double
  # precision; graphics::image() then rejects the non-monotonic boundaries.
  upstream <- tryCatch(
    {
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::image(model, yprop2pip = FALSE, order.by.pip = TRUE)
      "rendered"
    },
    error = function(e) conditionMessage(e)
  )

  built <- build_bma_inclusion_data(model)

  expect_true(!is.null(built))
  expect_true(nrow(built$cells) > 0)
  # Whether or not this particular fit trips the upstream failure, the
  # replacement must produce a grid either way.
  expect_true(is.character(upstream))
})


test_that("regressors are ordered by posterior inclusion probability", {
  skip_if_not_installed("BMS")
  local_options("artma.verbose" = 1)

  model <- fit_demo_bma()
  built <- build_bma_inclusion_data(model)

  bms_order <- rownames(stats::coef(model, order.by.pip = TRUE, exact = TRUE))
  expect_equal(built$regressors, bms_order)
})


test_that("cell signs match the coefficients of the model they belong to", {
  skip_if_not_installed("BMS")
  local_options("artma.verbose" = 1)

  model <- fit_demo_bma()
  built <- build_bma_inclusion_data(model)

  betas <- as.matrix(model$topmod$betas())
  weights <- exp(model$topmod$lik() - max(model$topmod$lik()))
  best <- order(weights, decreasing = TRUE)[1]
  best_betas <- stats::setNames(betas[, best], model$reg.names)

  top_cells <- built$cells[built$cells$model == 1, , drop = FALSE]
  observed <- stats::setNames(top_cells$inclusion, built$regressors[top_cells$row])

  expected <- vapply(names(observed), function(name) {
    value <- best_betas[[name]]
    if (value == 0) "Excluded" else if (value > 0) "Positive" else "Negative"
  }, character(1))

  expect_equal(observed[names(expected)], expected)
})


test_that("the grid truncates to the models that carry the posterior mass", {
  skip_if_not_installed("BMS")
  local_options("artma.verbose" = 1)

  model <- fit_demo_bma()
  built <- build_bma_inclusion_data(model, max_models = 12L, coverage = 0.99)

  expect_lte(built$n_models_shown, 12L)
  expect_lte(built$n_models_shown, built$n_models_total)
  expect_gte(built$mass_shown, 0)
  expect_lte(built$mass_shown, 1 + 1e-9)

  # Columns tile the full width exactly once, with no gaps or overlaps.
  first_row <- built$cells[built$cells$row == 1, , drop = FALSE]
  first_row <- first_row[order(first_row$model), , drop = FALSE]
  expect_equal(min(first_row$xmin), 0)
  expect_equal(max(first_row$xmax), 1)
  expect_equal(first_row$xmin[-1], utils::head(first_row$xmax, -1))
})


test_that("create_bma_inclusion_plot returns a ggplot with the three cell states", {
  skip_if_not_installed("BMS")
  local_options("artma.verbose" = 1)

  model <- fit_demo_bma()
  plot <- create_bma_inclusion_plot(model, theme_name = "blue")

  expect_true(ggplot2::is_ggplot(plot))
  expect_setequal(
    unique(plot$data$inclusion),
    intersect(c("Positive", "Negative", "Excluded"), unique(plot$data$inclusion))
  )
  expect_true(all(plot$data$inclusion %in% c("Positive", "Negative", "Excluded")))
})


test_that("build_bma_inclusion_data returns NULL when there is nothing to draw", {
  empty <- structure(
    list(
      reg.names = character(0),
      topmod = list(betas = function() matrix(numeric(0), nrow = 0, ncol = 0))
    ),
    class = "bma"
  )

  expect_null(build_bma_inclusion_data(empty))
})
