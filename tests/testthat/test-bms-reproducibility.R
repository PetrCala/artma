box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    skip_if_not_installed,
    test_that
  ]
)

# BMS::bms (0.3.5) reseeds the session RNG from the wall clock right before
# its sampling loop, which made every BMA/FMA fit non-reproducible no matter
# what seed the caller set. run_bma() therefore executes a patched copy of the
# function with that reseed stripped; these tests pin down both the surgery
# and the reproducibility it restores.

test_that("strip_bms_time_reseed removes exactly the wall-clock reseed call", {
  box::use(artma / econometric / bma[strip_bms_time_reseed])

  fn <- function(n) {
    set.seed(as.numeric(Sys.time()))
    stats::runif(n)
  }
  patched <- strip_bms_time_reseed(fn)

  body_text <- paste(deparse(body(patched)), collapse = "\n")
  expect_false(grepl("Sys.time", body_text, fixed = TRUE))

  # With the reseed gone, the function obeys the caller's seed.
  set.seed(5)
  expected <- stats::runif(3)
  set.seed(5)
  expect_identical(patched(3), expected)
})

test_that("strip_bms_time_reseed leaves a function without the pattern unchanged", {
  box::use(artma / econometric / bma[strip_bms_time_reseed])

  fn <- function(x) x + 1
  expect_identical(strip_bms_time_reseed(fn), fn)
})

test_that("seeded run_bma fits are reproducible and respond to the seed", {
  skip_if_not_installed("BMS")
  box::use(artma / econometric / bma[run_bma])

  set.seed(99)
  n <- 120
  bma_data <- data.frame(
    effect = stats::rnorm(n),
    mod1 = stats::rnorm(n),
    mod2 = stats::rnorm(n),
    mod3 = stats::rnorm(n),
    mod4 = stats::rnorm(n),
    mod5 = stats::rnorm(n),
    mod6 = stats::rnorm(n)
  )
  params <- list(
    burn = 200L, iter = 1000L, g = "UIP", mprior = "uniform",
    nmodel = 50L, mcmc = "bd"
  )

  fit <- function(seed) {
    set.seed(seed)
    model <- run_bma(bma_data, params)
    # The visited-model set, visit frequencies, and exact coefficients pin the
    # chain down; timing fields and the call record are left out on purpose.
    list(
      bool = model$topmod$bool(),
      lik = model$topmod$lik(),
      ncount = model$topmod$ncount(),
      coefs = stats::coef(model, order.by.pip = FALSE, exact = TRUE, include.constant = TRUE)
    )
  }

  first <- fit(123)
  second <- fit(123)
  expect_equal(first, second)

  # A different seed must produce a different chain (visit frequencies), or
  # the seed is not actually reaching the sampler.
  other <- fit(321)
  expect_false(identical(first$ncount, other$ncount))
})
