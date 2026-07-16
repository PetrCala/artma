box::use(
  testthat[
    expect_error,
    expect_true,
    skip_if_not_installed,
    test_that
  ],
  artma / calc / methods / maive[maive]
)

make_maive_data <- function(seed = 9, n = 40) {
  set.seed(seed)
  data.frame(
    bs = rnorm(n, 0.3, 0.1),
    sebs = runif(n, 0.05, 0.4),
    Ns = sample(50:400, n, replace = TRUE)
  )
}

test_that("maive dispatches to the MAIVE package and returns its contract", {
  skip_if_not_installed("MAIVE")
  dat <- make_maive_data()

  # Numeric method/weight args are coerced to integer before dispatch.
  res <- maive(dat, method = 1, weight = 0, instrument = 1, studylevel = 0, SE = 1, AR = 0)

  expect_true(is.list(res))
  expect_true(all(c("beta", "SE", "F-test") %in% names(res)))
  expect_true(is.numeric(res$beta) && is.finite(res$beta))
  expect_true(is.numeric(res$SE) && is.finite(res$SE))
})

test_that("maive aborts when required columns are missing", {
  skip_if_not_installed("MAIVE")
  expect_error(maive(data.frame(bs = 1:3, sebs = rep(0.1, 3)), SE = 1L), regexp = "Ns")
})

test_that("maive aborts when the MAIVE package is absent", {
  dat <- make_maive_data(n = 20)
  local_pretend_packages_absent("MAIVE")
  expect_error(maive(dat, SE = 1L), regexp = "MAIVE")
})
