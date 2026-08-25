box::use(
  testthat[
    expect_no_warning,
    expect_true,
    expect_warning,
    test_that
  ]
)

box::use(
  artma / data_config / write[warn_on_implausible_mapping]
)

# Mappings that bypass auto-detection (config_set, a hand-edited options file,
# an external tool) must run through the same value checks recognition applies
# to everything it accepts. The mapping is still saved; the user is warned.

make_df <- function() {
  data.frame(
    study = rep(sprintf("Author%02d", 1:10), each = 4),
    coef_no = rep(1:4, times = 10),
    eis = round(stats::rnorm(40, 0.2, 0.4), 3),
    stderr = round(runif(40, 0.02, 0.4), 3),
    obs = rep(sample(50:900, 10, replace = TRUE), each = 4)
  )
}


test_that("a mapping contradicted by the column values warns", {
  withr::local_seed(4)
  withr::local_options(list("artma.verbose" = 3))
  df <- make_df()

  # coef_no is a per-study coefficient counter: whole numbers only.
  expect_warning(
    warn_on_implausible_mapping(df, list(effect = list(source_name = "coef_no"))),
    "coef_no"
  )
})


test_that("a plausible mapping passes without a warning", {
  withr::local_seed(4)
  withr::local_options(list("artma.verbose" = 3))
  df <- make_df()

  expect_no_warning(
    warn_on_implausible_mapping(df, list(effect = list(source_name = "eis")))
  )
  expect_no_warning(
    warn_on_implausible_mapping(df, list(se = list(source_name = "stderr")))
  )
})


test_that("changes the check cannot judge are left alone", {
  withr::local_seed(4)
  withr::local_options(list("artma.verbose" = 3))
  df <- make_df()

  # Not a rename, an unknown column, a non-standard record: nothing to check.
  expect_no_warning(warn_on_implausible_mapping(df, list(effect = list(bma = TRUE))))
  expect_no_warning(warn_on_implausible_mapping(df, list(effect = list(source_name = "gone"))))
  expect_no_warning(warn_on_implausible_mapping(df, list(gdp = list(source_name = "coef_no"))))
  expect_no_warning(warn_on_implausible_mapping(NULL, list(effect = list(source_name = "coef_no"))))
})


test_that("every standard role runs through the check", {
  withr::local_seed(4)
  withr::local_options(list("artma.verbose" = 3))
  df <- make_df()

  # A per-study counter is no more a sample size than it is an effect size.
  expect_warning(
    warn_on_implausible_mapping(df, list(n_obs = list(source_name = "coef_no"))),
    "n_obs"
  )
})
