box::use(
  testthat[
    expect_equal, expect_false, expect_null, expect_s3_class, expect_true,
    skip, skip_if_not_installed, test_that
  ],
  withr[local_options]
)

# RoBMA fits through JAGS, a system library. rjags only loads when JAGS is
# present, which makes it a reliable probe for a machine that can actually
# fit -- but load the namespace in a subprocess, not here: on some platforms
# (observed on macOS oldrel CI) rjags segfaults on dyn.load when its bundled
# JAGS version doesn't match the system library, which would otherwise crash
# the whole test process instead of just failing this one probe.
skip_if_no_jags <- function() {
  skip_if_not_installed("RoBMA")
  rscript <- file.path(R.home("bin"), "Rscript")
  probe <- tryCatch(
    system2(
      rscript,
      c("-e", shQuote("quit(status = !requireNamespace('rjags', quietly = TRUE))")),
      stdout = FALSE, stderr = FALSE
    ),
    error = function(e) 1L,
    warning = function(w) 1L
  )
  if (!identical(probe, 0L)) {
    skip("JAGS is not available")
  }
}

box::use(
  artma / methods / robma[robma]
)

make_demo_robma_data <- function(n_studies = 12L) {
  set.seed(1234)
  data.frame(
    study_id = paste0("S", seq_len(n_studies)),
    effect = stats::rnorm(n_studies, mean = 0.3, sd = 0.1),
    se = stats::runif(n_studies, min = 0.05, max = 0.15),
    stringsAsFactors = FALSE
  )
}

test_that("robma skips when too few usable observations remain", {
  df <- data.frame(
    effect = c(0.2, NA_real_, 0.4),
    se = c(0.1, 0.1, 0)
  )

  local_options(list(artma.verbose = 0))

  result <- robma(df)

  expect_equal(result$meta$n_obs, 1L)
  expect_null(result$meta$model)
  expect_true(nzchar(result$meta$skipped))
  expect_s3_class(result$tables$summary, "data.frame")
  expect_equal(nrow(result$tables$summary), 0L)
})

test_that("robma fits the ensemble and returns estimates and components", {
  skip_if_no_jags()

  df <- make_demo_robma_data()

  local_options(list(
    artma.verbose = 0,
    artma.autonomy.level = "autonomous",
    artma.output.save_results = FALSE,
    artma.methods.robma.chains = 1L,
    artma.methods.robma.samples = 200L,
    artma.methods.robma.burnin = 100L,
    artma.methods.robma.adapt = 100L,
    artma.methods.robma.autofit = FALSE,
    artma.methods.robma.seed = 42L
  ))

  # The tiny sample counts that keep this test fast also trip RoBMA's
  # convergence checks, which is expected here.
  result <- suppressWarnings(robma(df))

  expect_false(isTRUE(nzchar(result$meta$skipped)))
  expect_equal(result$meta$n_obs, nrow(df))
  expect_true(!is.null(result$meta$model))

  for (key in c("summary", "components", "models")) {
    expect_s3_class(result$tables[[key]], "data.frame")
    expect_true(nrow(result$tables[[key]]) > 0L)
  }

  expect_equal(names(result$tables$summary)[1], "parameter")
  expect_equal(names(result$tables$components)[1], "component")
  expect_equal(names(result$tables$models)[1], "model")
})

test_that("robma is registered as an opt-in method", {
  box::use(
    artma / methods / robma[run],
    artma / modules / runtime_methods[get_method_metadata]
  )

  meta <- get_method_metadata(run, name = "robma")

  expect_true(meta$opt_in)
  expect_equal(meta$suggests, "RoBMA")
  expect_equal(meta$required_columns, c("effect", "se"))
})
