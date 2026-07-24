box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_true,
    test_that
  ],
  withr[local_options]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_error <- getFromNamespace("expect_error", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")

# Required columns are study_id, effect, se, n_obs (CONST$DATA$REQUIRED_COLNAMES).
make_df <- function(study_id = c("a", "b", "c")) {
  data.frame(
    study_id = study_id,
    effect = c(0.1, 0.2, 0.3),
    se = c(0.01, 0.02, 0.03),
    n_obs = c(10L, 20L, 30L),
    stringsAsFactors = FALSE
  )
}

# -- handle_missing_values: non-numeric required columns -----------------------

test_that("'remove' drops rows with NA in a non-numeric required column", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "remove",
    "artma.verbose" = 1
  ))

  df <- make_df(study_id = c("a", NA, "b"))
  result <- handle_missing_values(df)

  expect_equal(nrow(result), 2)
  expect_true(!anyNA(result$study_id))
})

test_that("'remove' drops non-numeric required NAs even when no other column has NAs", {
  box::use(artma / data / na_handling[handle_missing_values])

  # Guards the needs_processing fix: with NAs only in study_id, the remove
  # path must still run instead of returning the data frame untouched.
  local_options(list(
    "artma.data.na_handling" = "remove",
    "artma.verbose" = 1
  ))

  df <- make_df(study_id = c(NA, "a", "b"))
  result <- handle_missing_values(df)

  expect_equal(nrow(result), 2)
})

test_that("'stop' aborts on NAs in a non-numeric required column", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "stop",
    "artma.verbose" = 1
  ))

  df <- make_df(study_id = c("a", NA, "b"))
  expect_error(handle_missing_values(df), "non-numeric required columns")
})

test_that("imputation strategies abort on non-numeric required NAs and point to 'remove'", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "median",
    "artma.verbose" = 1
  ))

  df <- make_df(study_id = c("a", NA, "b"))
  expect_error(handle_missing_values(df), "remove")
})

test_that("an NA na_handling option behaves as the documented 'stop' default", {
  box::use(artma / data / na_handling[handle_missing_values])

  # Options with a `.na` template default load into options() as literal NA;
  # the read must fall back cleanly instead of crashing on `if (NA == ...)`
  # (issue #321, bug 1).
  local_options(list(
    "artma.data.na_handling" = NA,
    "artma.verbose" = 1
  ))

  df <- make_df(study_id = c("a", NA, "b"))
  expect_error(handle_missing_values(df), "non-numeric required columns")
})

# -- source column names in messages -------------------------------------------

test_that("required-NA messages show the source column name when mapped", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "stop",
    "artma.data.columns" = list(
      study_id = list(source_name = "Study Name")
    ),
    "artma.verbose" = 1
  ))

  df <- make_df(study_id = c("a", NA, "b"))
  expect_error(handle_missing_values(df), "Study Name")
})

test_that("required-NA messages fall back to the standardized column name", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "stop",
    "artma.data.columns" = list(),
    "artma.verbose" = 1
  ))

  df <- make_df(study_id = c("a", NA, "b"))
  expect_error(handle_missing_values(df), "study_id")
})

# -- numeric required columns keep their behavior ------------------------------

test_that("'stop' aborts on NAs in numeric required columns", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "stop",
    "artma.verbose" = 1
  ))

  df <- make_df()
  df$effect[2] <- NA_real_
  expect_error(handle_missing_values(df), "required columns")
})

test_that("'median' imputes NAs in numeric required columns", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "median",
    "artma.verbose" = 1
  ))

  df <- make_df()
  df$effect[2] <- NA_real_
  result <- handle_missing_values(df)

  expect_equal(nrow(result), 3)
  expect_equal(result$effect[2], stats::median(c(0.1, 0.3)))
})
