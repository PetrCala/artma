box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_message,
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

# -- missingness-ratio guard ---------------------------------------------------

# 10-row frame with a mostly-missing (90%) and a lightly-missing (10%) optional
# numeric column, so the default 0.5 guard splits them.
make_guard_df <- function() {
  data.frame(
    study_id = rep(c("a", "b"), each = 5),
    effect = seq(0.1, 1, length.out = 10),
    se = seq(0.01, 0.1, length.out = 10),
    n_obs = seq(10L, 100L, by = 10L),
    mod_sparse = c(1.5, rep(NA_real_, 9)),
    mod_dense = c(NA_real_, as.numeric(2:10)),
    stringsAsFactors = FALSE
  )
}

test_that("imputation skips optional columns above the missingness threshold", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "median",
    "artma.verbose" = 1
  ))

  result <- handle_missing_values(make_guard_df())

  expect_equal(sum(is.na(result$mod_sparse)), 9)
  expect_false(anyNA(result$mod_dense))
})

test_that("the missingness threshold is configurable via options", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "median",
    "artma.data.max_imputation_missingness" = 0.95,
    "artma.verbose" = 1
  ))

  result <- handle_missing_values(make_guard_df())

  expect_false(anyNA(result$mod_sparse))
  expect_equal(result$mod_sparse[2], 1.5)
})

test_that("a column exactly at the threshold is still imputed", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "median",
    "artma.verbose" = 1
  ))

  df <- make_guard_df()
  df$mod_half <- c(as.numeric(1:5), rep(NA_real_, 5))
  result <- handle_missing_values(df)

  expect_false(anyNA(result$mod_half))
})

test_that("required numeric columns are imputed regardless of the threshold", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "median",
    "artma.verbose" = 1
  ))

  df <- make_guard_df()
  df$effect[1:6] <- NA_real_
  result <- handle_missing_values(df)

  expect_false(anyNA(result$effect))
})

test_that("'interpolate' also honors the missingness guard", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "interpolate",
    "artma.verbose" = 1
  ))

  result <- handle_missing_values(make_guard_df())

  # Pre-guard, a single-valid-value column fell back to median imputation and
  # became a constant; now it must keep its missing values.
  expect_equal(sum(is.na(result$mod_sparse)), 9)
  expect_false(anyNA(result$mod_dense))
})

test_that("the guard emits a warning naming the skipped columns", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "median",
    "artma.verbose" = 2
  ))

  expect_message(
    handle_missing_values(make_guard_df()),
    "Skipping imputation"
  )
})

test_that("an invalid threshold option aborts with a clear message", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "median",
    "artma.data.max_imputation_missingness" = 1.5,
    "artma.verbose" = 1
  ))

  expect_error(
    handle_missing_values(make_guard_df()),
    "max_imputation_missingness"
  )
})

# -- "na" as a real category value (issue #402) --------------------------------

test_that("detect_missing_values does not flag a character column whose values are the literal 'na' category", {
  box::use(artma / data / na_handling[detect_missing_values])

  df <- make_df()
  df$discounting <- c("na", "exponential", "na")

  summary <- detect_missing_values(df)

  expect_false("discounting" %in% names(summary$optional_cols_with_na))
  expect_false(summary$has_optional_na)
})

test_that("detect_missing_values still flags real missing values in an optional numeric column", {
  box::use(artma / data / na_handling[detect_missing_values])

  df <- make_df()
  df$mod <- c(1, NA, 3)

  summary <- detect_missing_values(df)

  expect_true("mod" %in% names(summary$optional_cols_with_na))
  expect_equal(unname(summary$optional_cols_with_na["mod"]), 1)
})

test_that("'stop' does not abort when the only 'missing' values are the 'na' category", {
  box::use(artma / data / na_handling[handle_missing_values])

  local_options(list(
    "artma.data.na_handling" = "stop",
    "artma.verbose" = 1
  ))

  df <- make_df()
  df$discounting <- c("na", "exponential", "na")

  result <- handle_missing_values(df)

  expect_equal(result$discounting, c("na", "exponential", "na"))
})

test_that("'mice' leaves guarded columns unimputed", {
  box::use(artma / data / na_handling[handle_missing_values])
  testthat::skip_if_not_installed("mice")

  local_options(list(
    "artma.data.na_handling" = "mice",
    "artma.verbose" = 1
  ))

  # mice reads .Random.seed from the global environment and errors when the
  # RNG was never initialized on this worker.
  set.seed(42)
  df <- rbind(make_guard_df(), make_guard_df())
  result <- handle_missing_values(df)

  expect_equal(sum(is.na(result$mod_sparse)), 18)
  expect_false(anyNA(result$mod_dense))
})
