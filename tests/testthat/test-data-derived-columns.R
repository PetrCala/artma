box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_identical,
    expect_named,
    expect_true,
    test_that
  ]
)

box::use(
  artma / data / derived_columns[
    apply_derived_columns,
    coerce_derived_value,
    derived_column_names,
    get_derived_specs
  ]
)


#' A frame with enough rows and spread for the derived-encoding correlation
#' check to have something to work with.
sample_df <- function(n = 40) {
  set.seed(525)
  effect <- stats::rnorm(n)
  data.frame(
    effect = effect,
    se = abs(stats::rnorm(n)) + 0.1,
    top_journal = rep(c(0, 1), length.out = n),
    estimate_category = rep(c("preferred", "discounted", "other", "preferred"), length.out = n),
    stringsAsFactors = FALSE
  )
}


# --- get_derived_specs ------------------------------------------------------

test_that("get_derived_specs returns nothing when the option is unset", {
  withr::local_options(list("artma.data.derived" = NULL))
  expect_equal(length(get_derived_specs()), 0L)
})


test_that("get_derived_specs treats the NA template sentinel as empty", {
  withr::local_options(list("artma.data.derived" = NA))
  expect_equal(length(get_derived_specs()), 0L)
  expect_equal(derived_column_names(), character(0))
})


test_that("get_derived_specs normalizes a named list into a named character vector", {
  withr::local_options(list(
    "artma.data.derived" = list(a = "se * 2", b = "a + 1")
  ))

  specs <- get_derived_specs()
  expect_identical(specs, c(a = "se * 2", b = "a + 1"))
  expect_equal(derived_column_names(), c("a", "b"))
})


test_that("get_derived_specs rejects an unnamed entry", {
  withr::local_options(list("artma.data.derived" = list("se * 2")))
  expect_error(get_derived_specs(), "must be named")
})


test_that("get_derived_specs rejects a non-string expression", {
  withr::local_options(list("artma.data.derived" = list(a = 12)))
  expect_error(get_derived_specs(), "single R expression")
})


# --- coerce_derived_value ---------------------------------------------------

test_that("coerce_derived_value turns logicals into 0/1 integers", {
  expect_identical(coerce_derived_value(c(TRUE, FALSE, NA), "d", 3), c(1L, 0L, NA_integer_))
})


test_that("coerce_derived_value recycles a scalar across the frame", {
  expect_identical(coerce_derived_value(2, "d", 3), c(2, 2, 2))
})


test_that("coerce_derived_value rejects a length mismatch", {
  expect_error(coerce_derived_value(c(1, 2), "d", 3), "one per row")
})


test_that("coerce_derived_value rejects a non-atomic value", {
  expect_error(coerce_derived_value(list(1, 2, 3), "d", 3), "atomic vector")
})


# --- apply_derived_columns --------------------------------------------------

test_that("apply_derived_columns is a no-op when the option is unset", {
  withr::local_options(list("artma.data.derived" = NA, "artma.verbose" = 1))

  df <- sample_df()
  expect_identical(apply_derived_columns(df), df)
})


test_that("apply_derived_columns builds an interaction term", {
  withr::local_options(list(
    "artma.data.derived" = list(se_x_top_journal = "se * top_journal"),
    "artma.verbose" = 1
  ))

  df <- sample_df()
  result <- apply_derived_columns(df)

  expect_true("se_x_top_journal" %in% names(result))
  expect_equal(result$se_x_top_journal, df$se * df$top_journal)
})


test_that("apply_derived_columns builds indicators from a categorical column", {
  withr::local_options(list(
    "artma.data.derived" = list(
      preferred = "estimate_category == 'preferred'",
      discounted = "estimate_category == 'discounted'"
    ),
    "artma.verbose" = 1
  ))

  df <- sample_df()
  result <- apply_derived_columns(df)

  expect_identical(result$preferred, as.integer(df$estimate_category == "preferred"))
  expect_identical(result$discounted, as.integer(df$estimate_category == "discounted"))
})


test_that("apply_derived_columns lets a later expression build on an earlier one", {
  withr::local_options(list(
    "artma.data.derived" = list(
      preferred = "estimate_category == 'preferred'",
      se_x_preferred = "se * preferred"
    ),
    "artma.verbose" = 1
  ))

  df <- sample_df()
  result <- apply_derived_columns(df)

  expect_equal(
    result$se_x_preferred,
    df$se * as.integer(df$estimate_category == "preferred")
  )
})


test_that("apply_derived_columns rejects a name that shadows a standard column", {
  withr::local_options(list(
    "artma.data.derived" = list(effect = "se * 2"),
    "artma.verbose" = 1
  ))

  expect_error(apply_derived_columns(sample_df()), "shadow a standard column")
})


test_that("apply_derived_columns rejects a name already in the data", {
  withr::local_options(list(
    "artma.data.derived" = list(top_journal = "se * 2"),
    "artma.verbose" = 1
  ))

  expect_error(apply_derived_columns(sample_df()), "overwrite an existing column")
})


test_that("apply_derived_columns rejects a non-syntactic name", {
  withr::local_options(list(
    "artma.data.derived" = list("se x top journal" = "se * top_journal"),
    "artma.verbose" = 1
  ))

  expect_error(apply_derived_columns(sample_df()), "not a valid column name")
})


test_that("apply_derived_columns reports an unparseable expression", {
  withr::local_options(list(
    "artma.data.derived" = list(broken = "se *"),
    "artma.verbose" = 1
  ))

  expect_error(apply_derived_columns(sample_df()), "Could not parse")
})


test_that("apply_derived_columns reports an expression naming a missing column", {
  withr::local_options(list(
    "artma.data.derived" = list(bad = "se * nonexistent"),
    "artma.verbose" = 1
  ))

  expect_error(apply_derived_columns(sample_df()), "Failed to evaluate")
})


# --- Degenerate-moderator rejection -----------------------------------------

test_that("apply_derived_columns rejects a derived encoding of the effect", {
  withr::local_options(list(
    "artma.data.derived" = list(t_like = "effect / se"),
    "artma.data.columns" = list(),
    "artma.verbose" = 1
  ))

  expect_error(apply_derived_columns(sample_df()), "derived encoding")
})


test_that("bma_allow_derived keeps a flagged derived column", {
  withr::local_options(list(
    "artma.data.derived" = list(t_like = "effect / se"),
    "artma.data.columns" = list(t_like = list(var_name = "t_like", bma_allow_derived = TRUE)),
    "artma.verbose" = 1
  ))

  result <- apply_derived_columns(sample_df())
  expect_true("t_like" %in% names(result))
})


test_that("bma: false keeps a flagged derived column out of the moderator check", {
  withr::local_options(list(
    "artma.data.derived" = list(t_like = "effect / se"),
    "artma.data.columns" = list(t_like = list(var_name = "t_like", bma = FALSE)),
    "artma.verbose" = 1
  ))

  result <- apply_derived_columns(sample_df())
  expect_true("t_like" %in% names(result))
})


test_that("an interaction of the standard error with a dummy is not flagged", {
  withr::local_options(list(
    "artma.data.derived" = list(se_x_top_journal = "se * top_journal"),
    "artma.data.columns" = list(),
    "artma.verbose" = 1
  ))

  result <- apply_derived_columns(sample_df())
  expect_named(
    result,
    c("effect", "se", "top_journal", "estimate_category", "se_x_top_journal")
  )
})


# --- Schema reconciliation (#541) -------------------------------------------

#' A raw frame with everything the data pipeline treats as a required role,
#' plus one moderator to derive from.
raw_regression_df <- function(n = 40) {
  set.seed(541)
  data.frame(
    effect = stats::rnorm(n),
    se = abs(stats::rnorm(n)) + 0.1,
    study_id = rep(paste0("S", 1:8), length.out = n),
    n_obs = 100L,
    top5_journal = rep(c(0, 1), length.out = n),
    stringsAsFactors = FALSE
  )
}

test_that("a derived column configured with bma reaches the BMA moderator set", {
  box::use(
    artma / data / schema_reconcile[reconcile_schema],
    artma / methods / bma[prepare_bma_inputs]
  )

  raw_df <- raw_regression_df()

  withr::local_options(list(
    "artma.data.derived" = list(se_top5_journal = "se * top5_journal"),
    "artma.data.columns" = list(
      effect = list(source_name = "effect"),
      se = list(source_name = "se"),
      study_id = list(source_name = "study_id"),
      top5_journal = list(var_name = "top5_journal", bma = TRUE),
      se_top5_journal = list(var_name = "se_top5_journal", bma = TRUE)
    ),
    "artma.data.expected_schema_columns" = colnames(raw_df),
    "artma.temp.file_name" = NULL,
    "artma.temp.dir_name" = NULL,
    "artma.verbose" = 1
  ))

  reconcile_schema(raw_df, mode = "auto")

  config <- getOption("artma.data.columns")
  expect_true(isTRUE(config$se_top5_journal$bma))

  df <- apply_derived_columns(raw_df)
  expect_true("se_top5_journal" %in% names(df))

  prepared <- prepare_bma_inputs(
    df, config,
    use_vif_optimization = FALSE,
    max_groups_to_remove = 3,
    verbosity = 1
  )

  expect_true("se_top5_journal" %in% colnames(prepared$bma_data))
})
