box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_message,
    expect_true,
    test_that
  ],
  withr[local_options]
)

box::use(
  artma / methods / bma[prepare_bma_inputs],
  artma / variable / bma[
    detect_derived_effect_encodings,
    flag_derived_bma_candidates,
    suggest_variables_for_bma
  ]
)

#' Tests for derived-encoding detection in BMA moderator selection
#'
#' Published meta-analysis datasets commonly ship helper columns derived from
#' the effect or its standard error (winsorized copies, t-statistics, inverse
#' SEs). These must never enter the BMA moderator set automatically: the model
#' would regress the effect on an encoding of itself, zeroing out every
#' genuine moderator and collapsing the best-practice estimate to the sample
#' mean. Column names mirror a real case (armel_w, se_w, tstats, invse in the
#' Armington dataset) and deliberately dodge the name-based group heuristics,
#' so only the correlation-based detection can catch them.

winsorize_at <- function(x, level = 0.025) {
  bounds <- stats::quantile(x, probs = c(level, 1 - level), names = FALSE)
  pmin(pmax(x, bounds[1]), bounds[2])
}

make_derived_encodings_data <- function() {
  set.seed(2024)
  n <- 120L
  effect <- rnorm(n, mean = 1.5, sd = 1.2)
  se <- runif(n, min = 0.1, max = 1.5)

  data.frame(
    study_id = rep(paste0("S", seq_len(20L)), each = 6L),
    effect = effect,
    se = se,
    # Derived encodings that ship with many published datasets
    effect_w = winsorize_at(effect),
    se_w = winsorize_at(se),
    tstats = effect / se,
    invse = 1 / se,
    # Genuine moderators
    noise = rnorm(n, mean = 10, sd = 3),
    dummy_us = sample(c(0, 1), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

make_derived_encodings_config <- function() {
  vars <- c(
    "effect", "se", "effect_w", "se_w", "tstats", "invse", "noise", "dummy_us"
  )
  types <- c(
    effect = "float", se = "float", effect_w = "float", se_w = "float",
    tstats = "float", invse = "float", noise = "float", dummy_us = "dummy"
  )
  stats::setNames(
    lapply(vars, function(v) {
      list(var_name = v, data_type = types[[v]], bma = NA)
    }),
    vars
  )
}


# Detection -------------------------------------------------------------------

test_that("detect_derived_effect_encodings flags transforms of effect and se", {
  df <- make_derived_encodings_data()

  flagged <- detect_derived_effect_encodings(
    df,
    c("effect_w", "se_w", "tstats", "invse", "noise", "dummy_us")
  )

  expect_true(all(c("effect_w", "se_w", "tstats", "invse") %in% flagged$var_name))
  expect_false("noise" %in% flagged$var_name)
  expect_false("dummy_us" %in% flagged$var_name)
  expect_true(all(flagged$correlation >= 0.9))
  expect_true(all(!is.na(flagged$target)))
})

test_that("detect_derived_effect_encodings works without an se column", {
  df <- make_derived_encodings_data()
  df <- df[, c("effect", "effect_w", "noise")]

  flagged <- detect_derived_effect_encodings(df, c("effect_w", "noise"))

  expect_true("effect_w" %in% flagged$var_name)
  expect_false("noise" %in% flagged$var_name)
})

test_that("detect_derived_effect_encodings skips non-numeric and sparse columns", {
  df <- make_derived_encodings_data()
  df$label <- sample(letters, nrow(df), replace = TRUE)
  df$sparse_copy <- df$effect
  df$sparse_copy[seq_len(nrow(df) - 5L)] <- NA_real_

  flagged <- detect_derived_effect_encodings(df, c("label", "sparse_copy"))

  expect_equal(nrow(flagged), 0L)
})

test_that("detect_derived_effect_encodings returns empty for empty candidates", {
  df <- make_derived_encodings_data()

  flagged <- detect_derived_effect_encodings(df, character(0))

  expect_equal(nrow(flagged), 0L)
  expect_equal(names(flagged), c("var_name", "target", "correlation"))
})

test_that("flag_derived_bma_candidates honors the bma_allow_derived override", {
  local_options(list(artma.verbose = 0))
  df <- make_derived_encodings_data()

  flagged_all <- flag_derived_bma_candidates(
    df, c("effect_w", "tstats"),
    config = NULL
  )
  expect_true(all(c("effect_w", "tstats") %in% flagged_all$var_name))

  config <- list(
    effect_w = list(var_name = "effect_w", bma_allow_derived = TRUE)
  )
  flagged <- flag_derived_bma_candidates(
    df, c("effect_w", "tstats"),
    config = config
  )
  expect_false("effect_w" %in% flagged$var_name)
  expect_true("tstats" %in% flagged$var_name)
})


# Suggestion integration ------------------------------------------------------

test_that("suggest_variables_for_bma excludes derived encodings and warns", {
  local_options(list(artma.verbose = 3))
  df <- make_derived_encodings_data()
  config <- make_derived_encodings_config()

  suggestions <- NULL
  expect_message(
    suggestions <- suggest_variables_for_bma(df, config = config),
    "derived encoding"
  )

  suggested <- suggestions$var_name[suggestions$suggested]

  expect_false(any(c("effect_w", "se_w", "tstats", "invse") %in% suggested))
  expect_true("noise" %in% suggested)
  expect_true("se" %in% suggested) # priority variable stays by convention
})

test_that("suggest_variables_for_bma keeps a whitelisted derived column", {
  local_options(list(artma.verbose = 0))
  df <- make_derived_encodings_data()
  config <- make_derived_encodings_config()
  config$effect_w$bma_allow_derived <- TRUE

  suggestions <- suggest_variables_for_bma(df, config = config)
  suggested <- suggestions$var_name[suggestions$suggested]

  expect_true("effect_w" %in% suggested)
  expect_false(any(c("se_w", "tstats", "invse") %in% suggested))
})


# Configured moderators (prepare_bma_inputs) ----------------------------------

test_that("prepare_bma_inputs warns about configured derived moderators but keeps them", {
  local_options(list(artma.verbose = 1))
  df <- make_derived_encodings_data()
  config <- list(
    se = list(var_name = "se", bma = TRUE),
    noise = list(var_name = "noise", bma = TRUE),
    effect_w = list(var_name = "effect_w", bma = TRUE)
  )

  prepared <- NULL
  expect_message(
    prepared <- prepare_bma_inputs(
      df, config,
      use_vif_optimization = FALSE,
      max_groups_to_remove = 3,
      verbosity = 2
    ),
    "derived encoding"
  )

  # Explicit configuration wins: the column stays in the model data
  expect_true("effect_w" %in% colnames(prepared$bma_data))
})

test_that("prepare_bma_inputs stays quiet when the derived moderator is whitelisted", {
  local_options(list(artma.verbose = 1))
  df <- make_derived_encodings_data()
  config <- list(
    se = list(var_name = "se", bma = TRUE),
    noise = list(var_name = "noise", bma = TRUE),
    effect_w = list(
      var_name = "effect_w", bma = TRUE, bma_allow_derived = TRUE
    )
  )

  msgs <- testthat::capture_messages(
    prepare_bma_inputs(
      df, config,
      use_vif_optimization = FALSE,
      max_groups_to_remove = 3,
      verbosity = 2
    )
  )

  expect_false(any(grepl("derived encoding", msgs)))
})
