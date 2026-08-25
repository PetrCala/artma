box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_gt,
    expect_named,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(
  artma / data / column_recognition[recognize_columns],
  artma / data / derivation[
    derive_pcc_columns,
    derive_pcc_from_t_dof,
    describe_wide_format,
    detect_tdf_derivation,
    detect_wide_format,
    find_dof_column,
    find_horizon_families,
    looks_like_dof,
    role_name_is_canonical
  ],
  artma / data / interactive_mapping[confirm_derivation],
  artma / data / method_requirements[resolve_hard_required_colnames]
)


# --- Degrees-of-freedom column detection ------------------------------------

test_that("looks_like_dof accepts varying positive whole numbers", {
  expect_true(looks_like_dof(c(12, 45, 137, 422, 89)))
})


test_that("looks_like_dof rejects fractions, flags and zero-based counts", {
  expect_false(looks_like_dof(c(1.5, 2.25, 3.75, 10.1, 44.4)))
  expect_false(looks_like_dof(c(1, 2, 1, 2, 1, 2))) # two distinct values: a flag
  expect_false(looks_like_dof(c(0, 1, 5, 12, 30))) # a count starting at zero
  expect_false(looks_like_dof(c(NA, NA, NA, 40, 55), n_rows = 20)) # 25% coverage
})


test_that("find_dof_column requires both an anchored name and plausible values", {
  df <- data.frame(
    t = stats::rnorm(30),
    df = sample(20:400, 30),
    dfe = sample(20:400, 30), # not an anchored degrees-of-freedom name
    stringsAsFactors = FALSE
  )

  expect_equal(find_dof_column(df), "df")

  # The name alone is never enough: a 0/1 flag called "dof" is not a count.
  flagged <- data.frame(dof = rep(c(0, 1), 15))
  expect_null(find_dof_column(flagged))

  expect_null(find_dof_column(df, exclude = "df"))
})


# --- The partial-correlation formulas ---------------------------------------

test_that("derive_pcc_from_t_dof matches the closed-form conversion", {
  t_values <- c(2, -3.5, 0, 7.25)
  dof <- c(50, 120, 30, 800)

  derived <- derive_pcc_from_t_dof(t_values, dof)

  expect_equal(derived$effect, t_values / sqrt(t_values^2 + dof))
  expect_equal(derived$se, sqrt((1 - derived$effect^2) / dof))
})


test_that("the derived pair reproduces the t-statistic it came from", {
  t_values <- c(1.9, -4.2, 0.4, 11)
  dof <- c(64, 300, 45, 1200)

  derived <- derive_pcc_from_t_dof(t_values, dof)

  expect_equal(derived$effect / derived$se, t_values)
})


test_that("non-positive degrees of freedom yield NA rather than a complex number", {
  derived <- derive_pcc_from_t_dof(c(2, 2, 2), c(100, 0, -5))

  expect_true(is.finite(derived$effect[1]))
  expect_true(all(is.na(derived$effect[2:3])))
  expect_true(all(is.na(derived$se[2:3])))
})


# --- Choosing the derived route ---------------------------------------------

#' A dataset of the family this route exists for: coefficients reported in
#' incomparable units, with the t-statistic and degrees of freedom that make
#' them comparable sitting alongside under suffixed, non-canonical names.
make_incomparable_units_df <- function(n = 120, seed = 11) {
  withr::with_seed(seed, {
    dof <- sample(30:900, n, replace = TRUE)
    t_values <- round(stats::rnorm(n, 1.4, 2.2), 3)
    scale <- rep(c(1, 1000, 1e6), length.out = n)
    se <- round(abs(stats::rnorm(n, 0.4, 0.1)) * scale, 6)
    data.frame(
      Study = rep(sprintf("Author%02d", seq_len(n / 4)), each = 4),
      COEF_L = round(t_values * se, 6),
      SE_L = se,
      TSTAT_L = t_values,
      DF = dof,
      Sample = dof + 4L,
      stringsAsFactors = FALSE
    )
  })
}


test_that("detect_tdf_derivation displaces a non-canonical effect/se pair", {
  df <- make_incomparable_units_df()
  mapping <- list(effect = "COEF_L", se = "SE_L", t_stat = "TSTAT_L", study_id = "Study")

  derivation <- detect_tdf_derivation(df, mapping)

  expect_true(is.list(derivation))
  expect_equal(derivation$t_stat, "TSTAT_L")
  expect_equal(derivation$dof, "DF")
  expect_equal(unlist(derivation$replaces), c(effect = "COEF_L", se = "SE_L"))
  expect_gt(derivation$coverage, 0.9)
})


test_that("detect_tdf_derivation leaves a canonically named pair alone", {
  df <- make_incomparable_units_df()
  names(df)[names(df) == "COEF_L"] <- "estimate"
  names(df)[names(df) == "SE_L"] <- "se"
  mapping <- list(effect = "estimate", se = "se", t_stat = "TSTAT_L", study_id = "Study")

  expect_null(detect_tdf_derivation(df, mapping))
})


test_that("detect_tdf_derivation declines when the effect is already the partial correlation", {
  df <- make_incomparable_units_df()
  derived <- derive_pcc_from_t_dof(df$TSTAT_L, df$DF)
  df$COEF_L <- derived$effect
  df$SE_L <- derived$se
  mapping <- list(effect = "COEF_L", se = "SE_L", t_stat = "TSTAT_L", study_id = "Study")

  expect_null(detect_tdf_derivation(df, mapping))
})


test_that("detect_tdf_derivation needs both a t-statistic and a df companion", {
  df <- make_incomparable_units_df()
  mapping <- list(effect = "COEF_L", se = "SE_L", study_id = "Study")
  expect_null(detect_tdf_derivation(df, mapping))

  no_dof <- df[, setdiff(names(df), "DF"), drop = FALSE]
  expect_null(detect_tdf_derivation(no_dof, list(
    effect = "COEF_L", se = "SE_L", t_stat = "TSTAT_L", study_id = "Study"
  )))
})


test_that("role_name_is_canonical separates exact names from suffixed ones", {
  expect_true(role_name_is_canonical("se", "se"))
  expect_true(role_name_is_canonical("Estimate", "effect"))
  expect_false(role_name_is_canonical("SE_L", "se"))
  expect_false(role_name_is_canonical("COEF_L", "effect"))
})


test_that("recognize_columns declines the suffixed pair and reports the route", {
  withr::local_options(list("artma.verbose" = 1))
  df <- make_incomparable_units_df()

  mapping <- recognize_columns(df)
  derivation <- attr(mapping, "derivation")

  expect_false("effect" %in% names(mapping))
  expect_false("se" %in% names(mapping))
  expect_equal(derivation$t_stat, "TSTAT_L")
  expect_equal(derivation$dof, "DF")
  expect_true(grepl("TSTAT_L", attr(mapping, "declined")$effect$reason, fixed = TRUE))
})


# --- Wide-format sheets ------------------------------------------------------

#' An impulse-response sheet: one row per model, one column per horizon, and no
#' long-format effect/se pair anywhere.
make_wide_format_df <- function(n = 60, seed = 7) {
  withr::with_seed(seed, {
    horizons <- c(3, 6, 12, 18, 36)
    df <- data.frame(
      idstudy = rep(seq_len(n / 4), each = 4),
      study = rep(sprintf("Author%02d (200%d)", seq_len(n / 4), seq_len(n / 4) %% 10), each = 4),
      nobs = sample(80:900, n, replace = TRUE),
      lags = sample(1:8, n, replace = TRUE),
      stringsAsFactors = FALSE
    )
    for (h in horizons) {
      df[[sprintf("m%d_res", h)]] <- round(stats::rnorm(n, 0.3, 0.4), 4)
      df[[sprintf("SE%d", h)]] <- round(abs(stats::rnorm(n, 0.2, 0.05)), 4)
      df[[sprintf("band%d_u", h)]] <- round(stats::rnorm(n, 0.8, 0.4), 4)
    }
    df
  })
}


test_that("find_horizon_families groups the response columns by stem", {
  families <- find_horizon_families(make_wide_format_df())

  expect_true(all(c("m#_res", "se#", "band#_u") %in% names(families)))
  expect_equal(families[["m#_res"]]$horizons, c(3, 6, 12, 18, 36))
  expect_equal(families[["se#"]]$columns, c("SE3", "SE6", "SE12", "SE18", "SE36"))
})


test_that("detect_wide_format fires only when no long-format pair was mapped", {
  df <- make_wide_format_df()

  detection <- detect_wide_format(df, list(study_id = "study", n_obs = "nobs"))
  expect_true(is.list(detection))
  expect_true(length(detection$families) >= 3)

  expect_null(detect_wide_format(df, list(effect = "m3_res", se = "SE3")))
})


test_that("detect_wide_format ignores a long-format frame with numeric suffixes", {
  withr::local_seed(3)
  n <- 80
  df <- data.frame(
    study = rep(sprintf("Author%02d", seq_len(n / 4)), each = 4),
    effect = round(stats::rnorm(n, 0.2, 0.3), 4),
    se = round(abs(stats::rnorm(n, 0.15, 0.04)), 4),
    nobs = sample(50:900, n, replace = TRUE),
    lag1 = sample(0:1, n, replace = TRUE),
    lag2 = sample(0:1, n, replace = TRUE),
    lag3 = sample(0:1, n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  expect_null(detect_wide_format(df, list(study_id = "study")))
})


test_that("describe_wide_format names the layout and its families", {
  detection <- detect_wide_format(make_wide_format_df(), list())

  described <- describe_wide_format(detection)

  expect_true(grepl("wide format", described, fixed = TRUE))
  expect_true(grepl("SE3", described, fixed = TRUE))
})


test_that("recognize_columns attaches the wide-format decline to effect and se", {
  withr::local_options(list("artma.verbose" = 1))

  mapping <- recognize_columns(make_wide_format_df())
  declined <- attr(mapping, "declined")

  expect_false("effect" %in% names(mapping))
  expect_false("se" %in% names(mapping))
  expect_equal(declined$effect$layout, "wide_format")
  expect_equal(declined$se$layout, "wide_format")
  expect_true(grepl("wide format", declined$effect$reason, fixed = TRUE))
})


# --- Confirming the route ----------------------------------------------------

sample_derivation <- function() {
  list(
    t_stat = "TSTAT_L",
    dof = "DF",
    coverage = 0.95,
    replaces = list(effect = "COEF_L", se = "SE_L")
  )
}


test_that("a non-interactive session takes the derived route without asking", {
  result <- confirm_derivation(
    mapping = list(study_id = "Study"),
    derivation = sample_derivation(),
    select_fn = function(choices, prompt) stop("must not prompt"),
    is_interactive = FALSE
  )

  expect_equal(result$mapping$t_stat, "TSTAT_L")
  expect_equal(result$mapping$reg_dof, "DF")
  expect_equal(result$derivation$dof, "DF")
})


test_that("confirming the route maps the two inputs", {
  withr::local_options(list("artma.verbose" = 1))

  result <- confirm_derivation(
    mapping = list(study_id = "Study"),
    derivation = sample_derivation(),
    select_fn = function(choices, prompt) choices[1],
    is_interactive = TRUE
  )

  expect_equal(result$mapping$t_stat, "TSTAT_L")
  expect_equal(result$mapping$reg_dof, "DF")
  expect_false(is.null(result$derivation))
})


test_that("refusing the route restores the columns it displaced", {
  withr::local_options(list("artma.verbose" = 1))

  result <- confirm_derivation(
    mapping = list(study_id = "Study"),
    derivation = sample_derivation(),
    select_fn = function(choices, prompt) choices[2],
    is_interactive = TRUE
  )

  expect_null(result$derivation)
  expect_equal(result$mapping$effect, "COEF_L")
  expect_equal(result$mapping$se, "SE_L")
  expect_false("reg_dof" %in% names(result$mapping))
})


test_that("choosing neither leaves effect and se unmapped", {
  withr::local_options(list("artma.verbose" = 1))

  result <- confirm_derivation(
    mapping = list(study_id = "Study"),
    derivation = sample_derivation(),
    select_fn = function(choices, prompt) choices[length(choices)],
    is_interactive = TRUE
  )

  expect_null(result$derivation)
  expect_false("effect" %in% names(result$mapping))
  expect_false("se" %in% names(result$mapping))
})


test_that("confirm_derivation is a no-op without a proposal", {
  mapping <- list(effect = "e", se = "se")

  result <- confirm_derivation(mapping, derivation = NULL, is_interactive = TRUE)

  expect_equal(result$mapping, mapping)
  expect_null(result$derivation)
})


# --- The compute-phase step --------------------------------------------------

test_that("derive_pcc_columns is a no-op unless the option is set", {
  df <- data.frame(t_stat = c(2, 3), reg_dof = c(100, 200))

  withr::local_options(list("artma.data.derive_pcc" = FALSE))
  expect_equal(derive_pcc_columns(df), df)
})


test_that("derive_pcc_columns writes effect and se from t_stat and reg_dof", {
  withr::local_options(list(
    "artma.data.derive_pcc" = TRUE,
    "artma.verbose" = 1
  ))
  df <- data.frame(
    study_id = c("A", "B", "C"),
    t_stat = c(2, -3, 5),
    reg_dof = c(100, 200, 50)
  )

  result <- derive_pcc_columns(df)
  expected <- derive_pcc_from_t_dof(df$t_stat, df$reg_dof)

  expect_named(result, c("study_id", "t_stat", "reg_dof", "effect", "se"))
  expect_equal(result$effect, expected$effect)
  expect_equal(result$se, expected$se)
})


test_that("derive_pcc_columns coerces character inputs read from a spreadsheet", {
  withr::local_options(list(
    "artma.data.derive_pcc" = TRUE,
    "artma.verbose" = 1
  ))
  df <- data.frame(
    t_stat = c("2", "-3"),
    reg_dof = c("100", "200"),
    stringsAsFactors = FALSE
  )

  result <- derive_pcc_columns(df)

  expect_equal(result$effect, derive_pcc_from_t_dof(c(2, -3), c(100, 200))$effect)
})


test_that("derive_pcc_columns aborts when an input column is missing", {
  withr::local_options(list("artma.data.derive_pcc" = TRUE))

  expect_error(
    derive_pcc_columns(data.frame(t_stat = c(2, 3))),
    "reg_dof"
  )
})


test_that("the derived route swaps effect and se for its inputs in the required set", {
  withr::local_options(list("artma.data.derive_pcc" = TRUE))

  required <- resolve_hard_required_colnames()

  expect_false(any(c("effect", "se") %in% required))
  expect_true(all(c("study_id", "t_stat", "reg_dof") %in% required))
})


# --- End to end through the three pipeline phases -----------------------------

test_that("the pipeline prepares a file that carries only (t, df), not effect/se", {
  box::use(
    artma / data / index[configure_data, compute_data_impl, persist_data, prime_raw_df],
    artma / data / read[read_data],
    testing / fixtures / index[FIXTURES]
  )

  FIXTURES$local_cli_silence()

  fixture_dir <- withr::local_tempdir()
  n <- 80
  raw <- withr::with_seed(5, {
    dof <- sample(40:600, n, replace = TRUE)
    data.frame(
      Study = rep(sprintf("Author%02d (20%02d)", seq_len(n / 4), seq_len(n / 4)), each = 4),
      TSTAT_L = round(stats::rnorm(n, 1.2, 2), 4),
      DF = dof,
      Sample = dof + 4L,
      Region = rep(c("EU", "US"), length.out = n),
      stringsAsFactors = FALSE
    )
  })
  source_path <- file.path(fixture_dir, "tdf-data.csv")
  utils::write.csv(raw, source_path, row.names = FALSE)

  options_dir <- file.path(fixture_dir, "options")
  dir.create(options_dir)

  artma::options_create(
    options_file_name = "tdf.yaml",
    options_dir = options_dir,
    user_input = list(
      "data.source_path" = source_path,
      "data.derive_pcc" = TRUE,
      "data.na_handling" = "remove",
      "data.reconcile_mode" = "auto",
      "data.columns" = list(
        study_id = list(source_name = "Study"),
        t_stat = list(source_name = "TSTAT_L"),
        reg_dof = list(source_name = "DF"),
        n_obs = list(source_name = "Sample")
      ),
      "calc.se_zero_handling" = "ignore",
      "cache.use_cache" = FALSE,
      "verbose" = 1L
    ),
    should_validate = TRUE,
    should_overwrite = TRUE
  )

  withr::local_options(artma::options_load(
    options_file_name = "tdf.yaml",
    options_dir = options_dir,
    load_with_prefix = TRUE,
    should_validate = TRUE,
    should_add_temp_options = TRUE,
    should_return = TRUE
  ))

  df_raw <- read_data()
  prime_raw_df(df_raw)
  configure_data(df_raw)
  df <- compute_data_impl()
  persist_data(df)

  expect_true(all(c("effect", "se") %in% names(df)))
  expect_true(all(is.finite(df$effect)))
  expect_true(all(df$se > 0))
  # r = t / sqrt(t^2 + df) never leaves [-1, 1], and t is recovered from the pair
  expect_true(all(abs(df$effect) < 1))
  expect_equal(df$t_stat, df$effect / df$se, tolerance = 1e-8)
})


test_that("a displaced role stops being offered as a provisional candidate", {
  withr::local_options(list("artma.verbose" = 1))

  wide <- recognize_columns(make_wide_format_df())
  expect_false(any(c("effect", "se") %in% names(attr(wide, "provisional"))))

  derived <- recognize_columns(make_incomparable_units_df())
  expect_false(any(c("effect", "se") %in% names(attr(derived, "provisional"))))
})
