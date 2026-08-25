box::use(
  testthat[expect_equal, expect_error, expect_false, expect_setequal, expect_true, test_that],
  withr[defer, local_options, local_tempdir]
)

# Force artma/paths to load now, while box.path is still whatever the test
# runner started with. The synthetic-method tests below temporarily prepend a
# tempdir containing its own "artma/" subtree to box.path; get_pkg_path()
# resolves PATHS$PACKAGE_PATH by picking the first box.path entry that
# contains an "artma" directory, and that resolution runs (and its result is
# cached) only the first time artma/paths loads in the session. Loading it
# here, before any test touches box.path, guarantees that first resolution
# lands on the real package tree regardless of what other test files happen
# to run before this one.
box::use(artma / paths[PATHS])

# Regression coverage for https://github.com/PetrCala/artma/issues/400:
# standardize_column_names() used to hard-require n_obs (part of the fixed
# CONST$DATA$REQUIRED_COLNAMES set) regardless of which methods were actually
# requested, so a dataset with no per-estimate sample size aborted even when
# every requested method (e.g. effect_summary_stats, which only declares
# required_columns = c("effect", "study_size")) never touches it.
# resolve_hard_required_colnames() narrows the hard-required set to what the
# compute stage and the requested methods genuinely need.

# -- Unit tests: resolve_hard_required_colnames() against synthetic methods --
#
# Synthetic runtime method modules, independent of the real (evolving) method
# roster, so these tests exercise resolve_hard_required_colnames()'s own
# logic rather than any particular production method's declared columns.

build_synthetic_methods_dir <- function() {
  # .local_envir must target the caller's frame (the test_that block), not
  # this helper's own frame - otherwise the tempdir is cleaned up the instant
  # this function returns, before the caller ever reads from it.
  temp_root <- local_tempdir(.local_envir = parent.frame())
  methods_dir <- file.path(temp_root, "artma", "methods")
  dir.create(methods_dir, recursive = TRUE, showWarnings = FALSE)

  writeLines(c(
    "box::use(artma / modules / runtime_methods[register_runtime_method])",
    "run <- register_runtime_method(",
    "  function(df, ...) df,",
    "  stage = 'no_n_obs_needed',",
    "  required_columns = c('effect', 'study_size')",
    ")"
  ), file.path(methods_dir, "no_n_obs_needed.R"))

  writeLines(c(
    "box::use(artma / modules / runtime_methods[register_runtime_method])",
    "run <- register_runtime_method(",
    "  function(df, ...) df,",
    "  stage = 'needs_n_obs',",
    "  required_columns = c('effect', 'se', 'n_obs')",
    ")"
  ), file.path(methods_dir, "needs_n_obs.R"))

  writeLines(c(
    "box::use(artma / modules / runtime_methods[register_runtime_method])",
    "run <- register_runtime_method(",
    "  function(df, ...) df,",
    "  stage = 'needs_precision',",
    "  required_columns = c('effect', 'precision')",
    ")"
  ), file.path(methods_dir, "needs_precision.R"))

  defer(
    {
      try(box::unload("artma/methods/no_n_obs_needed"), silent = TRUE)
      try(box::unload("artma/methods/needs_n_obs"), silent = TRUE)
      try(box::unload("artma/methods/needs_precision"), silent = TRUE)
    },
    envir = parent.frame()
  )

  local_options(list(box.path = c(temp_root, getOption("box.path"))), .local_envir = parent.frame())

  methods_dir
}


test_that("resolve_hard_required_colnames keeps the full set when methods is NULL", {
  box::use(
    artma / const[CONST],
    artma / data / method_requirements[resolve_hard_required_colnames]
  )

  methods_dir <- build_synthetic_methods_dir()
  expect_setequal(
    resolve_hard_required_colnames(NULL, modules_dir = methods_dir),
    CONST$DATA$REQUIRED_COLNAMES
  )
})

test_that("resolve_hard_required_colnames keeps the full set for methods = 'all'", {
  box::use(
    artma / const[CONST],
    artma / data / method_requirements[resolve_hard_required_colnames]
  )

  methods_dir <- build_synthetic_methods_dir()
  expect_setequal(
    resolve_hard_required_colnames("all", modules_dir = methods_dir),
    CONST$DATA$REQUIRED_COLNAMES
  )
})

test_that("resolve_hard_required_colnames drops n_obs when no requested method needs it", {
  box::use(
    artma / data / method_requirements[resolve_hard_required_colnames]
  )

  methods_dir <- build_synthetic_methods_dir()
  result <- resolve_hard_required_colnames("no_n_obs_needed", modules_dir = methods_dir)

  expect_setequal(result, c("study_id", "effect", "se"))
  expect_false("n_obs" %in% result)
})

test_that("resolve_hard_required_colnames keeps n_obs when a requested method needs it", {
  box::use(
    artma / data / method_requirements[resolve_hard_required_colnames]
  )

  methods_dir <- build_synthetic_methods_dir()
  result <- resolve_hard_required_colnames("needs_n_obs", modules_dir = methods_dir)

  expect_true("n_obs" %in% result)
})

test_that("resolve_hard_required_colnames unions requirements across every requested method", {
  box::use(
    artma / data / method_requirements[resolve_hard_required_colnames]
  )

  methods_dir <- build_synthetic_methods_dir()
  result <- resolve_hard_required_colnames(
    c("no_n_obs_needed", "needs_n_obs"),
    modules_dir = methods_dir
  )

  # n_obs is required because at least one requested method needs it, even
  # though the other requested method does not.
  expect_true("n_obs" %in% result)
})

test_that("resolve_hard_required_colnames ignores unknown method names", {
  box::use(
    artma / const[CONST],
    artma / data / method_requirements[resolve_hard_required_colnames]
  )

  methods_dir <- build_synthetic_methods_dir()

  # Every requested name is unknown: nothing to narrow by, fall back to the
  # full historical set so invoke_runtime_methods() can report the real error.
  expect_setequal(
    resolve_hard_required_colnames("not_a_real_method", modules_dir = methods_dir),
    CONST$DATA$REQUIRED_COLNAMES
  )

  # A mix of a known and an unknown name still narrows by the known one.
  result <- resolve_hard_required_colnames(
    c("no_n_obs_needed", "not_a_real_method"),
    modules_dir = methods_dir
  )
  expect_false("n_obs" %in% result)
})

test_that("resolve_hard_required_colnames requires n_obs for precision only under DoF", {
  box::use(
    artma / data / method_requirements[resolve_hard_required_colnames]
  )

  methods_dir <- build_synthetic_methods_dir()

  local_options(list("artma.calc.precision_type" = "1/SE"))
  result_default <- resolve_hard_required_colnames("needs_precision", modules_dir = methods_dir)
  expect_false("n_obs" %in% result_default)

  local_options(list("artma.calc.precision_type" = "DoF"))
  result_dof <- resolve_hard_required_colnames("needs_precision", modules_dir = methods_dir)
  expect_true("n_obs" %in% result_dof)
})

# -- Integration tests: the resolved set actually gates standardize_column_names --

test_that("standardize_column_names passes on data missing n_obs when required_colnames omits it", {
  box::use(artma / data / utils[standardize_column_names])

  mock_df <- data.frame(
    study_id = 1:5,
    effect = c(0.1, 0.2, 0.3, 0.4, 0.5),
    se = c(0.01, 0.02, 0.03, 0.04, 0.05)
  )

  withr::with_options(list("artma.data.columns" = list()), {
    expect_error(
      standardize_column_names(mock_df, required_colnames = c("study_id", "effect", "se")),
      NA
    )
  })
})

test_that("standardize_column_names still aborts on data missing n_obs when it is required", {
  box::use(
    artma / const[CONST],
    artma / data / utils[standardize_column_names]
  )

  mock_df <- data.frame(
    study_id = 1:5,
    effect = c(0.1, 0.2, 0.3, 0.4, 0.5),
    se = c(0.01, 0.02, 0.03, 0.04, 0.05)
  )

  withr::with_options(list("artma.data.columns" = list()), {
    expect_error(
      standardize_column_names(mock_df, required_colnames = CONST$DATA$REQUIRED_COLNAMES),
      "Missing mapping for required columns"
    )
  })
})

# -- End-to-end: prepare_data() with real, production methods --
#
# Reproduces the exact issue #400 scenario through the full configure/compute
# pipeline, using the real effect_summary_stats and maive methods rather than
# synthetic ones, on a dataset that (like the climate-sensitivity dataset in
# the issue) has study_id/effect/se but no n_obs.
#
# Options are set directly (as in test-data-index.R) rather than via
# artma::options_create(), which would run this fixture's n_obs-less data
# through the interactive column-detection workflow; non-interactively, that
# workflow's "no good candidate" fallback maps the missing n_obs role onto
# whatever column it lands on first (a pre-existing quirk unrelated to this
# fix), which is not what these tests are about. Setting the options directly
# leaves "artma.data.columns" with no n_obs entry at all, exactly the target
# scenario. Computed-column entries are pre-seeded (as in test-data-index.R)
# so persist_data() finds nothing new to write and never needs a real options
# file target (artma.temp.file_name/dir_name).
precomputed_computed_column_overrides <- list(
  obs_id = list(var_name = "obs_id", is_computed = TRUE),
  study_id = list(var_name = "study_id", is_computed = TRUE),
  study_label = list(var_name = "study_label", is_computed = TRUE),
  t_stat = list(var_name = "t_stat", is_computed = TRUE),
  study_size = list(var_name = "study_size", is_computed = TRUE),
  reg_dof = list(var_name = "reg_dof", is_computed = TRUE),
  precision = list(var_name = "precision", is_computed = TRUE)
)

shared_runtime_options <- local({
  fixture_dir <- withr::local_tempdir(.local_envir = testthat::teardown_env())

  set.seed(400)
  df <- data.frame(
    study_id = rep(paste0("study_", 1:5), each = 4),
    effect = round(stats::rnorm(20, 0.3, 0.1), 4),
    se = round(stats::runif(20, 0.05, 0.2), 4),
    stringsAsFactors = FALSE
  )
  source_path <- file.path(fixture_dir, "no-n-obs-data.csv")
  utils::write.csv(df, source_path, row.names = FALSE)

  list(
    "artma.cache.use_cache" = FALSE,
    "artma.data.source_path" = source_path,
    "artma.data.columns" = precomputed_computed_column_overrides,
    "artma.data.config_setup" = "auto",
    "artma.data.na_handling" = "remove",
    "artma.data.reconcile_mode" = "auto",
    "artma.data.expected_schema_columns" = NA_character_,
    "artma.calc.se_zero_handling" = "ignore",
    "artma.verbose" = 1
  )
})

test_that("prepare_data succeeds without n_obs when the sole requested method does not need it", {
  local_options(shared_runtime_options)

  box::use(artma / data / index[prepare_data])

  df <- prepare_data(methods = "effect_summary_stats")
  expect_false("n_obs" %in% colnames(df))
  expect_true("study_size" %in% colnames(df))
})

test_that("prepare_data succeeds a second time, once schema reconciliation has a baseline", {
  # detect_schema_drift() independently defaults every required role with no
  # stored mapping to an identity mapping, so a run-specific required set
  # must reach reconcile_schema() too, not just standardize_column_names():
  # otherwise the *second* prepare_data() call (once
  # artma.data.expected_schema_columns holds a baseline, so drift is no
  # longer unconditionally suppressed) would treat n_obs as a dropped role
  # and abort during schema reconciliation, before standardize_column_names()
  # is ever reached.
  local_options(shared_runtime_options)

  box::use(artma / data / index[prepare_data])

  first <- prepare_data(methods = "effect_summary_stats")
  expect_false("n_obs" %in% colnames(first))

  second <- prepare_data(methods = "effect_summary_stats")
  expect_false("n_obs" %in% colnames(second))
})

test_that("prepare_data errors when the sole requested method needs n_obs and it is absent", {
  # Caught inside schema reconciliation, by auto_decisions(). A missing
  # required role is baseline-independent, so this is the same error whether or
  # not artma.data.expected_schema_columns holds a baseline yet;
  # standardize_column_names()'s blunter "Missing mapping" abort is now only a
  # backstop for callers that skip reconciliation entirely.
  local_options(shared_runtime_options)

  box::use(artma / data / index[prepare_data])

  expect_error(
    prepare_data(methods = "maive"),
    "Cannot auto-resolve missing required column"
  )
})

test_that("prepare_data errors for a method needing n_obs even once a schema baseline exists", {
  # The companion to the test above: having a baseline must not change which
  # error surfaces. Before, a first run fell through reconciliation to
  # standardize_column_names() while later runs stopped in auto_decisions();
  # the two paths now agree.
  local_options(shared_runtime_options)

  box::use(artma / data / index[prepare_data])

  prepare_data(methods = "effect_summary_stats") # establishes the baseline

  expect_error(
    prepare_data(methods = "maive"),
    "Cannot auto-resolve missing required column"
  )
})

test_that("prepare_data unions requirements: n_obs is required if any requested method needs it", {
  local_options(shared_runtime_options)

  box::use(artma / data / index[prepare_data])

  expect_error(
    prepare_data(methods = c("effect_summary_stats", "maive")),
    "Cannot auto-resolve missing required column"
  )
})

test_that("prepare_data with methods = 'all' keeps the historical strict requirement", {
  local_options(shared_runtime_options)

  box::use(artma / data / index[prepare_data])

  expect_error(
    prepare_data(methods = "all"),
    "Cannot auto-resolve missing required column"
  )
})

test_that("prepare_data with methods = NULL keeps the historical strict requirement", {
  local_options(shared_runtime_options)

  box::use(artma / data / index[prepare_data])

  expect_error(
    prepare_data(),
    "Cannot auto-resolve missing required column"
  )
})
