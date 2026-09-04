box::use(
  artma / libs / infrastructure / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

# `prepare_data()` is split into three explicit phases:
#
#   configure -> compute -> persist
#
# * configure (interactive, uncached, runs first): resolves schema drift and
#   the missing-value and zero-standard-error strategies, prompting where the
#   autonomy/interactivity gate allows it, and updating the persisted config.
#   Skipped decisions fall back to deterministic defaults.
# * compute (pure, cached): read is shared with configure; standardizes,
#   preprocesses, and derives columns using only the resolved config. It is the
#   ONLY phase behind the cache and performs no prompts and no option writes.
# * persist (idempotent, always runs): registers the computed columns in the
#   data config. Running it twice, warm or cold cache, is a no-op.
#
# The raw data frame is read exactly once per run (in `prepare_data`) and handed
# to the cached compute phase through this module-scoped env, so the compute
# cache key stays keyed only on `build_data_cache_signature()` rather than on the
# hashed data frame content. The requested methods ride along the same env so
# the compute phase can resolve the same run-specific required-column set as
# the configure phase, without becoming part of the cache key: the resolved
# set only ever gates an abort, and that gate always runs afresh in the
# uncached configure phase below, so cache warmth cannot hide a genuine miss.
.raw_env <- new.env(parent = emptyenv())

#' @title Prime the raw data frame for the compute phase
#' @description Store the raw data frame that `compute_data_impl()` reads, plus
#'   the methods requested for this run (used to narrow the hard-required
#'   column set; see `artma / data / method_requirements
#'   [resolve_hard_required_colnames]`). Called by `prepare_data()` after its
#'   single disk read; also usable by tests that drive the compute phase in
#'   isolation.
#' @param df_raw *[data.frame]* Raw data frame with original column names.
#' @param methods *[character, optional]* The methods requested for this run,
#'   as passed to `artma()`. Defaults to `NULL` (unresolved selection).
#' @return `NULL`, invisibly.
#' @keywords internal
prime_raw_df <- function(df_raw, methods = NULL) {
  .raw_env$df_raw <- df_raw
  .raw_env$methods <- methods
  invisible(NULL)
}

#' @title Configure phase
#' @description Interactive, uncached phase. Resolves any schema drift and
#'   decides the missing-value and zero-standard-error handling strategies
#'   before the cached compute phase runs, so compute never prompts and its
#'   cache key is stable across runs.
#' @param df_raw *[data.frame]* Raw data frame with original column names.
#' @param methods *[character, optional]* The methods requested for this run,
#'   as passed to `artma()`. Narrows the hard-required column set (see
#'   `artma / data / method_requirements[resolve_hard_required_colnames]`).
#'   Defaults to `NULL`, which keeps the full historical required set.
#' @return `NULL`, invisibly.
#' @keywords internal
configure_data <- function(df_raw, methods = NULL) {
  box::use(
    artma / data / utils[standardize_column_names],
    artma / data / method_requirements[resolve_hard_required_colnames],
    artma / data / schema_reconcile[reconcile_schema],
    artma / data / preprocess[clean_data],
    artma / data / configure[resolve_na_handling, resolve_se_zero_handling],
    artma / data / derivation[derive_pcc_columns]
  )

  # This is the run's hard-required-column gate: it runs on every
  # prepare_data() call (this phase is never cached), so a method list that
  # newly needs a column re-evaluates the check even when the compute phase
  # below would otherwise hit a stale cache.
  required_colnames <- resolve_hard_required_colnames(methods)

  # Detect and resolve any schema drift before column standardization. This may
  # prompt and updates both in-memory and persisted config. Passing the same
  # run-specific required set keeps a column no requested method needs (e.g.
  # n_obs) from forcing schema reconciliation itself to abort or prompt when
  # it is genuinely absent and was never explicitly mapped.
  mode <- getOption("artma.data.reconcile_mode", "ask")
  reconcile_schema(df_raw, mode = mode, required_colnames = required_colnames)

  # Decide the missing-value and zero-SE strategies on the cleaned, standardized
  # frame so the compute phase can handle both without prompting. Standardize
  # quietly: this frame is a configure-phase intermediate, and the compute phase
  # standardizes the same raw frame again with messages on.
  df_std <- standardize_column_names(df_raw, quiet = TRUE, required_colnames = required_colnames)
  # The derived effect/se have to exist before the zero-SE and missing-value
  # strategies are decided, or both decisions would be taken on a frame missing
  # the two columns they are about.
  df_std <- suppressMessages(derive_pcc_columns(df_std))
  df_clean <- clean_data(df_std)
  resolve_na_handling(df_clean)
  resolve_se_zero_handling(df_clean)

  invisible(NULL)
}

#' @title Compute phase implementation
#' @description Pure, cacheable phase. Standardizes the raw frame, preprocesses
#'   it, and derives the optional columns using only the resolved config. It
#'   reads the raw frame primed by `prepare_data()` (no extra disk read) and
#'   performs no prompts and no option writes.
#' @return *[data.frame]* The prepared data frame.
#' @keywords internal
compute_data_impl <- function() {
  box::use(
    artma / data / utils[standardize_column_names],
    artma / data / method_requirements[resolve_hard_required_colnames],
    artma / data_config / resolve[prime_df_for_config_cache],
    artma / data / preprocess[preprocess_data],
    artma / data / compute[compute_optional_columns],
    artma / data / derivation[derive_pcc_columns],
    artma / data / derived_columns[apply_derived_columns]
  )

  df_raw <- .raw_env$df_raw

  # Apply colnames map (now updated if drift was reconciled in configure). The
  # configure phase already ran this same gate (uncached, always), so this
  # cannot abort here on a path configure_data() didn't already clear; it is
  # recomputed rather than cached alongside df_raw purely so this call reports
  # the same run-specific required set the configure phase used.
  required_colnames <- resolve_hard_required_colnames(.raw_env$methods)
  df <- standardize_column_names(df_raw, required_colnames = required_colnames)

  # Derived before the config is primed, so `effect` and `se` are configured,
  # cleaned, winsorized and validated exactly like columns read from the file.
  df <- derive_pcc_columns(df)

  prime_df_for_config_cache(df)
  df <- preprocess_data(df)
  df <- compute_optional_columns(df)

  # Last, so a user-defined expression sees every standard column (`t_stat`,
  # `precision`, `study_size`) and the winsorized `effect`/`se`: an interaction
  # with the standard error then uses the same values the analysis does.
  apply_derived_columns(df)
}

compute_data <- cache_cli_runner(
  compute_data_impl,
  stage = "prepare_data",
  key_builder = function(...) build_data_cache_signature()
)

#' @title Compute the unwinsorized variant of the prepared data
#' @description The compute phase again, with winsorization switched off, for
#'   runtime methods that declare `winsorize = FALSE` (see
#'   `register_runtime_method()`). Everything else about the frame (cleaning,
#'   missing-value handling, subset conditions, derived columns) is identical,
#'   so the result has the same rows and columns as `prepare_data()` returned,
#'   with `effect`, `se` and the columns derived from them unclipped. Reads the
#'   raw frame primed by the preceding `prepare_data()` call; the cache keys
#'   on the option group, so the two variants never collide. Runs at most at
#'   warning verbosity: the winsorized pass already narrated the pipeline.
#' @return *[data.frame]* The prepared data frame without winsorization.
compute_unwinsorized_data <- function() {
  without_winsorization(compute_data())
}

#' @title Evaluate a compute step with winsorization switched off
#' @description Runs `expr` with the winsorization level pinned to `0` and the
#'   verbosity capped at warnings, so a second pass over the pipeline neither
#'   clips nor repeats the narration of the first. Shared by
#'   `compute_unwinsorized_data()` and the provided-data branch of
#'   `prepare_run_context()`.
#' @param expr *[any]* The expression to evaluate.
#' @return The value of `expr`.
without_winsorization <- function(expr) {
  box::use(artma / libs / core / utils[get_verbosity])

  withr::with_options(
    list(
      artma.data.winsorization_level = 0,
      artma.verbose = min(get_verbosity(), 2)
    ),
    expr
  )
}

#' @title Persist phase
#' @description Idempotent, always-run phase. Registers the computed columns in
#'   the data config so they survive a warm-cache run (when compute is skipped).
#'   Running it again is a no-op because the entries already exist.
#' @param df *[data.frame]* The prepared data frame with computed columns.
#' @return `NULL`, invisibly.
#' @keywords internal
persist_data <- function(df) {
  box::use(artma / data / compute[update_config_with_computed_columns])

  update_config_with_computed_columns(df)

  invisible(NULL)
}

#' @title Prepare data
#' @description Prepare data for analysis. This includes reading, preprocessing,
#'   cleaning, and validating the data. Orchestrates the configure, compute, and
#'   persist phases. Only the compute phase is cached; the configure and persist
#'   side effects always run, warm or cold cache.
#' @param methods *[character, optional]* The methods requested for this run,
#'   as passed to `artma()`. Narrows the raw columns the data pipeline hard-
#'   requires to just what the compute stage and these methods actually need
#'   (see `artma / data / method_requirements[resolve_hard_required_colnames]`).
#'   Defaults to `NULL`, which keeps the full historical required set (used
#'   when the method selection is not yet resolved, e.g. an interactive menu).
#' @return *[data.frame]* The prepared data frame.
prepare_data <- function(methods = NULL) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data / read[read_data]
  )

  if (get_verbosity() >= 4) {
    cli::cli_inform("Preparing data for analysis.")
  }

  # Read raw data once and share it with both the configure and compute phases.
  df_raw <- read_data()
  prime_raw_df(df_raw, methods)

  configure_data(df_raw, methods) # interactive, uncached
  df <- compute_data() # pure, cached
  persist_data(df) # idempotent, always runs

  df
}


# Re-export useful functions for external use
box::use(
  artma / data / read[read_data],
  artma / data / column_recognition[
    recognize_columns,
    get_required_column_names
  ],
  artma / data / interactive_mapping[
    column_mapping_workflow,
    interactive_column_mapping
  ],
  artma / data / smart_detection[
    detect_delimiter,
    smart_read_csv
  ]
)

box::export(
  prepare_data,
  configure_data, # phases exported so tests can drive them in isolation
  compute_data_impl,
  compute_unwinsorized_data,
  without_winsorization,
  persist_data,
  prime_raw_df, # lets tests seed the raw frame the cached compute reads
  read_data,
  recognize_columns,
  get_required_column_names,
  column_mapping_workflow,
  interactive_column_mapping,
  detect_delimiter,
  smart_read_csv
)
