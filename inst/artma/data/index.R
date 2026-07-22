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
# hashed data frame content.
.raw_env <- new.env(parent = emptyenv())

#' @title Prime the raw data frame for the compute phase
#' @description Store the raw data frame that `compute_data_impl()` reads. Called
#'   by `prepare_data()` after its single disk read; also usable by tests that
#'   drive the compute phase in isolation.
#' @param df_raw *[data.frame]* Raw data frame with original column names.
#' @return `NULL`, invisibly.
#' @keywords internal
prime_raw_df <- function(df_raw) {
  .raw_env$df_raw <- df_raw
  invisible(NULL)
}

#' @title Configure phase
#' @description Interactive, uncached phase. Resolves any schema drift and
#'   decides the missing-value and zero-standard-error handling strategies
#'   before the cached compute phase runs, so compute never prompts and its
#'   cache key is stable across runs.
#' @param df_raw *[data.frame]* Raw data frame with original column names.
#' @return `NULL`, invisibly.
#' @keywords internal
configure_data <- function(df_raw) {
  box::use(
    artma / data / utils[standardize_column_names],
    artma / data / schema_reconcile[reconcile_schema],
    artma / data / preprocess[clean_data, resolve_na_handling, resolve_se_zero_handling]
  )

  # Detect and resolve any schema drift before column standardization. This may
  # prompt and updates both in-memory and persisted config.
  mode <- getOption("artma.data.reconcile_mode", "ask")
  reconcile_schema(df_raw, mode = mode)

  # Decide the missing-value and zero-SE strategies on the cleaned, standardized
  # frame so the compute phase can handle both without prompting.
  df_std <- standardize_column_names(df_raw, auto_detect = FALSE)
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
    artma / data_config / resolve[prime_df_for_config_cache],
    artma / data / preprocess[preprocess_data],
    artma / data / compute[compute_optional_columns]
  )

  df_raw <- .raw_env$df_raw

  # Apply colnames map (now updated if drift was reconciled in configure)
  df <- standardize_column_names(df_raw, auto_detect = FALSE)

  prime_df_for_config_cache(df)
  df <- preprocess_data(df)
  compute_optional_columns(df)
}

compute_data <- cache_cli_runner(
  compute_data_impl,
  stage = "prepare_data",
  key_builder = function(...) build_data_cache_signature()
)

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
#' @return *[data.frame]* The prepared data frame.
prepare_data <- function() {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data / read[read_data]
  )

  if (get_verbosity() >= 4) {
    cli::cli_inform("Preparing data for analysis.")
  }

  # Read raw data once and share it with both the configure and compute phases.
  df_raw <- read_data()
  prime_raw_df(df_raw)

  configure_data(df_raw) # interactive, uncached
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
