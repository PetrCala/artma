#' @title Resolve the raw columns the current run cannot proceed without
#' @description
#' `standardize_column_names()` hard-aborts when a hard-required raw column is
#' absent from the data and has no configured mapping. Historically that set
#' was the fixed `CONST$DATA$REQUIRED_COLNAMES` (`study_id`, `effect`, `se`,
#' `n_obs`), regardless of which methods were actually requested, so a dataset
#' missing only `n_obs` aborted even when every requested method (for example
#' `effect_summary_stats`) never touches it.
#'
#' This resolves a narrower, run-specific hard-required set instead:
#'
#' * `study_id`, `effect`, and `se` stay unconditionally required. The compute
#'   stage (`compute_optional_columns()`) always derives `t_stat` from
#'   `effect`/`se` and normalizes `study_id`/`study_size` from `study_id`,
#'   regardless of which methods run, so their absence breaks data preparation
#'   itself rather than any one method.
#' * `n_obs` is optional-by-default: `add_reg_dof_column()` and
#'   `add_precision_column()` degrade to `NA` when it is missing rather than
#'   aborting. It only re-joins the hard-required set when a requested method
#'   declares a genuine need for it, directly (`n_obs`, `reg_dof`) or
#'   indirectly (`precision`, but only when `calc.precision_type` is `"DoF"`;
#'   the default `"1/SE"` never touches `n_obs`).
#'
#' `methods` must be a concrete, already-resolved selection to narrow
#' anything. `NULL` (interactive selection deferred) and `"all"` fall back to
#' the full historical set, since narrowing either would either be impossible
#' (the selection is not known yet) or pointless (every non-opt-in method,
#' including the ones that need `n_obs`, may run). Unknown method names are
#' likewise ignored here; `invoke_runtime_methods()` reports those.
#'
#' This is called from the uncached configure phase of the data pipeline
#' (`configure_data()`), which runs on every `prepare_data()` call regardless
#' of whether the cached compute phase hits or misses. That keeps the abort
#' decision independent of cache warmth: a method list that newly needs
#' `n_obs` re-evaluates and aborts even when the underlying data frame would
#' otherwise be served from cache.
#' @param methods *\[character, optional\]* The methods requested for this run,
#'   as passed to `artma()`. `NULL` or `"all"` fall back to the full
#'   historical required set.
#' @param modules_dir *\[character, optional\]* Directory to discover runtime
#'   method modules in. Defaults to `NULL`, in which case the standard package
#'   methods directory is used. Mainly for dependency injection in tests.
#' @return *\[character\]* The raw columns this run cannot proceed without.
resolve_hard_required_colnames <- function(methods = NULL, modules_dir = NULL) {
  box::use(
    artma / const[CONST],
    artma / data / derivation[pcc_derivation_active],
    artma / modules / runtime_methods[get_method_metadata, get_runtime_method_modules],
    artma / options / typed_accessors[get_precision_type]
  )

  # With the (t, df) route configured, `effect` and `se` are produced by
  # `derive_pcc_columns()` rather than read, so the raw file must carry the two
  # inputs instead of the two outputs.
  apply_derivation <- function(required) {
    if (!pcc_derivation_active()) {
      return(required)
    }
    union(setdiff(required, c("effect", "se")), c("t_stat", "reg_dof"))
  }

  full_required <- apply_derivation(CONST$DATA$REQUIRED_COLNAMES)
  base_required <- apply_derivation(c("study_id", "effect", "se"))

  if (is.factor(methods)) {
    methods <- as.character(methods)
  }
  if (!is.character(methods) || length(methods) == 0L) {
    # NULL, or any other non-character input: the selection is not yet
    # resolved (interactive menu) or not usable here. Keep today's behavior.
    return(full_required)
  }

  methods <- unique(trimws(methods[!is.na(methods)]))
  methods <- methods[nzchar(methods)]

  if (length(methods) == 0L || identical(methods, "all")) {
    # "all" may run every non-opt-in method, including ones that need n_obs.
    return(full_required)
  }

  modules <- tryCatch(
    if (is.null(modules_dir)) get_runtime_method_modules() else get_runtime_method_modules(modules_dir = modules_dir),
    error = function(e) NULL
  )
  if (is.null(modules)) {
    return(full_required)
  }

  known_methods <- intersect(methods, names(modules))
  if (length(known_methods) == 0L) {
    # Nothing recognized: let invoke_runtime_methods() report the real error.
    return(full_required)
  }

  declared_columns <- unique(unlist(lapply(known_methods, function(name) {
    get_method_metadata(modules[[name]]$run, name = name)$required_columns
  })))

  needs_n_obs <- any(c("n_obs", "reg_dof") %in% declared_columns) ||
    ("precision" %in% declared_columns && identical(get_precision_type(), "DoF"))

  if (needs_n_obs) union(base_required, "n_obs") else base_required
}

box::export(resolve_hard_required_colnames)
