box::use(
  artma / options / index[get_option_group],
  artma / libs / infrastructure / source_fingerprint[package_source_fingerprint]
)

# Option keys under the "artma." prefix that the data pipeline itself writes
# while `prepare_data()` runs. They must never feed the cache signature:
# including them would let the first run rewrite its own cache key, so a
# second identical run could never hit the cache.
#
# - "data.columns": `update_config_with_computed_columns()` appends computed
#   column entries during the run. User-authored entries still feed the
#   signature; see `user_authored_config_entries()`.
# - "data.expected_schema_columns": the schema baseline persisted by
#   `persist_expected_schema_cols()` during schema reconciliation.
# - "data.source_path": runtime-populated; captured separately as a
#   normalized path plus the file modification time.
PIPELINE_WRITTEN_OPTION_KEYS <- c(
  "data.columns",
  "data.expected_schema_columns",
  "data.source_path"
)

# Prefixes of runtime session bookkeeping keys (never user-authored), e.g.
# "temp.file_name" and "temp.dir_name".
RUNTIME_OPTION_PREFIXES <- c(
  "temp."
)

#' @title Sort a named list by name
#' @description Byte-order sorting keeps the signature independent of the
#'   order in which options were set and of the session locale. Empty inputs
#'   collapse to a canonical `list()` so that an empty group hashes the same
#'   whether it arrived as an unnamed `list()` or a named list emptied by
#'   filtering (e.g. a data config holding only pipeline-written entries). This
#'   keeps the compute cache key stable across runs.
#' @keywords internal
sort_by_name <- function(x) {
  if (length(x) == 0L) {
    return(list())
  }
  nms <- names(x)
  if (is.null(nms)) {
    return(x)
  }
  x[order(nms, method = "radix")]
}

#' @title Drop options written by the pipeline itself
#' @description Filters a flat option group (names relative to the "artma."
#'   prefix) down to user-authored entries by removing the keys listed in
#'   `PIPELINE_WRITTEN_OPTION_KEYS` and the prefixes in
#'   `RUNTIME_OPTION_PREFIXES`.
#' @keywords internal
drop_pipeline_written_options <- function(opts) {
  if (!is.list(opts) || length(opts) == 0L) {
    return(list())
  }

  keys <- names(opts)
  drop <- keys %in% PIPELINE_WRITTEN_OPTION_KEYS
  for (prefix in RUNTIME_OPTION_PREFIXES) {
    drop <- drop | startsWith(keys, prefix)
  }

  sort_by_name(opts[!drop])
}

#' @title Keep user-authored data config entries
#' @description Entries flagged `is_computed = TRUE` are appended to the data
#'   config by the pipeline itself and are excluded so they cannot invalidate
#'   the cache key of the run that produced them.
#' @keywords internal
user_authored_config_entries <- function(config) {
  if (!is.list(config) || length(config) == 0L) {
    return(list())
  }

  computed <- vapply(
    config,
    function(entry) is.list(entry) && isTRUE(entry$is_computed),
    logical(1)
  )

  sort_by_name(config[!computed])
}

#' @title Build cache signature for data-dependent workflows
#' @description
#' Construct a deterministic list of the user-controlled inputs that influence
#' data preparation: the configured data source (normalized path plus file
#' modification time), the user-authored part of the data config, the
#' remaining user-authored `artma.*` options, the installed package version,
#' and a fingerprint of the package's own source tree. The list is forwarded to
#' `cache_cli()` wrappers as the `cache_signature` argument and `memoise`
#' hashes it as part of the cache key, so no explicit hashing happens here.
#'
#' The `artma.*` options are taken as a whole group rather than as a
#' per-consumer subset, so an option a workflow reads indirectly (through a
#' shared formatter or calculation helper) cannot be missed. That is coarser
#' than necessary, which is the safe direction: a needless recomputation costs
#' time, a stale hit costs correctness.
#'
#' The source fingerprint covers every R file under the package's module root,
#' so editing a method (or anything it calls) invalidates the caches built from
#' it. Callers that key on a single method add its own source hash on top; see
#' `register_runtime_method()`.
#'
#' Options that the pipeline itself writes while running (computed column
#' entries in `data.columns`, the `data.expected_schema_columns` baseline, and
#' `temp.*` session bookkeeping) are excluded. Hashing them would make run 1
#' change its own signature, so an identical run 2 would always miss the
#' cache.
#' @return *\[list\]* A deterministic signature list suitable for
#'   forwarding to `cache_cli()` wrappers as a `cache_signature` argument.
build_data_cache_signature <- function() {
  source_path <- getOption("artma.data.source_path", NULL)
  normalized_path <- NULL
  source_mtime <- NA_real_

  if (!is.null(source_path)) {
    normalized_path <- tryCatch(
      normalizePath(source_path, mustWork = FALSE),
      error = function(err) source_path
    )

    file_info <- tryCatch(file.info(normalized_path), error = function(err) NULL)
    if (!is.null(file_info) && nrow(file_info) == 1) {
      mtime <- file_info$mtime
      if (!is.na(mtime)) {
        source_mtime <- unclass(mtime)
      }
    }
  }

  data_config <- user_authored_config_entries(getOption("artma.data.columns", NULL))
  artma_options <- drop_pipeline_written_options(get_option_group("artma"))

  list(
    source_path = normalized_path,
    source_mtime = source_mtime,
    data_config = data_config,
    artma_options = artma_options,
    package_version = as.character(utils::packageVersion("artma")),
    source_fingerprint = package_source_fingerprint()
  )
}

box::export(build_data_cache_signature)
