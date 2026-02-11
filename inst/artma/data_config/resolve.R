#' @title Data Config Resolution Module
#' @description Provides caching and resolution logic for the two-layer
#'   data config system. The base config (auto-generated from the dataframe)
#'   is merged with sparse overrides (from the options file) to produce
#'   the fully-resolved config that consumers expect.

# Module-level cache for the dataframe only (keyed by source path)
.cache_env <- new.env(parent = emptyenv())

#' @title Prime Dataframe Cache for Config Resolution
#' @description Stores a dataframe and source path in the module cache so
#'   subsequent config resolution can reuse an already-read dataframe.
#' @param df *\[data.frame\]* The dataframe to cache.
#' @param df_path *\[character, optional\]* Source path associated with the
#'   dataframe. Defaults to `getOption("artma.data.source_path")`.
#' @return `NULL`
prime_df_for_config_cache <- function(
    df,
    df_path = getOption("artma.data.source_path")) {
  box::use(artma / libs / core / validation[validate])

  validate(is.data.frame(df))

  .cache_env$cached_df <- df
  .cache_env$cached_path <- df_path

  invisible(NULL)
}

#' @title Read Dataframe for Config Resolution
#' @description Reads the dataframe from `artma.data.source_path`, caching it
#'   in memory to avoid repeated disk reads within a session. The cache is
#'   invalidated when the source path changes.
#' @return *\[data.frame\]* The cached or freshly-read dataframe
read_df_for_config <- function() {
  box::use(artma / data / read[read_data])

  df_path <- getOption("artma.data.source_path")
  if (is.null(df_path) || (length(df_path) == 1 && is.na(df_path))) {
    cli::cli_abort("Cannot resolve data config: {.code artma.data.source_path} is not set.")
  }

  # Check if cached df is still valid (same source path)
  if (!is.null(.cache_env$cached_df) && identical(.cache_env$cached_path, df_path)) {
    return(.cache_env$cached_df)
  }

  df <- read_data(df_path)
  .cache_env$cached_df <- df
  .cache_env$cached_path <- df_path
  df
}

#' @title Invalidate DataFrame Cache
#' @description Clears the cached dataframe, forcing re-read on next access.
invalidate_df_cache <- function() {
  .cache_env$cached_df <- NULL
  .cache_env$cached_path <- NULL
}

#' @title Merge Sparse Overrides onto Base Config
#' @description Deep-merges sparse overrides into a base config. For each
#'   variable in overrides, its fields are applied on top of the corresponding
#'   base entry via `modifyList`.
#' @param base *\[list\]* The base config (fully populated defaults)
#' @param overrides *\[list\]* The sparse overrides (only non-default fields)
#' @return *\[list\]* The merged config
merge_config <- function(base, overrides) {
  if (!is.list(overrides) || length(overrides) == 0) {
    return(base)
  }

  for (var_key in names(overrides)) {
    override_entry <- overrides[[var_key]]
    if (!is.list(override_entry)) next

    if (var_key %in% names(base)) {
      # Existing variable: overlay specific fields
      base[[var_key]] <- utils::modifyList(base[[var_key]], override_entry)
    } else {
      # Variable not in base (e.g., removed from df or manually added).
      # Ensure var_name is present — extract_overrides() strips it from
      # sparse overrides, so override-only entries may lack the field.
      if (is.null(override_entry$var_name)) {
        override_entry$var_name <- var_key
      }
      base[[var_key]] <- override_entry
    }
  }

  base
}

box::export(
  prime_df_for_config_cache,
  read_df_for_config,
  invalidate_df_cache,
  merge_config
)
