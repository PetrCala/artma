#' @title Data Config Resolution Module
#' @description Provides caching and resolution logic for the two-layer
#'   data config system. The base config (auto-generated from the dataframe)
#'   is merged with sparse overrides (from the options file) to produce
#'   the fully-resolved config that consumers expect.

# Module-level cache for the dataframe. Cache validity is keyed on the source
# path, the source file modification time, and the column name mapping used
# to standardize the dataframe.
.cache_env <- new.env(parent = emptyenv())

#' @title Get Source File Modification Time
#' @description Returns the modification time of a file as a numeric value,
#'   or `NA_real_` when the path is unset or the file cannot be inspected.
#' @param path *\[character\]* Path to the source file.
#' @return *\[numeric\]* The modification time, or `NA_real_`.
#' @keywords internal
get_source_mtime <- function(path) {
  if (is.null(path) || length(path) != 1 || is.na(path)) {
    return(NA_real_)
  }
  mtime <- tryCatch(file.mtime(path), error = function(e) NA)
  if (length(mtime) != 1 || is.na(mtime)) {
    return(NA_real_)
  }
  as.numeric(mtime)
}

#' @title Get Current Column Names Map
#' @description Reads the current column name mapping from the options
#'   namespace. Used as part of the cache key so that a changed mapping
#'   forces re-standardization.
#' @return *\[list\]* The current column names map (possibly empty).
#' @keywords internal
get_current_colnames_map <- function() {
  box::use(artma / options / utils[get_option_group])

  tryCatch(
    get_option_group("artma.data.colnames"),
    error = function(e) list()
  )
}

#' @title Standardize a Dataframe for Config Resolution
#' @description Applies the same column name standardization the data pipeline
#'   uses, so config keys are canonical no matter which entry point resolves
#'   the config first. When no complete column mapping is available (e.g., the
#'   options file has not been configured yet), falls back to syntactic names
#'   via `make.names`.
#' @param df *\[data.frame\]* The dataframe to standardize.
#' @return *\[data.frame\]* The dataframe with standardized column names.
#' @keywords internal
standardize_df_for_config <- function(df) {
  box::use(
    artma / data / utils[standardize_column_names],
    artma / libs / core / utils[get_verbosity]
  )

  tryCatch(
    standardize_column_names(df, auto_detect = FALSE),
    error = function(e) {
      if (get_verbosity() >= 4) {
        cli::cli_inform(
          "Could not standardize column names for config resolution ({e$message}); using syntactic column names instead."
        )
      }
      names(df) <- make.names(names(df))
      df
    }
  )
}

#' @title Prime Dataframe Cache for Config Resolution
#' @description Stores a dataframe and source path in the module cache so
#'   subsequent config resolution can reuse an already-read dataframe. The
#'   dataframe is expected to have standardized column names (as produced by
#'   the data pipeline).
#' @param df *\[data.frame\]* The dataframe to cache.
#' @param df_path *\[character, optional\]* Source path associated with the
#'   dataframe. Defaults to `getOption("artma.data.source_path")`.
#' @return `NULL`
prime_df_for_config_cache <- function(
  df,
  df_path = getOption("artma.data.source_path")
) {
  box::use(artma / libs / core / validation[validate])

  validate(is.data.frame(df))

  .cache_env$cached_df <- df
  .cache_env$cached_path <- df_path
  .cache_env$cached_mtime <- get_source_mtime(df_path)
  .cache_env$cached_colnames_map <- get_current_colnames_map()

  invisible(NULL)
}

#' @title Read Dataframe for Config Resolution
#' @description Reads the dataframe from `artma.data.source_path` and
#'   standardizes its column names, caching the result in memory to avoid
#'   repeated disk reads within a session. The cache is invalidated when the
#'   source path changes, when the source file is modified, or when the
#'   column name mapping changes.
#' @return *\[data.frame\]* The cached or freshly-read dataframe with
#'   standardized column names.
read_df_for_config <- function() {
  box::use(artma / data / read[read_data])

  df_path <- getOption("artma.data.source_path")
  if (is.null(df_path) || (length(df_path) == 1 && is.na(df_path))) {
    cli::cli_abort("Cannot resolve data config: {.code artma.data.source_path} is not set.")
  }

  current_mtime <- get_source_mtime(df_path)
  current_map <- get_current_colnames_map()

  cache_is_valid <- !is.null(.cache_env$cached_df) &&
    identical(.cache_env$cached_path, df_path) &&
    identical(.cache_env$cached_mtime, current_mtime) &&
    identical(.cache_env$cached_colnames_map, current_map)

  if (cache_is_valid) {
    return(.cache_env$cached_df)
  }

  df <- standardize_df_for_config(read_data(df_path))
  .cache_env$cached_df <- df
  .cache_env$cached_path <- df_path
  .cache_env$cached_mtime <- current_mtime
  .cache_env$cached_colnames_map <- current_map
  df
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
      # Ensure var_name is present -- extract_overrides() strips it from
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
  merge_config
)
