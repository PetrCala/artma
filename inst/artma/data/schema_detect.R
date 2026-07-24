#' @title Schema Drift Detection
#' @description Pure detection layer for schema reconciliation. Compares the
#'   user's dataset columns against the unified per-column store
#'   (`data.columns`) and proposes renames via the shared column-matching
#'   engine. Contains no prompts and no persistence, so it is unit-testable in
#'   isolation.

#' @title Check that a value is a single usable column name
#' @keywords internal
is_valid_colname <- function(x) {
  !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

#' @title Detect schema drift
#' @description Compares the current dataframe columns against the unified
#'   per-column store to identify renames, removals, and additions.
#' @param raw_df *\[data.frame\]* Raw dataframe with original column names.
#' @param columns_store *\[list\]* The unified per-column store
#'   (from `artma.data.columns`): one record per column, keyed by the standard
#'   name for role columns and by the column's own name for moderators.
#' @return *\[list\]* Drift report with fields: `missing_roles` (named character
#'   vector, names = standard names, values = the stored source columns that
#'   vanished), `missing_moderators`, `added`, `conflicts` (named character
#'   vector, names = standard names whose non-identity mapping collides with a
#'   different raw column of the same name, values = the mapped source
#'   columns), and `has_drift`.
#' @keywords internal
detect_schema_drift <- function(raw_df, columns_store) {
  box::use(
    artma / data / utils[get_required_colnames, get_standardized_colnames],
    artma / const[CONST]
  )

  df_cols <- make.names(colnames(raw_df))
  std_names <- get_standardized_colnames()
  required <- get_required_colnames()

  if (!is.list(columns_store)) columns_store <- list()
  store_keys <- names(columns_store)

  entry_is_computed <- function(entry) is.list(entry) && isTRUE(entry[["is_computed"]])

  # --- Role columns (standard names) ---
  # A role's stored source is its record's source_name; required roles with no
  # stored source default to the identity mapping (the standard name itself).
  role_sources <- list()
  for (std in std_names) {
    entry <- columns_store[[std]]
    if (entry_is_computed(entry)) next
    src <- if (is.list(entry)) entry[["source_name"]] else NULL
    if (is_valid_colname(src)) {
      role_sources[[std]] <- src
    } else if (std %in% required) {
      role_sources[[std]] <- std
    }
  }

  role_values <- vapply(role_sources, function(src) make.names(src), character(1))
  missing_role_std <- names(role_values)[!role_values %in% df_cols]
  missing_roles <- vapply(role_sources[missing_role_std], identity, character(1))

  # --- Mapping conflicts ---
  # A role mapped (non-identity) to a source column collides when the raw data
  # also contains a *different* column named exactly like the standard name.
  # Renaming the source would then produce two columns sharing that name, so
  # `standardize_column_names()` aborts. Flag it here so reconciliation can
  # resolve it up front. Byte-identical occupants are excluded (the pipeline
  # drops those quietly), as are conflicts the user already resolved via
  # `drop_conflicting_raw`.
  conflicts <- character(0)
  raw_df_cols <- make.names(colnames(raw_df))
  for (std in names(role_values)) {
    src_norm <- role_values[[std]]
    if (identical(src_norm, std)) next
    if (!std %in% df_cols || !src_norm %in% df_cols) next

    entry <- columns_store[[std]]
    if (is.list(entry) && isTRUE(entry[["drop_conflicting_raw"]])) next

    src_values <- raw_df[[which(raw_df_cols == src_norm)[[1]]]]
    std_values <- raw_df[[which(raw_df_cols == std)[[1]]]]
    if (identical(src_values, std_values)) next

    conflicts[[std]] <- role_sources[[std]]
  }

  # --- Moderator columns (non-role record keys) ---
  # Computed columns are added by the pipeline, not by the user's data, so they
  # will never be present in the raw df and must not be flagged as missing.
  moderator_keys <- store_keys[
    !store_keys %in% std_names &
      !vapply(columns_store, entry_is_computed, logical(1))
  ]
  moderator_keys_norm <- stats::setNames(make.names(moderator_keys), moderator_keys)
  missing_moderators <- names(moderator_keys_norm)[!moderator_keys_norm %in% df_cols]

  # --- Added columns (in df but not referenced by anything) ---
  # Standard names and computed columns count as referenced so that columns the
  # system already knows about do not appear as "new".
  referenced <- unique(make.names(c(
    unname(role_values),
    store_keys,
    std_names,
    CONST$DATA$COMPUTED_COLNAMES
  )))
  added <- df_cols[!df_cols %in% referenced]

  list(
    missing_roles = missing_roles, # named: std name -> stored source column
    missing_moderators = missing_moderators,
    added = added,
    conflicts = conflicts, # named: std name -> mapped source column
    has_drift = (
      length(missing_roles) > 0 ||
        length(missing_moderators) > 0 ||
        length(added) > 0 ||
        length(conflicts) > 0
    )
  )
}

#' @title Propose renames via the shared column-matching engine
#' @description For each missing column, finds the best candidate from the
#'   available (unmatched) columns using the recognition engine from
#'   `column_recognition.R`: string similarity plus, when the standard column
#'   is known, the pattern/value-analysis signal.
#' @param missing *\[character\]* Named vector: names identify the record (a
#'   standard name for roles, the column's own name for moderators), values are
#'   the stored column names that are now missing from the dataframe.
#' @param available_cols *\[character\]* Unmatched columns from the raw df.
#' @param raw_df *\[data.frame, optional\]* The raw dataframe, for value analysis.
#' @param roles_known *\[logical\]* Whether the names of `missing` are standard
#'   column names (enables the pattern signal).
#' @return *\[list\]* Named by the record key. Each element:
#'   `list(candidate, score)`.
#' @keywords internal
propose_renames <- function(missing, available_cols, raw_df = NULL, roles_known = FALSE) {
  box::use(
    artma / data / column_recognition[MATCH_THRESHOLDS, score_rename_candidate]
  )

  if (length(missing) == 0 || length(available_cols) == 0) {
    return(stats::setNames(list(), character(0)))
  }

  proposals <- list()

  for (i in seq_along(missing)) {
    record_key <- names(missing)[[i]]
    stored_name <- missing[[i]]
    std_name <- if (roles_known) record_key else NULL

    best_score <- 0
    best_candidate <- NA_character_

    for (cand in available_cols) {
      score <- score_rename_candidate(
        stored_name = stored_name,
        candidate = cand,
        std_name = std_name,
        df = raw_df
      )
      if (score > best_score) {
        best_score <- score
        best_candidate <- cand
      }
    }

    proposals[[record_key]] <- list(
      candidate = if (best_score >= MATCH_THRESHOLDS$rename_suggest) best_candidate else NA_character_,
      score = best_score
    )
  }

  proposals
}

box::export(detect_schema_drift, propose_renames)
