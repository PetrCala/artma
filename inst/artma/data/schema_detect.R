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
#' @param required *\[character, optional\]* The raw columns treated as required
#'   for identity-mapping and drift purposes. Defaults to `NULL`, which uses
#'   the full `get_required_colnames()` set (the historical behavior). Callers
#'   that know which methods are actually being run pass a narrower,
#'   run-specific set here; see `artma / data / method_requirements
#'   [resolve_hard_required_colnames]`. A role outside this set whose mapped
#'   source column goes missing is reported under `missing_optional_roles`
#'   instead of `missing_roles`, so it never forces schema reconciliation to
#'   abort: its mapping can simply be dropped.
#' @param derived *\[character, optional\]* Column names the compute phase will
#'   create from `data.derived` expressions. They are configured like any other
#'   moderator but do not exist in the raw data yet, so they must not be
#'   reported as missing. Defaults to `NULL`, which reads the names from the
#'   `data.derived` option.
#' @return *\[list\]* Drift report with fields: `missing_roles` (named character
#'   vector, names = standard names of required roles, values = the stored
#'   source columns that vanished), `missing_optional_roles` (same shape, for
#'   roles outside `required` that carry an explicit mapping),
#'   `missing_moderators`, `added`, `conflicts` (named character vector, names
#'   = standard names whose non-identity mapping collides with a different raw
#'   column of the same name, values = the mapped source columns), and
#'   `has_drift`.
#' @keywords internal
detect_schema_drift <- function(raw_df, columns_store, required = NULL, derived = NULL) {
  box::use(
    artma / data / utils[get_required_colnames, get_standardized_colnames],
    artma / data / derived_columns[derived_column_names],
    artma / const[CONST]
  )

  df_cols <- make.names(colnames(raw_df))
  std_names <- get_standardized_colnames()
  if (is.null(required)) {
    required <- get_required_colnames()
  }
  if (is.null(derived)) {
    derived <- derived_column_names()
  }
  derived_norm <- make.names(derived)

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
  missing_std <- names(role_values)[!role_values %in% df_cols]
  # A vanished source is only blocking when the role is required for this run.
  # An optional role (t_stat, or n_obs when no requested method needs it) that
  # was mapped by hand and then dropped from the file just loses its mapping.
  missing_required_std <- missing_std[missing_std %in% required]
  missing_optional_std <- setdiff(missing_std, missing_required_std)
  missing_roles <- vapply(role_sources[missing_required_std], identity, character(1))
  missing_optional_roles <- vapply(role_sources[missing_optional_std], identity, character(1))

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
  # `data.derived` columns are the user-defined case of the same thing: they are
  # created at the end of the compute phase, one step after reconciliation, so a
  # config entry naming one (typically to give it `bma: yes`) is not drift.
  moderator_keys <- store_keys[
    !store_keys %in% std_names &
      !make.names(store_keys) %in% derived_norm &
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
    derived,
    CONST$DATA$COMPUTED_COLNAMES
  )))
  added <- df_cols[!df_cols %in% referenced]

  list(
    missing_roles = missing_roles, # named: std name -> stored source column
    missing_optional_roles = missing_optional_roles, # same shape, non-required roles
    missing_moderators = missing_moderators,
    added = added,
    conflicts = conflicts, # named: std name -> mapped source column
    has_drift = (
      length(missing_roles) > 0 ||
        length(missing_optional_roles) > 0 ||
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
#'
#'   Assignment is exclusive: every candidate column is proposed to at most
#'   one missing column, awarded greedily from the highest-scoring pair down.
#'   Without this, two missing moderators could both be remapped onto the same
#'   new column and the second would silently overwrite the first's
#'   configuration. A proposal is also flagged `ambiguous` when a rival
#'   candidate, or a rival missing column claiming the same candidate, scores
#'   within `MATCH_THRESHOLDS$rename_tie_margin` of it; callers still show
#'   such a proposal but must not apply it without asking.
#' @param missing *\[character\]* Named vector: names identify the record (a
#'   standard name for roles, the column's own name for moderators), values are
#'   the stored column names that are now missing from the dataframe.
#' @param available_cols *\[character\]* Unmatched columns from the raw df.
#' @param raw_df *\[data.frame, optional\]* The raw dataframe, for value analysis.
#' @param roles_known *\[logical\]* Whether the names of `missing` are standard
#'   column names (enables the pattern signal).
#' @return *\[list\]* One element per record key, in the order of `missing`.
#'   Each element: `list(candidate, score, ambiguous, runner_up, contested)`.
#'   `candidate` is `NA` when nothing clears `rename_suggest` (or every
#'   acceptable candidate went to a higher-scoring record); `score` is then the
#'   record's best raw score, for messages. `runner_up` names the rival
#'   candidate behind a tie (`NA` otherwise) and `contested` says whether the
#'   tie came from another missing column wanting the same candidate.
#' @keywords internal
propose_renames <- function(missing, available_cols, raw_df = NULL, roles_known = FALSE) {
  box::use(
    artma / data / column_recognition[MATCH_THRESHOLDS, score_rename_candidate]
  )

  record_keys <- names(missing)
  if (length(missing) == 0) {
    return(stats::setNames(list(), character(0)))
  }

  no_candidate <- function(score) {
    list(
      candidate = NA_character_, score = score, ambiguous = FALSE,
      runner_up = NA_character_, contested = FALSE
    )
  }

  if (length(available_cols) == 0) {
    return(stats::setNames(lapply(record_keys, function(key) no_candidate(0)), record_keys))
  }

  scores <- matrix(
    0,
    nrow = length(record_keys), ncol = length(available_cols),
    dimnames = list(record_keys, available_cols)
  )
  for (i in seq_along(missing)) {
    std_name <- if (roles_known) record_keys[[i]] else NULL
    for (cand in available_cols) {
      scores[i, cand] <- score_rename_candidate(
        stored_name = missing[[i]],
        candidate = cand,
        std_name = std_name,
        df = raw_df
      )
    }
  }

  suggest <- MATCH_THRESHOLDS$rename_suggest
  margin <- MATCH_THRESHOLDS$rename_tie_margin

  proposals <- list()
  open_records <- record_keys
  open_cands <- available_cols

  while (length(open_records) > 0 && length(open_cands) > 0) {
    sub <- scores[open_records, open_cands, drop = FALSE]
    best <- max(sub)
    if (best < suggest) break

    hit <- which(sub == best, arr.ind = TRUE)[1, ]
    rec <- open_records[[hit[["row"]]]]
    cand <- open_cands[[hit[["col"]]]]

    rival_cands <- setdiff(open_cands, cand)
    rival_recs <- setdiff(open_records, rec)
    rival_cand_scores <- stats::setNames(sub[rec, rival_cands], rival_cands)
    rival_rec_scores <- stats::setNames(sub[rival_recs, cand], rival_recs)

    runner_up_score <- if (length(rival_cand_scores)) max(rival_cand_scores) else -Inf
    tied_candidate <- (best - runner_up_score) <= margin
    contested <- length(rival_rec_scores) > 0 && (best - max(rival_rec_scores)) <= margin

    proposals[[rec]] <- list(
      candidate = cand,
      score = best,
      ambiguous = tied_candidate || contested,
      runner_up = if (tied_candidate) names(rival_cand_scores)[[which.max(rival_cand_scores)]] else NA_character_,
      contested = contested
    )

    open_records <- rival_recs
    open_cands <- rival_cands
  }

  for (rec in setdiff(record_keys, names(proposals))) {
    proposals[[rec]] <- no_candidate(max(scores[rec, ]))
  }

  proposals[record_keys]
}

box::export(detect_schema_drift, propose_renames)
