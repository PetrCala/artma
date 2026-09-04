#' @title Schema Drift Detection and Reconciliation
#' @description Thin orchestrator that wires together the pure detection layer
#'   (`schema_detect.R`), the interactive UI (`schema_ui.R`), and the
#'   persistence layer (`schema_persist.R`). Detects changes between the user's
#'   dataset columns and the unified per-column store (`data.columns`), then
#'   guides the user through resolving those changes before the analysis
#'   pipeline runs. All drift is diffed against, and applied to, the single
#'   per-column store.

box::use(
  artma / data / schema_detect[detect_schema_drift, propose_renames],
  artma / data / schema_persist[
    apply_reconciliation,
    get_columns_store,
    normalize_expected_schema_cols,
    persist_expected_schema_cols
  ],
  artma / data / schema_ui[
    ask_decisions,
    auto_decisions,
    confirm_decisions,
    proposal_is_auto,
    show_drift_summary
  ]
)

#' @title Emit schema reconciliation completion message
#' @keywords internal
emit_reconcile_complete <- function() {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 3) {
    cli::cli_alert_success("Schema reconciliation complete.")
  }
}

#' @title Columns a set of proposals lays claim to
#' @keywords internal
proposed_candidates <- function(proposals) {
  candidates <- unlist(lapply(proposals, function(prop) prop$candidate), use.names = FALSE)
  candidates[!is.na(candidates)]
}

#' @title Reconcile schema drift
#' @description Detects changes between the current dataset columns and the
#'   unified per-column store, then resolves them before the analysis pipeline
#'   runs. Should be called with the raw (un-standardized) dataframe.
#' @param raw_df *\[data.frame\]* Raw dataframe with original column names.
#' @param mode *\[character\]* One of `"ask"`, `"auto"`, or `"strict"`. If `NULL`,
#'   reads from `artma.data.reconcile_mode` option (default: `"ask"`).
#' @param required_colnames *\[character, optional\]* The raw columns treated as
#'   required for this run. Defaults to `NULL`, which uses the full
#'   `get_required_colnames()` set (the historical behavior). Passed through
#'   to `detect_schema_drift()`; see `artma / data / method_requirements
#'   [resolve_hard_required_colnames]` for the run-specific, method-aware set.
#' @param is_interactive *\[logical\]* Whether the session is interactive.
#'   Defaults to `interactive()`. Injectable so both branches of the prompt gate
#'   are exercisable in headless tests; production callers should leave it at
#'   the default.
#' @param select_fn *\[function, optional\]* Menu backend for the interactive
#'   path, see `artma / data / schema_ui[ask_decisions]`. Defaults to `NULL`
#'   (`climenu::select`); injectable for tests.
#' @return `NULL` invisibly. Side effects: updates options file and in-memory
#'   state if drift is detected and resolved.
#' @keywords internal
reconcile_schema <- function(raw_df, mode = NULL, required_colnames = NULL, is_interactive = interactive(), select_fn = NULL) {
  box::use(
    artma / libs / core / autonomy[should_prompt_user],
    artma / libs / core / utils[get_verbosity],
    artma / data / utils[get_required_colnames]
  )

  mode <- mode %||% getOption("artma.data.reconcile_mode", "ask")
  if (is.null(required_colnames)) {
    required_colnames <- get_required_colnames()
  }
  current_schema_cols <- unique(make.names(colnames(raw_df)))
  expected_schema_cols <- normalize_expected_schema_cols(
    getOption("artma.data.expected_schema_columns", NA_character_)
  )

  first_run <- length(expected_schema_cols) == 0L

  columns_store <- get_columns_store()

  # Detect drift
  drift <- detect_schema_drift(raw_df, columns_store, required = required_colnames)

  if (first_run) {
    # No baseline schema yet, so moderator and "added" comparisons are
    # meaningless and get suppressed. Missing roles and mapping conflicts are
    # baseline-independent, though: a required role with neither a mapping nor
    # a matching raw column is just as broken on the first run as on the
    # tenth. Suppressing it here only defers the failure to
    # `standardize_column_names()`, which by design cannot offer a fix, so the
    # user's first-ever run got a flat abort while every later run got the
    # reconciliation UI. Let it through and resolve it here.
    drift$missing_moderators <- character(0)
    drift$added <- character(0)
  } else {
    # "Added" columns should only include columns that are new relative to the
    # stored baseline schema. Baseline columns that are simply not mapped
    # should not be treated as drift on every run.
    drift$added <- setdiff(drift$added, expected_schema_cols)
  }

  drift_fields <- c("missing_roles", "missing_optional_roles", "missing_moderators", "added", "conflicts")
  drift$has_drift <- any(lengths(drift[drift_fields]) > 0)

  if (!drift$has_drift) {
    if (first_run) persist_expected_schema_cols(current_schema_cols)
    emit_reconcile_complete()
    return(invisible(NULL))
  }

  # Strict mode: abort with a structured message
  if (mode == "strict") {
    msgs <- c("x" = "Dataset schema does not match the stored configuration.")
    if (length(drift$missing_roles) > 0) {
      msgs <- c(msgs, "i" = cli::format_inline(
        "Missing required column{?s}: {.val {unname(drift$missing_roles)}}"
      ))
    }
    if (length(drift$missing_optional_roles) > 0) {
      msgs <- c(msgs, "i" = cli::format_inline(
        "Missing mapped optional column{?s}: {.val {unname(drift$missing_optional_roles)}}"
      ))
    }
    if (length(drift$missing_moderators) > 0) {
      msgs <- c(msgs, "i" = cli::format_inline(
        "Missing moderator{?s}: {.val {drift$missing_moderators}}"
      ))
    }
    if (length(drift$added) > 0) {
      msgs <- c(msgs, "i" = cli::format_inline(
        "New column{?s} not in config: {.val {drift$added}}"
      ))
    }
    if (length(drift$conflicts) > 0) {
      conflict_pairs <- paste0(unname(drift$conflicts), " -> ", names(drift$conflicts))
      msgs <- c(msgs, "i" = cli::format_inline(
        "Mapping conflict{?s}: {.val {conflict_pairs}} while the data also contains a different raw column of the same standard name"
      ))
    }
    msgs <- c(msgs,
      "i" = "Set {.code data.reconcile_mode} to {.val ask} for interactive resolution, or to {.val auto} for automatic resolution."
    )
    cli::cli_abort(msgs)
  }

  # Stored role sources (for display) and "unmatched" columns available for
  # fuzzy matching: everything the store does not already account for.
  box::use(
    artma / data / utils[get_colnames_map, get_standardized_colnames]
  )
  missing_role_sources <- c(drift$missing_roles, drift$missing_optional_roles)
  role_sources <- as.list(missing_role_sources)
  matched_role_sources <- character(0)
  full_map <- get_colnames_map()
  # Required roles with no explicit record are tracked as identity mappings
  for (std in setdiff(required_colnames, names(full_map))) {
    full_map[[std]] <- std
  }
  for (std in names(full_map)) {
    if (!std %in% names(missing_role_sources)) {
      role_sources[[std]] <- full_map[[std]]
      matched_role_sources <- c(matched_role_sources, full_map[[std]])
    }
  }

  moderator_keys <- setdiff(names(columns_store), get_standardized_colnames())
  matched_cols <- make.names(c(matched_role_sources, moderator_keys))
  matched_cols <- setdiff(matched_cols, make.names(unname(missing_role_sources)))
  available_cols <- setdiff(make.names(colnames(raw_df)), matched_cols)

  # Rename proposals via the shared matching engine. Each pass is exclusive
  # within itself, and the passes run in priority order (required roles,
  # optional roles, moderators) with earlier winners withheld from later
  # passes, so no column is ever proposed to two records.
  proposals_roles <- propose_renames(
    drift$missing_roles, available_cols,
    raw_df = raw_df, roles_known = TRUE
  )
  available_cols <- setdiff(available_cols, proposed_candidates(proposals_roles))
  proposals_optional <- propose_renames(
    drift$missing_optional_roles, available_cols,
    raw_df = raw_df, roles_known = TRUE
  )
  available_cols <- setdiff(available_cols, proposed_candidates(proposals_optional))
  proposals_moderators <- propose_renames(
    stats::setNames(drift$missing_moderators, drift$missing_moderators),
    available_cols,
    raw_df = raw_df
  )

  # Show unified diff. Unlike emit_reconcile_complete()'s plain success note,
  # this is drift detail worth a "Warnings + errors" gate rather than always
  # printing - it was previously ungated, unlike every sibling cli call in
  # this module.
  if (get_verbosity() >= 2) {
    show_drift_summary(drift, proposals_roles, proposals_optional, proposals_moderators, role_sources)
  }

  # Collect decisions. A required role with no auto-acceptable candidate cannot
  # be resolved without asking, and `auto_decisions()` would abort on it. In an
  # interactive session that is worth a prompt regardless of autonomy level:
  # the alternative is failing the run over a question the user is sitting
  # right there to answer. This mirrors `interactive_column_mapping()`, which
  # already falls through to its prompts for missing required columns at the
  # autonomous level. Optional and moderator decisions stay gated as before.
  unresolvable_roles <- Filter(
    function(std) !proposal_is_auto(proposals_roles[[std]]),
    names(drift$missing_roles)
  )

  do_prompt <- (mode == "ask") && isTRUE(is_interactive) && (
    should_prompt_user(required_level = "autonomous", is_interactive = is_interactive) ||
      length(unresolvable_roles) > 0
  )

  if (mode == "auto" || !do_prompt) {
    decisions <- auto_decisions(drift, proposals_roles, proposals_optional, proposals_moderators)
  } else {
    decisions <- ask_decisions(
      drift, proposals_roles, proposals_optional, proposals_moderators, raw_df,
      select_fn = select_fn
    )
    confirm_decisions(decisions, drift, role_sources, select_fn = select_fn)
  }

  # Apply through the single write path
  apply_reconciliation(decisions)
  persist_expected_schema_cols(current_schema_cols)

  emit_reconcile_complete()

  invisible(NULL)
}

box::export(reconcile_schema, detect_schema_drift)
