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

#' @title Reconcile schema drift
#' @description Detects changes between the current dataset columns and the
#'   unified per-column store, then resolves them before the analysis pipeline
#'   runs. Should be called with the raw (un-standardized) dataframe.
#' @param raw_df *\[data.frame\]* Raw dataframe with original column names.
#' @param mode *\[character\]* One of `"ask"`, `"auto"`, or `"strict"`. If `NULL`,
#'   reads from `artma.data.reconcile_mode` option (default: `"ask"`).
#' @return `NULL` invisibly. Side effects: updates options file and in-memory
#'   state if drift is detected and resolved.
#' @keywords internal
reconcile_schema <- function(raw_df, mode = NULL) {
  box::use(
    artma / libs / core / autonomy[should_prompt_user]
  )

  mode <- mode %||% getOption("artma.data.reconcile_mode", "ask")
  current_schema_cols <- unique(make.names(colnames(raw_df)))
  expected_schema_cols <- normalize_expected_schema_cols(
    getOption("artma.data.expected_schema_columns", NA_character_)
  )

  first_run <- length(expected_schema_cols) == 0L

  columns_store <- get_columns_store()

  # Detect drift
  drift <- detect_schema_drift(raw_df, columns_store)

  if (first_run) {
    # No baseline schema yet: missing/added comparisons are meaningless, so
    # suppress them. Mapping conflicts are baseline-independent (an explicit
    # mapping colliding with a raw column) and stay actionable even now;
    # left unresolved they would abort the pipeline later anyway.
    drift$missing_roles <- stats::setNames(character(0), character(0))
    drift$missing_moderators <- character(0)
    drift$added <- character(0)
  } else {
    # "Added" columns should only include columns that are new relative to the
    # stored baseline schema. Baseline columns that are simply not mapped
    # should not be treated as drift on every run.
    drift$added <- setdiff(drift$added, expected_schema_cols)
  }

  drift$has_drift <- (
    length(drift$missing_roles) > 0 ||
      length(drift$missing_moderators) > 0 ||
      length(drift$added) > 0 ||
      length(drift$conflicts) > 0
  )

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
      "i" = "Use {.code reconcile = \"ask\"} for interactive resolution.",
      "i" = "Use {.code reconcile = \"auto\"} for automatic resolution."
    )
    cli::cli_abort(msgs)
  }

  # Stored role sources (for display) and "unmatched" columns available for
  # fuzzy matching: everything the store does not already account for.
  box::use(
    artma / data / utils[get_colnames_map, get_required_colnames, get_standardized_colnames]
  )
  role_sources <- as.list(drift$missing_roles)
  matched_role_sources <- character(0)
  full_map <- get_colnames_map()
  # Required roles with no explicit record are tracked as identity mappings
  for (std in setdiff(get_required_colnames(), names(full_map))) {
    full_map[[std]] <- std
  }
  for (std in names(full_map)) {
    if (!std %in% names(drift$missing_roles)) {
      role_sources[[std]] <- full_map[[std]]
      matched_role_sources <- c(matched_role_sources, full_map[[std]])
    }
  }

  moderator_keys <- setdiff(names(columns_store), get_standardized_colnames())
  matched_cols <- make.names(c(matched_role_sources, moderator_keys))
  matched_cols <- setdiff(matched_cols, make.names(unname(drift$missing_roles)))
  available_cols <- setdiff(make.names(colnames(raw_df)), matched_cols)

  # Rename proposals via the shared matching engine
  proposals_roles <- propose_renames(
    drift$missing_roles, available_cols,
    raw_df = raw_df, roles_known = TRUE
  )
  proposals_moderators <- propose_renames(
    stats::setNames(drift$missing_moderators, drift$missing_moderators),
    available_cols,
    raw_df = raw_df
  )

  # Show unified diff
  show_drift_summary(drift, proposals_roles, proposals_moderators, role_sources)

  # Collect decisions
  do_prompt <- (mode == "ask") && should_prompt_user(required_level = "autonomous")

  if (mode == "auto" || !do_prompt) {
    decisions <- auto_decisions(drift, proposals_roles, proposals_moderators)
  } else {
    decisions <- ask_decisions(drift, proposals_roles, proposals_moderators, raw_df)
    confirm_decisions(decisions, drift, role_sources)
  }

  # Apply through the single write path
  apply_reconciliation(decisions)
  persist_expected_schema_cols(current_schema_cols)

  emit_reconcile_complete()

  invisible(NULL)
}

box::export(reconcile_schema, detect_schema_drift)
