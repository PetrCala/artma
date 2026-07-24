#' @title Schema Reconciliation Interactive UI
#' @description Console-facing layer for schema reconciliation: prints the drift
#'   summary, collects decisions (interactively or automatically), and confirms
#'   the outcome. Every prompt lives here so the detection and persistence
#'   layers stay pure.

#' @title Format confidence as percentage string
#' @keywords internal
fmt_pct <- function(score) {
  paste0(round(score * 100), "%")
}

#' @title Show drift summary
#' @description Prints a unified diff of detected changes to the console.
#' @keywords internal
show_drift_summary <- function(drift, proposals_roles, proposals_moderators, role_sources) {
  cli::cli_rule(left = "artma detected dataset changes")

  # Role (standard) columns
  cli::cli_h3("Standard columns")
  for (std in names(role_sources)) {
    stored <- role_sources[[std]]
    if (std %in% names(drift$conflicts)) {
      cli::cli_alert_warning(
        "{.val {stored}} {cli::symbol$arrow_right} {.val {std}} CONFLICTS with an existing {.val {std}} column in the data"
      )
    } else if (std %in% names(drift$missing_roles)) {
      prop <- proposals_roles[[std]]
      if (!is.null(prop) && !is.na(prop$candidate)) {
        cli::cli_alert_warning(
          "{.val {stored}} {cli::symbol$arrow_right} NOT FOUND  (suggested: {.val {prop$candidate}} [{fmt_pct(prop$score)}])"
        )
      } else {
        cli::cli_alert_danger("{.val {stored}} {cli::symbol$arrow_right} NOT FOUND  (no suggestion)")
      }
    } else {
      cli::cli_alert_success("{.val {stored}} {cli::symbol$tick}")
    }
  }

  # Moderator columns
  if (length(drift$missing_moderators) > 0 || length(drift$added) > 0) {
    cli::cli_h3("Moderator columns")
    for (mod in drift$missing_moderators) {
      prop <- proposals_moderators[[mod]]
      if (!is.null(prop) && !is.na(prop$candidate)) {
        cli::cli_alert_warning(
          "{.val {mod}} {cli::symbol$arrow_right} NOT FOUND  (suggested: {.val {prop$candidate}} [{fmt_pct(prop$score)}])"
        )
      } else {
        cli::cli_alert_warning("{.val {mod}} {cli::symbol$arrow_right} NOT FOUND")
      }
    }
    for (col in drift$added) {
      cli::cli_alert_info("{.val {col}}  (new column)")
    }
  }

  cli::cli_rule()
}

#' @title Auto-resolve decisions
#' @description Applies defaults without prompting (for auto mode).
#' @keywords internal
auto_decisions <- function(drift, proposals_roles, proposals_moderators) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data / column_recognition[MATCH_THRESHOLDS]
  )

  renames <- list()
  drops <- character(0)
  remaps <- list()
  conflicts <- list()

  # Mapping conflicts: the explicit mapping is a deliberate user choice, so it
  # wins over a colliding raw column; the raw column is dropped with a warning.
  for (std in names(drift$conflicts)) {
    src <- drift$conflicts[[std]]
    conflicts[[std]] <- "keep_mapping"
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning(
        "Column {.val {std}} is mapped from {.val {src}}, but the data also contains a different {.val {std}} column. Keeping the mapping and dropping the raw {.val {std}} column. Run {.code artma::config.reset(\"{std}\")} to use the raw column instead."
      )
    }
  }

  # Role columns: accept high-confidence proposals, abort if unresolvable
  for (std in names(drift$missing_roles)) {
    stored <- drift$missing_roles[[std]]
    prop <- proposals_roles[[std]]

    if (!is.null(prop) && !is.na(prop$candidate) && prop$score >= MATCH_THRESHOLDS$rename_auto) {
      renames[[std]] <- prop$candidate
      if (get_verbosity() >= 3) {
        cli::cli_alert_info(
          "Auto-mapped {.val {stored}} {cli::symbol$arrow_right} {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
        )
      }
    } else {
      candidate_msg <- if (!is.null(prop) && !is.na(prop$candidate)) {
        cli::format_inline(
          "Best candidate {.val {prop$candidate}} has confidence {fmt_pct(prop$score)} (below {fmt_pct(MATCH_THRESHOLDS$rename_auto)})."
        )
      } else {
        "No candidate found."
      }
      cli::cli_abort(c(
        "x" = "Cannot auto-resolve missing required column: {.val {stored}}",
        "i" = candidate_msg,
        "i" = "Use {.code reconcile = \"ask\"} to resolve this interactively."
      ))
    }
  }

  # Moderators: drop missing, remap if high confidence
  for (mod in drift$missing_moderators) {
    prop <- proposals_moderators[[mod]]

    if (!is.null(prop) && !is.na(prop$candidate) && prop$score >= MATCH_THRESHOLDS$rename_auto) {
      remaps[[mod]] <- prop$candidate
      if (get_verbosity() >= 3) {
        cli::cli_alert_info(
          "Auto-remapped moderator {.val {mod}} {cli::symbol$arrow_right} {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
        )
      }
    } else {
      drops <- c(drops, mod)
      if (get_verbosity() >= 3) {
        cli::cli_alert_warning("Dropped missing moderator {.val {mod}} from analysis configuration.")
      }
    }
  }

  list(renames = renames, drops = drops, remaps = remaps, conflicts = conflicts)
}

#' @title Ask for reconciliation decisions interactively
#' @description Shows menus for each drift item and collects user choices.
#' @keywords internal
ask_decisions <- function(drift, proposals_roles, proposals_moderators, raw_df) {
  renames <- list()
  drops <- character(0)
  remaps <- list()
  conflicts <- list()

  all_df_cols <- make.names(colnames(raw_df))

  # --- Mapping conflicts ---
  for (std in names(drift$conflicts)) {
    src <- drift$conflicts[[std]]

    prompt_text <- cli::format_inline(
      "Column {.val {std}} is mapped from {.val {src}}, but the data also contains a different column named {.val {std}}. Which one should supply {.val {std}}?"
    )
    choices <- c(
      cli::format_inline("Keep the mapping: use {.val {src}} and drop the raw {.val {std}} column"),
      cli::format_inline("Use the raw {.val {std}} column (removes the mapping from {.val {src}})"),
      "Abort"
    )

    choice <- climenu::select(choices = choices, prompt = prompt_text)

    if (is.null(choice) || grepl("^Abort", choice)) {
      cli::cli_abort("Reconciliation aborted by user.")
    }

    conflicts[[std]] <- if (grepl("^Keep", choice)) "keep_mapping" else "use_existing"
  }

  # --- Role columns ---
  for (std in names(drift$missing_roles)) {
    stored <- drift$missing_roles[[std]]
    prop <- proposals_roles[[std]]

    has_proposal <- !is.null(prop) && !is.na(prop$candidate)

    if (has_proposal) {
      prompt_text <- cli::format_inline(
        "Required column {.val {stored}} is missing. Suggested rename: {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
      )
      choices <- c(
        cli::format_inline("Accept: use {.val {prop$candidate}}"),
        "Map to a different column",
        "Abort"
      )
    } else {
      prompt_text <- cli::format_inline(
        "Required column {.val {stored}} is missing. No rename suggestion found."
      )
      choices <- c("Map to a different column", "Abort")
    }

    choice <- climenu::select(choices = choices, prompt = prompt_text)

    if (is.null(choice) || grepl("^Abort", choice)) {
      cli::cli_abort("Reconciliation aborted by user.")
    }

    if (has_proposal && grepl("^Accept", choice)) {
      renames[[std]] <- prop$candidate
    } else {
      # Manual mapping via second menu
      available <- setdiff(all_df_cols, unlist(renames))
      manual_choice <- climenu::select(
        choices = available,
        prompt  = cli::format_inline("Select the column to use for {.val {std}}:")
      )
      if (is.null(manual_choice)) {
        cli::cli_abort("Reconciliation aborted by user.")
      }
      renames[[std]] <- manual_choice
    }
  }

  # --- Moderator columns ---
  for (mod in drift$missing_moderators) {
    prop <- proposals_moderators[[mod]]

    has_proposal <- !is.null(prop) && !is.na(prop$candidate)

    if (has_proposal) {
      prompt_text <- cli::format_inline(
        "Moderator {.val {mod}} no longer exists. Suggested rename: {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
      )
      choices <- c(
        "Drop from analysis (default)",
        cli::format_inline("Remap to {.val {prop$candidate}}"),
        "Map to a different column",
        "Abort"
      )
    } else {
      prompt_text <- cli::format_inline(
        "Moderator {.val {mod}} no longer exists in the dataset."
      )
      choices <- c(
        "Drop from analysis (default)",
        "Map to a different column",
        "Abort"
      )
    }

    choice <- climenu::select(choices = choices, prompt = prompt_text)

    if (is.null(choice) || grepl("^Abort", choice)) {
      cli::cli_abort("Reconciliation aborted by user.")
    }

    if (grepl("^Drop", choice)) {
      drops <- c(drops, mod)
    } else if (has_proposal && grepl("^Remap", choice)) {
      remaps[[mod]] <- prop$candidate
    } else {
      # Manual mapping
      available <- setdiff(all_df_cols, unlist(renames))
      manual_choice <- climenu::select(
        choices = available,
        prompt  = cli::format_inline("Select the column to remap {.val {mod}} to:")
      )
      if (is.null(manual_choice)) {
        cli::cli_abort("Reconciliation aborted by user.")
      }
      remaps[[mod]] <- manual_choice
    }
  }

  list(renames = renames, drops = drops, remaps = remaps, conflicts = conflicts)
}

#' @title Show reconciliation outcome summary and ask for confirmation
#' @keywords internal
confirm_decisions <- function(decisions, drift, role_sources) {
  cli::cli_rule(left = "Configuration update summary")

  # Role renames
  for (std in names(decisions$renames)) {
    old_raw <- role_sources[[std]]
    new_raw <- decisions$renames[[std]]
    cli::cli_alert_success("Mapped: {.val {old_raw}} {cli::symbol$arrow_right} {.val {new_raw}}")
  }

  # Mapping conflict resolutions
  for (std in names(decisions$conflicts)) {
    src <- role_sources[[std]]
    if (identical(decisions$conflicts[[std]], "keep_mapping")) {
      cli::cli_alert_success(
        "Kept mapping: {.val {src}} {cli::symbol$arrow_right} {.val {std}} (the raw {.val {std}} column will be dropped from the analysis)"
      )
    } else {
      cli::cli_alert_success(
        "Using the raw {.val {std}} column (removed the mapping from {.val {src}})"
      )
    }
  }

  # Moderator drops
  for (mod in decisions$drops) {
    cli::cli_alert_warning("Dropped from analysis: {.val {mod}}")
  }

  # Moderator remaps
  for (old_mod in names(decisions$remaps)) {
    new_mod <- decisions$remaps[[old_mod]]
    cli::cli_alert_success("Remapped moderator: {.val {old_mod}} {cli::symbol$arrow_right} {.val {new_mod}}")
  }

  # Added (informational only)
  if (length(drift$added) > 0) {
    cli::cli_alert_info(
      "New column{?s} detected (will be available in summary stats): {.val {drift$added}}"
    )
  }

  # Unchanged role columns
  unchanged <- setdiff(
    names(role_sources),
    c(names(drift$missing_roles), names(drift$conflicts))
  )
  if (length(unchanged) > 0) {
    unchanged_raw <- unlist(role_sources[unchanged], use.names = FALSE)
    cli::cli_alert_success("Unchanged: {.val {unchanged_raw}}")
  }

  cli::cli_rule()

  choice <- climenu::select(
    choices = c("Save changes and continue analysis", "Abort"),
    prompt  = "Apply these changes to your configuration file?"
  )

  if (is.null(choice) || choice == "Abort") {
    cli::cli_abort("Reconciliation aborted by user.")
  }

  invisible(NULL)
}

box::export(show_drift_summary, auto_decisions, ask_decisions, confirm_decisions)
