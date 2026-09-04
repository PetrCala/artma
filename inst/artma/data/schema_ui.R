#' @title Schema Reconciliation Interactive UI
#' @description Console-facing layer for schema reconciliation: prints the drift
#'   summary, collects decisions (interactively or automatically), and confirms
#'   the outcome. Every prompt lives here so the detection and persistence
#'   layers stay pure. The menu backend is injectable (`select_fn`), so the
#'   prompt flow is drivable without a terminal.

#' @title Format confidence as percentage string
#' @keywords internal
fmt_pct <- function(score) {
  paste0(round(score * 100), "%")
}

#' @title Whether a proposal is safe to apply without asking
#' @description High enough confidence and no tie, either with a rival
#'   candidate or with another missing column claiming the same candidate.
#' @keywords internal
proposal_is_auto <- function(prop) {
  box::use(artma / data / column_recognition[MATCH_THRESHOLDS])

  !is.null(prop) && !is.na(prop$candidate) &&
    prop$score >= MATCH_THRESHOLDS$rename_auto &&
    !isTRUE(prop$ambiguous)
}

#' @title Whether a proposal carries a candidate worth showing
#' @keywords internal
proposal_has_candidate <- function(prop) {
  !is.null(prop) && !is.na(prop$candidate)
}

#' @title Describe a proposal for the drift summary
#' @return *\[character\]* Plain text such as `suggested: "x" [80%]`, with a
#'   note when the suggestion is a tie, or `no suggestion`.
#' @keywords internal
describe_proposal <- function(prop) {
  if (!proposal_has_candidate(prop)) {
    return("no suggestion")
  }
  text <- cli::format_inline("suggested: {.val {prop$candidate}} [{fmt_pct(prop$score)}]")
  if (isTRUE(prop$ambiguous)) {
    tie <- if (!is.na(prop$runner_up)) {
      cli::format_inline("ties with {.val {prop$runner_up}}")
    } else {
      "also claimed by another missing column"
    }
    text <- paste0(text, ", ", tie)
  }
  text
}

#' @title Hints on how to resolve a required column by hand
#' @keywords internal
manual_mapping_hints <- function(std) {
  c(
    "i" = cli::format_inline(
      "Map it directly: {.code artma::config_set(\"{std}\", source_name = \"<column>\")}, or set {.code data.columns.{std}.source_name} in the options file."
    ),
    "i" = cli::format_inline(
      "Or set {.code data.reconcile_mode} to {.val ask} and run in an interactive session to pick the column at a prompt."
    )
  )
}

#' @title Show drift summary
#' @description Prints a unified diff of detected changes to the console.
#' @keywords internal
show_drift_summary <- function(drift, proposals_roles, proposals_optional, proposals_moderators, role_sources) {
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
      note <- describe_proposal(prop)
      if (proposal_has_candidate(prop)) {
        cli::cli_alert_warning("{.val {stored}} {cli::symbol$arrow_right} NOT FOUND  ({note})")
      } else {
        cli::cli_alert_danger("{.val {stored}} {cli::symbol$arrow_right} NOT FOUND  ({note})")
      }
    } else if (std %in% names(drift$missing_optional_roles)) {
      note <- describe_proposal(proposals_optional[[std]])
      cli::cli_alert_warning(
        "{.val {stored}} {cli::symbol$arrow_right} NOT FOUND  (optional {.field {std}}; {note})"
      )
    } else {
      cli::cli_alert_success("{.val {stored}} {cli::symbol$tick}")
    }
  }

  # Moderator columns
  if (length(drift$missing_moderators) > 0 || length(drift$added) > 0) {
    cli::cli_h3("Moderator columns")
    for (mod in drift$missing_moderators) {
      note <- describe_proposal(proposals_moderators[[mod]])
      cli::cli_alert_warning("{.val {mod}} {cli::symbol$arrow_right} NOT FOUND  ({note})")
    }
    for (col in drift$added) {
      cli::cli_alert_info("{.val {col}}  (new column)")
    }
  }

  cli::cli_rule()
}

#' @title Auto-resolve decisions
#' @description Applies defaults without prompting (for auto mode).
#' @return *\[list\]* With `renames` (required roles), `unmaps` (optional roles
#'   whose mapping is dropped), `drops`, `remaps` (moderators), `conflicts`.
#' @keywords internal
auto_decisions <- function(drift, proposals_roles, proposals_optional, proposals_moderators) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / data / column_recognition[MATCH_THRESHOLDS]
  )

  renames <- list()
  unmaps <- character(0)
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
        "Column {.val {std}} is mapped from {.val {src}}, but the data also contains a different {.val {std}} column. Keeping the mapping and dropping the raw {.val {std}} column. Run {.code artma::config_reset(\"{std}\")} to use the raw column instead."
      )
    }
  }

  # Required roles: accept unambiguous high-confidence proposals, abort otherwise
  for (std in names(drift$missing_roles)) {
    stored <- drift$missing_roles[[std]]
    prop <- proposals_roles[[std]]

    if (proposal_is_auto(prop)) {
      renames[[std]] <- prop$candidate
      if (get_verbosity() >= 3) {
        cli::cli_alert_info(
          "Auto-mapped {.val {stored}} {cli::symbol$arrow_right} {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
        )
      }
      next
    }

    candidate_msg <- if (!proposal_has_candidate(prop)) {
      "No candidate found."
    } else if (isTRUE(prop$ambiguous)) {
      cli::format_inline(
        "Best candidate {.val {prop$candidate}} [{fmt_pct(prop$score)}] is ambiguous: it {describe_tie(prop)}."
      )
    } else {
      cli::format_inline(
        "Best candidate {.val {prop$candidate}} has confidence {fmt_pct(prop$score)} (below {fmt_pct(MATCH_THRESHOLDS$rename_auto)})."
      )
    }
    cli::cli_abort(c(
      "x" = "Cannot auto-resolve missing required column: {.val {stored}}",
      "i" = candidate_msg,
      manual_mapping_hints(std)
    ))
  }

  # Optional roles: remap if unambiguous and confident, otherwise drop the
  # mapping. The pipeline tolerates an unmapped optional role, so this never
  # aborts.
  for (std in names(drift$missing_optional_roles)) {
    stored <- drift$missing_optional_roles[[std]]
    prop <- proposals_optional[[std]]

    if (proposal_is_auto(prop)) {
      renames[[std]] <- prop$candidate
      if (get_verbosity() >= 3) {
        cli::cli_alert_info(
          "Auto-mapped optional {.val {stored}} {cli::symbol$arrow_right} {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
        )
      }
    } else {
      unmaps <- c(unmaps, std)
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(
          "Dropped the mapping {.val {stored}} {cli::symbol$arrow_right} {.field {std}}: the column no longer exists in the data. Map another column with {.code artma::config_set(\"{std}\", source_name = \"<column>\")} if needed."
        )
      }
    }
  }

  # Moderators: drop missing, remap if unambiguous and confident
  for (mod in drift$missing_moderators) {
    prop <- proposals_moderators[[mod]]

    if (proposal_is_auto(prop)) {
      remaps[[mod]] <- prop$candidate
      if (get_verbosity() >= 3) {
        cli::cli_alert_info(
          "Auto-remapped moderator {.val {mod}} {cli::symbol$arrow_right} {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
        )
      }
    } else {
      drops <- c(drops, mod)
      if (get_verbosity() >= 3) {
        why <- if (proposal_has_candidate(prop)) {
          cli::format_inline(" ({describe_proposal(prop)}; not applied without asking)")
        } else {
          ""
        }
        cli::cli_alert_warning("Dropped missing moderator {.val {mod}} from analysis configuration{why}.")
      }
    }
  }

  list(renames = renames, unmaps = unmaps, drops = drops, remaps = remaps, conflicts = conflicts)
}

#' @title Describe why a proposal is a tie
#' @keywords internal
describe_tie <- function(prop) {
  if (!is.na(prop$runner_up)) {
    cli::format_inline("scores about the same as {.val {prop$runner_up}}")
  } else {
    "is claimed equally well by another missing column"
  }
}

#' @title Describe an unexpected menu answer for an error message
#' @keywords internal
describe_answer <- function(choice) {
  if (is.character(choice) && length(choice) == 1L) {
    return(cli::format_inline("{.val {choice}}"))
  }
  cli::format_inline("{.cls {class(choice)}} of length {length(choice)}")
}

#' @title Ask a menu question and check the answer against the offered choices
#' @description The menu backend is injectable, so its return value is treated
#'   as untrusted input rather than assumed to be one of the labels it was
#'   handed. Anything other than `NULL` or an offered choice is a broken
#'   backend, not a decision, and must not reach the configuration.
#' @return *\[character or NULL\]* The chosen label, or `NULL` when the menu
#'   was cancelled.
#' @keywords internal
select_checked <- function(select_fn, choices, prompt) {
  choice <- select_fn(choices = choices, prompt = prompt)

  if (is.null(choice)) {
    return(NULL)
  }
  if (is.character(choice) && length(choice) == 1L && choice %in% choices) {
    return(choice)
  }

  cli::cli_abort(c(
    "x" = "The menu backend returned a value that was not offered: {describe_answer(choice)}.",
    "i" = "It must return {.code NULL} or one of the offered choices: {.val {choices}}."
  ))
}

#' @title Ask for reconciliation decisions interactively
#' @description Shows menus for each drift item and collects user choices.
#'   Columns already chosen for one record are withheld from the later menus,
#'   so two records never end up on the same column.
#' @param available_cols *\[character, optional\]* Raw columns the manual picker
#'   may offer: the dataset columns that no untouched record already backs, the
#'   same pool the rename proposals draw from. Defaults to every column of
#'   `raw_df`.
#' @param select_fn *\[function, optional\]* Menu backend with the signature
#'   of `climenu::select(choices, prompt)`, returning the chosen label or
#'   `NULL`. Defaults to `climenu::select`; injectable for tests.
#' @keywords internal
ask_decisions <- function(drift, proposals_roles, proposals_optional, proposals_moderators, raw_df, available_cols = NULL, select_fn = NULL) {
  if (is.null(select_fn)) select_fn <- climenu::select
  if (is.null(available_cols)) available_cols <- make.names(colnames(raw_df))

  renames <- list()
  unmaps <- character(0)
  drops <- character(0)
  remaps <- list()
  conflicts <- list()

  taken <- character(0)

  abort_by_user <- function() cli::cli_abort("Reconciliation aborted by user.")

  # A proposal whose candidate an earlier answer already claimed is no longer
  # on offer.
  usable_proposal <- function(prop) {
    proposal_has_candidate(prop) && !prop$candidate %in% taken
  }

  # The pool is narrowed the same way the proposals are: a column an untouched
  # record already backs is never on offer. Two records mapped to one raw
  # column collapse silently in `standardize_column_names()`, and the abort
  # that follows names neither the duplicate nor this menu.
  pick_manually <- function(prompt_text, target) {
    available <- setdiff(available_cols, taken)
    if (length(available) == 0) {
      cli::cli_abort(c(
        "x" = "No unclaimed column is left to map {.val {target}} to.",
        "i" = "Every column of the dataset already backs another configured column.",
        "i" = "Edit {.code data.columns} in the options file to free one up."
      ))
    }
    choice <- select_checked(select_fn, available, prompt_text)
    if (is.null(choice)) abort_by_user()
    choice
  }

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

    choice <- select_checked(select_fn, choices, prompt_text)

    if (is.null(choice) || grepl("^Abort", choice)) abort_by_user()

    conflicts[[std]] <- if (grepl("^Keep", choice)) "keep_mapping" else "use_existing"
  }

  # --- Required role columns ---
  for (std in names(drift$missing_roles)) {
    stored <- drift$missing_roles[[std]]
    prop <- proposals_roles[[std]]
    has_proposal <- usable_proposal(prop)

    if (has_proposal) {
      prompt_text <- cli::format_inline(
        "Required column {.val {stored}} is missing. Suggested rename: {.val {prop$candidate}} [{fmt_pct(prop$score)}]{tie_suffix(prop)}"
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

    choice <- select_checked(select_fn, choices, prompt_text)

    if (is.null(choice) || grepl("^Abort", choice)) abort_by_user()

    picked <- if (has_proposal && grepl("^Accept", choice)) {
      prop$candidate
    } else {
      pick_manually(cli::format_inline("Select the column to use for {.val {std}}:"), std)
    }
    renames[[std]] <- picked
    taken <- c(taken, picked)
  }

  # --- Optional role columns ---
  for (std in names(drift$missing_optional_roles)) {
    stored <- drift$missing_optional_roles[[std]]
    prop <- proposals_optional[[std]]
    has_proposal <- usable_proposal(prop)

    prompt_text <- cli::format_inline(
      "Optional column {.field {std}} was mapped from {.val {stored}}, which no longer exists in the dataset."
    )
    choices <- c(
      "Drop the mapping (default)",
      if (has_proposal) cli::format_inline("Remap to {.val {prop$candidate}} [{fmt_pct(prop$score)}]{tie_suffix(prop)}"),
      "Map to a different column",
      "Abort"
    )

    choice <- select_checked(select_fn, choices, prompt_text)

    if (is.null(choice) || grepl("^Abort", choice)) abort_by_user()

    if (grepl("^Drop", choice)) {
      unmaps <- c(unmaps, std)
      next
    }
    picked <- if (has_proposal && grepl("^Remap", choice)) {
      prop$candidate
    } else {
      pick_manually(cli::format_inline("Select the column to use for {.field {std}}:"), std)
    }
    renames[[std]] <- picked
    taken <- c(taken, picked)
  }

  # --- Moderator columns ---
  for (mod in drift$missing_moderators) {
    prop <- proposals_moderators[[mod]]
    has_proposal <- usable_proposal(prop)

    if (has_proposal) {
      prompt_text <- cli::format_inline(
        "Moderator {.val {mod}} no longer exists. Suggested rename: {.val {prop$candidate}} [{fmt_pct(prop$score)}]{tie_suffix(prop)}"
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

    choice <- select_checked(select_fn, choices, prompt_text)

    if (is.null(choice) || grepl("^Abort", choice)) abort_by_user()

    if (grepl("^Drop", choice)) {
      drops <- c(drops, mod)
      next
    }
    picked <- if (has_proposal && grepl("^Remap", choice)) {
      prop$candidate
    } else {
      pick_manually(cli::format_inline("Select the column to remap {.val {mod}} to:"), mod)
    }
    remaps[[mod]] <- picked
    taken <- c(taken, picked)
  }

  list(renames = renames, unmaps = unmaps, drops = drops, remaps = remaps, conflicts = conflicts)
}

#' @title Suffix noting a tied suggestion in a prompt
#' @keywords internal
tie_suffix <- function(prop) {
  if (!isTRUE(prop$ambiguous)) {
    return("")
  }
  paste0(" (", describe_tie(prop), ")")
}

#' @title Show reconciliation outcome summary and ask for confirmation
#' @param select_fn *\[function, optional\]* Menu backend, see `ask_decisions()`.
#' @keywords internal
confirm_decisions <- function(decisions, drift, role_sources, select_fn = NULL) {
  if (is.null(select_fn)) select_fn <- climenu::select

  cli::cli_rule(left = "Configuration update summary")

  # Role renames (required and optional)
  for (std in names(decisions$renames)) {
    old_raw <- role_sources[[std]]
    new_raw <- decisions$renames[[std]]
    cli::cli_alert_success("Mapped: {.val {old_raw}} {cli::symbol$arrow_right} {.val {new_raw}}")
  }

  # Dropped optional mappings
  for (std in decisions$unmaps) {
    cli::cli_alert_warning(
      "Dropped mapping: {.val {role_sources[[std]]}} {cli::symbol$arrow_right} {.field {std}} (optional; left unmapped)"
    )
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
    c(names(drift$missing_roles), names(drift$missing_optional_roles), names(drift$conflicts))
  )
  if (length(unchanged) > 0) {
    unchanged_raw <- unlist(role_sources[unchanged], use.names = FALSE)
    cli::cli_alert_success("Unchanged: {.val {unchanged_raw}}")
  }

  cli::cli_rule()

  choice <- select_checked(
    select_fn,
    choices = c("Save changes and continue analysis", "Abort"),
    prompt  = "Apply these changes to your configuration file?"
  )

  if (is.null(choice) || choice == "Abort") {
    cli::cli_abort("Reconciliation aborted by user.")
  }

  invisible(NULL)
}

box::export(show_drift_summary, auto_decisions, ask_decisions, confirm_decisions, proposal_is_auto)
