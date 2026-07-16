#' @title Schema Drift Detection and Reconciliation
#' @description Detects changes between the user's dataset columns and the stored
#'   configuration, then guides the user through resolving those changes before
#'   the analysis pipeline runs.

# -- Helpe---

#' @title Format confidence as percentage string
#' @keywords internal
fmt_pct <- function(score) {
  paste0(round(score * 100), "%")
}

#' @title Normalize expected schema columns
#' @keywords internal
normalize_expected_schema_cols <- function(cols) {
  if (is.null(cols)) {
    return(character(0))
  }

  if (is.list(cols)) {
    cols <- unlist(cols, use.names = FALSE)
  }

  cols <- as.character(cols)
  cols <- cols[!is.na(cols)]
  cols <- trimws(cols)
  cols <- cols[nzchar(cols)]

  if (length(cols) == 0L) {
    return(character(0))
  }

  unique(make.names(cols))
}

#' @title Persist expected schema columns
#' @keywords internal
persist_expected_schema_cols <- function(cols) {
  box::use(artma / libs / core / utils[get_verbosity])

  normalized <- normalize_expected_schema_cols(cols)
  options("artma.data.expected_schema_columns" = normalized)

  options_file_name <- getOption("artma.temp.file_name")
  options_dir <- getOption("artma.temp.dir_name")
  has_options_file <- !is.null(options_file_name) && !is.null(options_dir)

  if (!has_options_file) {
    return(invisible(normalized))
  }

  tryCatch(
    {
      suppressMessages(
        artma::options.modify(
          options_file_name = options_file_name,
          options_dir = options_dir,
          user_input = list("data.expected_schema_columns" = normalized),
          should_validate = FALSE
        )
      )
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(
          "Could not persist expected schema columns to options file: {e$message}"
        )
      }
    }
  )

  invisible(normalized)
}

#' @title Emit schema reconciliation completion message
#' @keywords internal
emit_reconcile_complete <- function() {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 3) {
    cli::cli_alert_success("Schema reconciliation complete.")
  }
}

# -- Core detecti--

#' @title Detect schema drift
#' @description Compares the current dataframe columns against the stored
#'   configuration to identify renames, removals, and additions.
#' @param raw_df *\[data.frame\]* Raw dataframe with original column names.
#' @param colnames_map *\[list\]* Named list mapping standard names to actual
#'   column names (from `artma.data.colnames`).
#' @param config_overrides *\[list\]* Sparse config overrides (from `artma.data.config`).
#' @return *\[list\]* Drift report with fields: `colnames_drift`, `config_drift`, `has_drift`.
#' @keywords internal
detect_schema_drift <- function(raw_df, colnames_map, config_overrides) {
  df_cols <- make.names(colnames(raw_df))

  # --- Required columns (via colnames map) ---
  map_clean <- colnames_map[!vapply(colnames_map, function(x) is.null(x) || (length(x) == 1 && is.na(x)), logical(1))]
  map_values <- stats::setNames(make.names(unlist(map_clean)), names(map_clean))

  # Standard names whose actual column is missing from the df
  missing_required_std <- names(map_values)[!map_values %in% df_cols]
  missing_required_raw <- map_values[missing_required_std]

  # --- Moderator columns (config override keys) ---
  standard_names <- names(map_clean)
  config_keys <- names(config_overrides)

  # Exclude computed columns - they are added by the pipeline, not by the user's
  # data, so they will never be present in the raw df and should not be flagged
  # as missing moderators.
  is_computed <- vapply(
    config_overrides,
    function(entry) isTRUE(entry[["is_computed"]]),
    logical(1)
  )
  user_config_keys <- config_keys[!is_computed]
  moderator_keys <- setdiff(user_config_keys, standard_names)
  moderator_keys_norm <- stats::setNames(make.names(moderator_keys), moderator_keys)

  missing_moderators <- names(moderator_keys_norm)[!moderator_keys_norm %in% df_cols]

  # --- Added columns (in df but not referenced by anything) ---
  # Use all config keys (including computed) for this check so that columns
  # the system already knows about (even as computed) don't appear as "new".
  all_config_keys_norm <- make.names(config_keys)
  all_referenced <- unique(c(unlist(map_values), all_config_keys_norm))
  added <- df_cols[!df_cols %in% all_referenced]

  list(
    colnames_drift = list(
      missing_std  = missing_required_std, # standard names (effect, se, ...)
      missing_raw  = missing_required_raw # the actual stored col names that vanished
    ),
    config_drift = list(
      missing_moderators = missing_moderators,
      added              = added
    ),
    has_drift = (
      length(missing_required_std) > 0 ||
        length(missing_moderators) > 0 ||
        length(added) > 0
    )
  )
}

# -- Fuzzy matchi--

#' @title Propose renames via fuzzy matching
#' @description For each missing column name, finds the best candidate from the
#'   available (unmatched) columns using string edit distance.
#' @param missing_raw *\[character\]* Named vector: names are standard names,
#'   values are the original stored column names that are now missing.
#' @param available_cols *\[character\]* Unmatched columns from the raw df.
#' @return *\[list\]* Named by the missing column's original stored name.
#'   Each element: `list(candidate, score)`.
#' @keywords internal
propose_renames <- function(missing_raw, available_cols) {
  if (length(missing_raw) == 0 || length(available_cols) == 0) {
    return(stats::setNames(list(), character(0)))
  }

  proposals <- list()

  for (i in seq_along(missing_raw)) {
    stored_name <- missing_raw[[i]]
    std_name <- names(missing_raw)[[i]]

    best_score <- 0
    best_candidate <- NA_character_

    for (cand in available_cols) {
      max_len <- max(nchar(stored_name), nchar(cand))
      if (max_len == 0L) next
      score <- 1 - utils::adist(stored_name, cand)[1L, 1L] / max_len
      if (score > best_score) {
        best_score <- score
        best_candidate <- cand
      }
    }

    proposals[[stored_name]] <- list(
      std_name  = std_name,
      candidate = if (best_score >= 0.5) best_candidate else NA_character_,
      score     = best_score
    )
  }

  proposals
}

# -- Displ---

#' @title Show drift summary
#' @description Prints a unified diff of detected changes to the console.
#' @keywords internal
show_drift_summary <- function(drift, proposals_required, proposals_moderators,
                               colnames_map) {
  map_clean <- colnames_map[!vapply(
    colnames_map, function(x) is.null(x) || (length(x) == 1 && is.na(x)), logical(1)
  )]

  cli::cli_rule(left = "artma detected dataset changes")

  # Required columns
  cli::cli_h3("Required columns")
  for (std in names(map_clean)) {
    raw_val <- map_clean[[std]]
    if (std %in% drift$colnames_drift$missing_std) {
      prop <- proposals_required[[make.names(raw_val)]]
      if (!is.null(prop) && !is.na(prop$candidate)) {
        cli::cli_alert_warning(
          "{.val {raw_val}} {cli::symbol$arrow_right} NOT FOUND  (suggested: {.val {prop$candidate}} [{fmt_pct(prop$score)}])"
        )
      } else {
        cli::cli_alert_danger("{.val {raw_val}} {cli::symbol$arrow_right} NOT FOUND  (no suggestion)")
      }
    } else {
      cli::cli_alert_success("{.val {raw_val}} {cli::symbol$tick}")
    }
  }

  # Moderator columns
  if (length(drift$config_drift$missing_moderators) > 0 || length(drift$config_drift$added) > 0) {
    cli::cli_h3("Moderator columns")
    for (mod in drift$config_drift$missing_moderators) {
      prop <- proposals_moderators[[make.names(mod)]]
      if (!is.null(prop) && !is.na(prop$candidate)) {
        cli::cli_alert_warning(
          "{.val {mod}} {cli::symbol$arrow_right} NOT FOUND  (suggested: {.val {prop$candidate}} [{fmt_pct(prop$score)}])"
        )
      } else {
        cli::cli_alert_warning("{.val {mod}} {cli::symbol$arrow_right} NOT FOUND")
      }
    }
    for (col in drift$config_drift$added) {
      cli::cli_alert_info("{.val {col}}  (new column)")
    }
  }

  cli::cli_rule()
}

# -- Decision collecti--

#' @title Auto-resolve decisions
#' @description Applies defaults without prompting (for auto mode).
#' @keywords internal
auto_decisions <- function(drift, proposals_required, proposals_moderators) {
  box::use(artma / libs / core / utils[get_verbosity])

  colnames_updates <- list()
  config_drops <- character(0)
  config_remaps <- list()

  # Required columns: accept high-confidence proposals, abort if unresolvable
  for (std in drift$colnames_drift$missing_std) {
    raw_val <- drift$colnames_drift$missing_raw[[std]]
    stored <- make.names(raw_val)
    prop <- proposals_required[[stored]]

    if (!is.null(prop) && !is.na(prop$candidate) && prop$score >= 0.75) {
      colnames_updates[[std]] <- prop$candidate
      if (get_verbosity() >= 3) {
        cli::cli_alert_info(
          "Auto-mapped {.val {raw_val}} {cli::symbol$arrow_right} {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
        )
      }
    } else {
      candidate_msg <- if (!is.null(prop) && !is.na(prop$candidate)) {
        cli::format_inline("Best candidate {.val {prop$candidate}} has confidence {fmt_pct(prop$score)} (below 75%).")
      } else {
        "No candidate found."
      }
      cli::cli_abort(c(
        "x" = "Cannot auto-resolve missing required column: {.val {raw_val}}",
        "i" = candidate_msg,
        "i" = "Use {.code reconcile = \"ask\"} to resolve this interactively."
      ))
    }
  }

  # Moderators: drop missing, remap if high confidence
  for (mod in drift$config_drift$missing_moderators) {
    stored <- make.names(mod)
    prop <- proposals_moderators[[stored]]

    if (!is.null(prop) && !is.na(prop$candidate) && prop$score >= 0.75) {
      config_remaps[[mod]] <- prop$candidate
      if (get_verbosity() >= 3) {
        cli::cli_alert_info(
          "Auto-remapped moderator {.val {mod}} {cli::symbol$arrow_right} {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
        )
      }
    } else {
      config_drops <- c(config_drops, mod)
      if (get_verbosity() >= 3) {
        cli::cli_alert_warning("Dropped missing moderator {.val {mod}} from analysis configuration.")
      }
    }
  }

  list(
    colnames_updates = colnames_updates,
    config_drops     = config_drops,
    config_remaps    = config_remaps
  )
}

#' @title Ask for reconciliation decisions interactively
#' @description Shows menus for each drift item and collects user choices.
#' @keywords internal
ask_decisions <- function(drift, proposals_required, proposals_moderators, raw_df) {
  colnames_updates <- list()
  config_drops <- character(0)
  config_remaps <- list()

  all_df_cols <- make.names(colnames(raw_df))

  # --- Required columns ---
  for (std in drift$colnames_drift$missing_std) {
    raw_val <- drift$colnames_drift$missing_raw[[std]]
    stored <- make.names(raw_val)
    prop <- proposals_required[[stored]]

    has_proposal <- !is.null(prop) && !is.na(prop$candidate)

    if (has_proposal) {
      prompt_text <- cli::format_inline(
        "Required column {.val {raw_val}} is missing. Suggested rename: {.val {prop$candidate}} [{fmt_pct(prop$score)}]"
      )
      choices <- c(
        cli::format_inline("Accept: use {.val {prop$candidate}}"),
        "Map to a different column",
        "Abort"
      )
    } else {
      prompt_text <- cli::format_inline(
        "Required column {.val {raw_val}} is missing. No rename suggestion found."
      )
      choices <- c("Map to a different column", "Abort")
    }

    choice <- climenu::select(choices = choices, prompt = prompt_text)

    if (is.null(choice) || grepl("^Abort", choice)) {
      cli::cli_abort("Reconciliation aborted by user.")
    }

    if (has_proposal && grepl("^Accept", choice)) {
      colnames_updates[[std]] <- prop$candidate
    } else {
      # Manual mapping via second menu
      available <- setdiff(all_df_cols, unlist(colnames_updates))
      manual_choice <- climenu::select(
        choices = available,
        prompt  = cli::format_inline("Select the column to use for {.val {std}}:")
      )
      if (is.null(manual_choice)) {
        cli::cli_abort("Reconciliation aborted by user.")
      }
      colnames_updates[[std]] <- manual_choice
    }
  }

  # --- Moderator columns ---
  for (mod in drift$config_drift$missing_moderators) {
    stored <- make.names(mod)
    prop <- proposals_moderators[[stored]]

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
      config_drops <- c(config_drops, mod)
    } else if (has_proposal && grepl("^Remap", choice)) {
      config_remaps[[mod]] <- prop$candidate
    } else {
      # Manual mapping
      available <- setdiff(all_df_cols, unlist(colnames_updates))
      manual_choice <- climenu::select(
        choices = available,
        prompt  = cli::format_inline("Select the column to remap {.val {mod}} to:")
      )
      if (is.null(manual_choice)) {
        cli::cli_abort("Reconciliation aborted by user.")
      }
      config_remaps[[mod]] <- manual_choice
    }
  }

  list(
    colnames_updates = colnames_updates,
    config_drops     = config_drops,
    config_remaps    = config_remaps
  )
}

# -- Confirmation and summa-y

#' @title Show reconciliation outcome summary and ask for confirmation
#' @keywords internal
confirm_decisions <- function(decisions, drift, colnames_map) {
  map_clean <- colnames_map[!vapply(
    colnames_map, function(x) is.null(x) || (length(x) == 1 && is.na(x)), logical(1)
  )]

  cli::cli_rule(left = "Configuration update summary")

  # Colnames remaps
  for (std in names(decisions$colnames_updates)) {
    old_raw <- map_clean[[std]]
    new_raw <- decisions$colnames_updates[[std]]
    cli::cli_alert_success("Mapped: {.val {old_raw}} {cli::symbol$arrow_right} {.val {new_raw}}")
  }

  # Moderator drops
  for (mod in decisions$config_drops) {
    cli::cli_alert_warning("Dropped from analysis: {.val {mod}}")
  }

  # Moderator remaps
  for (old_mod in names(decisions$config_remaps)) {
    new_mod <- decisions$config_remaps[[old_mod]]
    cli::cli_alert_success("Remapped moderator: {.val {old_mod}} {cli::symbol$arrow_right} {.val {new_mod}}")
  }

  # Added (informational only)
  if (length(drift$config_drift$added) > 0) {
    cli::cli_alert_info(
      "New column{?s} detected (will be available in summary stats): {.val {drift$config_drift$added}}"
    )
  }

  # Unchanged required cols
  unchanged <- setdiff(names(map_clean), drift$colnames_drift$missing_std)
  if (length(unchanged) > 0) {
    cli::cli_alert_success("Unchanged: {.val {unchanged}}")
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

# -- Applicati--

#' @title Apply reconciliation decisions
#' @description Persists the reconciliation decisions to the options file and
#'   updates in-memory state.
#' @keywords internal
apply_reconciliation <- function(decisions) {
  box::use(
    artma / data_config / write[update_data_config, reset_config_overrides],
    artma / data_config / read[get_data_config],
    artma / libs / core / utils[get_verbosity]
  )

  options_file_name <- getOption("artma.temp.file_name")
  options_dir <- getOption("artma.temp.dir_name")

  has_options_file <- !is.null(options_file_name) && !is.null(options_dir)

  # 1. Update colnames map entries - always in-memory, persist to file if available
  if (length(decisions$colnames_updates) > 0) {
    for (std in names(decisions$colnames_updates)) {
      opt_key <- paste0("artma.data.colnames.", std)
      options(stats::setNames(list(decisions$colnames_updates[[std]]), opt_key))
    }
    if (has_options_file) {
      colnames_input <- stats::setNames(
        as.list(unlist(decisions$colnames_updates)),
        paste0("data.colnames.", names(decisions$colnames_updates))
      )
      suppressMessages(
        artma::options.modify(
          options_file_name = options_file_name,
          options_dir       = options_dir,
          user_input        = colnames_input,
          should_validate   = FALSE
        )
      )
    }
  }

  # 2. Drop missing moderators - always update in-memory config, persist if file available
  if (length(decisions$config_drops) > 0) {
    current_overrides <- getOption("artma.data.config", list())
    if (!is.list(current_overrides)) current_overrides <- list()
    for (mod in decisions$config_drops) {
      current_overrides[[make.names(mod)]] <- NULL
    }
    options("artma.data.config" = current_overrides)

    if (has_options_file) {
      for (mod in decisions$config_drops) {
        tryCatch(
          reset_config_overrides(var_name = mod),
          error = function(e) {
            if (get_verbosity() >= 2) {
              cli::cli_alert_warning("Could not drop {.val {mod}} from options file: {e$message}")
            }
          }
        )
      }
    }
  }

  # 3. Remap moderators - always update in-memory config (sparse overrides), persist if file available
  if (length(decisions$config_remaps) > 0) {
    current_overrides <- getOption("artma.data.config", list())
    if (!is.list(current_overrides)) current_overrides <- list()

    for (old_mod in names(decisions$config_remaps)) {
      new_mod <- decisions$config_remaps[[old_mod]]
      old_key <- make.names(old_mod)
      new_key <- make.names(new_mod)
      old_entry <- current_overrides[[old_key]]

      # Move override entry to new key (keep verbose name, update var_name)
      if (!is.null(old_entry)) {
        old_entry$var_name <- new_mod
        current_overrides[[new_key]] <- old_entry
      }
      current_overrides[[old_key]] <- NULL
    }
    options("artma.data.config" = current_overrides)

    if (has_options_file) {
      full_config <- tryCatch(get_data_config(), error = function(e) list())

      for (old_mod in names(decisions$config_remaps)) {
        new_mod <- decisions$config_remaps[[old_mod]]
        old_key <- make.names(old_mod)
        new_key <- make.names(new_mod)
        old_entry <- full_config[[old_key]]

        if (!is.null(old_entry)) {
          old_entry$var_name <- new_mod
          tryCatch(
            update_data_config(stats::setNames(list(old_entry), new_key)),
            error = function(e) {
              if (get_verbosity() >= 2) {
                cli::cli_alert_warning("Could not remap moderator {.val {old_mod}}: {e$message}")
              }
            }
          )
        }
        tryCatch(
          reset_config_overrides(var_name = old_key),
          error = function(e) {
            if (get_verbosity() >= 2) {
              cli::cli_alert_warning("Could not remove old moderator {.val {old_mod}}: {e$message}")
            }
          }
        )
      }
    }
  }

  invisible(NULL)
}

# -- Main entry poi--

#' @title Reconcile schema drift
#' @description Detects changes between the current dataset columns and the
#'   stored configuration, then resolves them before the analysis pipeline runs.
#'   Should be called with the raw (un-standardized) dataframe.
#' @param raw_df *\[data.frame\]* Raw dataframe with original column names.
#' @param mode *\[character\]* One of `"ask"`, `"auto"`, or `"strict"`. If `NULL`,
#'   reads from `artma.data.reconcile_mode` option (default: `"ask"`).
#' @return `NULL` invisibly. Side effects: updates options file and in-memory
#'   state if drift is detected and resolved.
#' @keywords internal
reconcile_schema <- function(raw_df, mode = NULL) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / autonomy[should_prompt_user],
    artma / options / index[get_option_group]
  )

  mode <- mode %||% getOption("artma.data.reconcile_mode", "ask")
  current_schema_cols <- unique(make.names(colnames(raw_df)))
  expected_schema_cols <- normalize_expected_schema_cols(
    getOption("artma.data.expected_schema_columns", NA_character_)
  )

  # Initialize baseline schema on first run. Until this baseline exists, drift
  # details are suppressed regardless of reconcile mode.
  if (length(expected_schema_cols) == 0L) {
    persist_expected_schema_cols(current_schema_cols)
    emit_reconcile_complete()
    return(invisible(NULL))
  }

  # Get current colnames map and config overrides
  colnames_map <- tryCatch(
    {
      opt <- get_option_group("artma.data.colnames")
      if (is.list(opt)) opt else list()
    },
    error = function(e) list()
  )

  config_overrides <- getOption("artma.data.config", list())
  if (!is.list(config_overrides)) config_overrides <- list()

  # Detect drift
  drift <- detect_schema_drift(raw_df, colnames_map, config_overrides)

  # "Added" columns should only include columns that are new relative to the
  # stored baseline schema. Baseline columns that are simply not mapped should
  # not be treated as drift on every run.
  drift$config_drift$added <- setdiff(drift$config_drift$added, expected_schema_cols)
  drift$has_drift <- (
    length(drift$colnames_drift$missing_std) > 0 ||
      length(drift$config_drift$missing_moderators) > 0 ||
      length(drift$config_drift$added) > 0
  )

  if (!drift$has_drift) {
    emit_reconcile_complete()
    return(invisible(NULL))
  }

  # Strict mode: abort with a structured message
  if (mode == "strict") {
    msgs <- c("x" = "Dataset schema does not match the stored configuration.")
    if (length(drift$colnames_drift$missing_std) > 0) {
      missing_raw <- drift$colnames_drift$missing_raw
      msgs <- c(msgs, "i" = cli::format_inline(
        "Missing required column{?s}: {.val {unname(missing_raw)}}"
      ))
    }
    if (length(drift$config_drift$missing_moderators) > 0) {
      msgs <- c(msgs, "i" = cli::format_inline(
        "Missing moderator{?s}: {.val {drift$config_drift$missing_moderators}}"
      ))
    }
    if (length(drift$config_drift$added) > 0) {
      msgs <- c(msgs, "i" = cli::format_inline(
        "New column{?s} not in config: {.val {drift$config_drift$added}}"
      ))
    }
    msgs <- c(msgs,
      "i" = "Use {.code reconcile = \"ask\"} for interactive resolution.",
      "i" = "Use {.code reconcile = \"auto\"} for automatic resolution."
    )
    cli::cli_abort(msgs)
  }

  # Compute "unmatched" columns available for fuzzy matching
  map_clean <- colnames_map[!vapply(
    colnames_map,
    function(x) is.null(x) || (length(x) == 1 && is.na(x)),
    logical(1)
  )]
  matched_cols <- make.names(c(
    unlist(map_clean),
    setdiff(names(config_overrides), names(map_clean))
  ))
  # Missing cols are the ones whose stored names are absent; exclude them from matched
  missing_raw_cols <- drift$colnames_drift$missing_raw
  matched_cols <- setdiff(matched_cols, make.names(unlist(missing_raw_cols)))
  available_cols <- setdiff(make.names(colnames(raw_df)), matched_cols)

  # Fuzzy proposals for required and moderator columns
  proposals_required <- propose_renames(drift$colnames_drift$missing_raw, available_cols)
  proposals_moderators <- propose_renames(
    stats::setNames(
      make.names(drift$config_drift$missing_moderators),
      drift$config_drift$missing_moderators
    ),
    available_cols
  )

  # Show unified diff
  show_drift_summary(drift, proposals_required, proposals_moderators, colnames_map)

  # Collect decisions
  do_prompt <- (mode == "ask") && should_prompt_user(required_level = "autonomous")

  if (mode == "auto" || !do_prompt) {
    decisions <- auto_decisions(drift, proposals_required, proposals_moderators)
  } else {
    decisions <- ask_decisions(drift, proposals_required, proposals_moderators, raw_df)
    confirm_decisions(decisions, drift, colnames_map)
  }

  # Apply
  apply_reconciliation(decisions)
  persist_expected_schema_cols(current_schema_cols)

  emit_reconcile_complete()

  invisible(NULL)
}

box::export(reconcile_schema, detect_schema_drift)
