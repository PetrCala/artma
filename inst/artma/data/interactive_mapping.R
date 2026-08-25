#' @title Format mapping for display
#' @description Helper function to format column mapping for user display
#' @param mapping *\[list\]* The column mapping (std_col -> data_col)
#' @param required_cols *\[character\]* Required column names
#' @param all_std_cols *\[character\]* All standard column names (required + optional)
#' @return *\[list\]* List with 'required' and 'optional' mappings
format_mapping_display <- function(mapping, required_cols, all_std_cols = NULL) {
  if (is.null(all_std_cols)) {
    all_std_cols <- names(mapping)
  }

  required_mapping <- mapping[names(mapping) %in% required_cols]
  optional_mapping <- mapping[names(mapping) %in% setdiff(all_std_cols, required_cols)]

  list(
    required = required_mapping,
    optional = optional_mapping
  )
}


#' @title Present detected column mapping to user
#' @description Show detected columns to user and get their confirmation choice
#' @param auto_mapping *\[list\]* Automatically detected column mapping
#' @param df *\[data.frame\]* The data frame
#' @param required_cols *\[character\]* Required column names
#' @param all_std_cols *\[character\]* All standard column names
#' @return *\[character\]* User's choice: "accept", "modify", or "skip_optional"
present_detected_mapping <- function(
  auto_mapping,
  df,
  required_cols,
  all_std_cols = NULL
) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (length(auto_mapping) == 0) {
    return("modify")
  }

  if (is.null(all_std_cols)) {
    box::use(artma / data / column_recognition[get_column_patterns])
    patterns <- get_column_patterns()
    all_std_cols <- names(patterns)
  }

  # Format mapping for display
  formatted <- format_mapping_display(auto_mapping, required_cols, all_std_cols)

  if (get_verbosity() >= 3) {
    cli::cli_h2("Detected Column Mapping")
    cli::cli_inform("We have detected the following columns in your data:")
    cli::cli_par()

    # Show required columns
    if (length(formatted$required) > 0) {
      cli::cli_inform("{.strong Required columns:}")
      for (std_col in names(formatted$required)) {
        cli::cli_inform("  {cli::symbol$bullet} {.field {std_col}} {cli::symbol$arrow_right} {.val {formatted$required[[std_col]]}}")
      }
    }

    # Show optional columns
    if (length(formatted$optional) > 0) {
      cli::cli_inform("{.strong Optional columns:}")
      for (std_col in names(formatted$optional)) {
        cli::cli_inform("  {cli::symbol$bullet} {.field {std_col}} {cli::symbol$arrow_right} {.val {formatted$optional[[std_col]]}}")
      }
    }

    cli::cli_par()
  }

  # Present choices to user
  cli::cli_inform("What would you like to do?")
  choices <- c(
    "Accept all detected columns",
    "Modify mappings",
    "Skip optional columns (keep only required)"
  )

  choice <- climenu::menu(choices = choices)

  if (is.null(choice)) {
    cli::cli_abort("Column mapping cancelled by user")
  }

  if (grepl("Accept", choice, fixed = TRUE)) {
    return("accept")
  } else if (grepl("Modify", choice, fixed = TRUE)) {
    return("modify")
  } else if (grepl("Skip", choice, fixed = TRUE)) {
    return("skip_optional")
  }

  # Default to modify
  "modify"
}


#' @title Describe the evidence behind a provisional candidate
#' @description Turns one entry of the `provisional` attribute produced by
#'   `recognize_columns()` into plain lines a user can judge: how much the
#'   values look like the role, how complete the column is, whether it agrees
#'   with the column already mapped to its counterpart role, and how far ahead
#'   of the runner-up it sits.
#' @param entry *\[list\]* One provisional candidate.
#' @return *\[character\]* Evidence lines, most telling first.
format_provisional_evidence <- function(entry) {
  lines <- character(0)
  summary <- entry$summary

  if (!is.null(entry$evidence) && !is.na(entry$evidence)) {
    lines <- c(lines, sprintf(
      "its values look like %s values (evidence %.2f out of 1)", entry$role, entry$evidence
    ))
  }
  if (is.list(summary)) {
    if (!is.null(summary$coverage) && !is.na(summary$coverage) &&
      !is.null(summary$n_distinct) && !is.na(summary$n_distinct)) {
      lines <- c(lines, sprintf(
        "%.0f%% of rows populated, %d distinct values",
        100 * summary$coverage, summary$n_distinct
      ))
    }
    if (!is.null(summary$non_integer_share) && !is.na(summary$non_integer_share)) {
      lines <- c(lines, sprintf(
        "%.0f%% of the values are not whole numbers",
        100 * summary$non_integer_share
      ))
    }
  }
  if (!is.null(entry$pair_consistency) && !is.na(entry$pair_consistency) &&
    !is.null(entry$pair_with) && !is.na(entry$pair_with)) {
    lines <- c(lines, sprintf(
      "consistent with the mapped column %s on %.0f%% of rows",
      entry$pair_with, 100 * entry$pair_consistency
    ))
  }
  if (!is.null(entry$name_score) && !is.na(entry$name_score) && entry$name_score <= 0) {
    lines <- c(lines, "its name carries no signal for this role, which is why it was not accepted on its own")
  }
  alternatives <- entry$alternative_summaries
  if (is.list(alternatives) && length(alternatives) > 0) {
    for (alt in alternatives) {
      lines <- c(lines, sprintf(
        "%s is just as plausible on the values (evidence %.2f%s)",
        alt$column,
        alt$evidence,
        if (is.null(alt$pair_consistency) || is.na(alt$pair_consistency)) {
          ""
        } else {
          sprintf(", pairs on %.0f%% of rows", 100 * alt$pair_consistency)
        }
      ))
    }
  } else {
    has_runner_up <- !is.null(entry$runner_up) && !is.na(entry$runner_up) &&
      !is.null(entry$margin) && is.finite(entry$margin)
    if (has_runner_up) {
      lines <- c(lines, sprintf(
        "ahead of the next candidate (%s) by %.2f", entry$runner_up, entry$margin
      ))
    }
  }

  lines
}


#' @title Default menu used to confirm a provisional candidate
#' @description Prints the question and asks it via climenu. Factored out so
#'   `confirm_provisional_mappings()` can be driven without a terminal.
#' @param choices *\[character\]* Menu entries.
#' @param prompt *\[character\]* The question to print above the menu.
#' @return *\[character or NULL\]* The selected entry, or `NULL` when the user
#'   cancels.
#' @keywords internal
provisional_menu <- function(choices, prompt) {
  cli::cli_inform(prompt)
  climenu::menu(choices = choices)
}


#' @title Confirm sub-threshold column candidates with the user
#' @description Recognition declines a candidate whose name carries no signal
#'   even when its values and its agreement with the rest of the data say it is
#'   the right column (a bare `eis` column holding the effect size). Accepting
#'   such a column on its own is how identifier columns used to slip in, so the
#'   automatic path must stay strict; this asks instead. Each candidate costs
#'   one question, and confirming a rename persists through the per-column
#'   store, so the question is asked once per dataset. Near-ties (two columns
#'   equally plausible for one role) are asked the same way rather than
#'   resolved silently; keeping the auto-detected column there stores nothing,
#'   so that one question can come back on a later run.
#'
#'   `interactive()` is the hard gate: outside an interactive session the
#'   mapping is returned untouched.
#' @param mapping *\[list\]* The mapping so far (std_col -> data_col).
#' @param provisional *\[list, optional\]* Candidates from the `provisional`
#'   attribute of `recognize_columns()`. Defaults to that attribute.
#' @param allow_ties *\[logical, optional\]* Whether to ask about near-ties as
#'   well as unmapped roles. Defaults to TRUE.
#' @param select_fn *\[function, optional\]* Menu function receiving
#'   `(choices, prompt)` and returning the selected entry (or `NULL` when
#'   cancelled). Injectable for testing; defaults to a climenu menu.
#' @param is_interactive *\[logical, optional\]* Whether the session is
#'   interactive. Injectable for testing; defaults to `interactive()`.
#' @return *\[list\]* The mapping, with confirmed candidates added or swapped in.
confirm_provisional_mappings <- function(
  mapping,
  provisional = attr(mapping, "provisional"),
  allow_ties = TRUE,
  select_fn = NULL,
  is_interactive = interactive()
) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity]
  )

  validate(is.list(mapping))

  if (!isTRUE(is_interactive) || !is.list(provisional) || length(provisional) == 0) {
    return(mapping)
  }

  if (is.null(select_fn)) select_fn <- provisional_menu

  for (role in names(provisional)) {
    entry <- provisional[[role]]
    if (!is.list(entry) || is.null(entry$column)) next

    kind <- if (is.null(entry$kind)) "unmapped" else entry$kind
    if (identical(kind, "tie") && !isTRUE(allow_ties)) next
    if (identical(kind, "unmapped") && role %in% names(mapping)) next

    # Never offer a column another role already holds.
    taken <- setdiff(unlist(mapping, use.names = FALSE), mapping[[role]])
    candidates <- setdiff(c(entry$column, entry$alternatives), taken)
    if (length(candidates) == 0) next

    if (get_verbosity() >= 3) {
      cli::cli_h2("Confirm the {.field {role}} column")
      for (line in format_provisional_evidence(entry)) {
        cli::cli_inform("  {cli::symbol$bullet} {line}")
      }
    }

    if (identical(kind, "tie")) {
      prompt <- sprintf(
        "Two columns are equally plausible as '%s'. Which one holds it?", role
      )
      choices <- candidates
    } else if (length(candidates) == 1) {
      prompt <- sprintf("Map '%s' to the column '%s'?", role, candidates[1])
      choices <- c(candidates, sprintf("--- No, leave %s unmapped ---", role))
    } else {
      # Twins the recognizer cannot separate on the evidence (est / LB / UB,
      # winsorized and raw copies of one column): the user picks.
      prompt <- sprintf("Which column holds '%s'?", role)
      choices <- c(candidates, sprintf("--- None of these, leave %s unmapped ---", role))
    }

    selected <- select_fn(choices, prompt)

    # Cancelling declines the suggestion: the mapping keeps whatever the
    # automatic path decided.
    if (is.null(selected) || length(selected) != 1 || is.na(selected)) next
    if (!selected %in% candidates) next

    mapping[[role]] <- selected
    if (get_verbosity() >= 3) {
      cli::cli_alert_success("Mapped {.field {role}} to {.val {selected}}")
    }
  }

  mapping
}


#' @title Confirm the derived (t, df) route with the user
#' @description Recognition proposes deriving `effect` and `se` as partial
#'   correlations when it finds a t-statistic column, a degrees-of-freedom
#'   companion, and no canonically named effect/standard-error pair (see
#'   `artma / data / derivation[detect_tdf_derivation]`). The choice between
#'   the derived quantity and the coefficient columns the file happens to carry
#'   is the analyst's, not the detector's, so an interactive session asks.
#'
#'   Outside an interactive session the proposal stands: recognition already
#'   declined the columns it displaced, and the pair it found was named in the
#'   log.
#' @param mapping *\[list\]* The mapping so far (std_col -> data_col).
#' @param derivation *\[list, optional\]* The `derivation` attribute of
#'   `recognize_columns()`. Defaults to that attribute.
#' @param select_fn *\[function, optional\]* Menu function receiving
#'   `(choices, prompt)` and returning the selected entry (or `NULL` when
#'   cancelled). Injectable for testing; defaults to a climenu menu.
#' @param is_interactive *\[logical, optional\]* Whether the session is
#'   interactive. Injectable for testing; defaults to `interactive()`.
#' @return *\[list\]* With `mapping` (the t-statistic and degrees-of-freedom
#'   columns added when the route is taken, the displaced effect/se columns
#'   restored when it is refused) and `derivation` (`NULL` when refused).
confirm_derivation <- function(
  mapping,
  derivation = attr(mapping, "derivation"),
  select_fn = NULL,
  is_interactive = interactive()
) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity]
  )

  validate(is.list(mapping))

  if (!is.list(derivation) || is.null(derivation$t_stat) || is.null(derivation$dof)) {
    return(list(mapping = mapping, derivation = NULL))
  }

  take_route <- function() {
    mapping[["t_stat"]] <- derivation$t_stat
    mapping[["reg_dof"]] <- derivation$dof
    list(mapping = mapping, derivation = derivation)
  }

  if (!isTRUE(is_interactive)) {
    return(take_route())
  }

  if (is.null(select_fn)) select_fn <- provisional_menu

  displaced <- unlist(derivation$replaces, use.names = FALSE)
  derive_choice <- sprintf(
    "Derive them from '%s' and '%s'", derivation$t_stat, derivation$dof
  )
  keep_choice <- if (length(displaced) > 0) {
    sprintf("Read them from %s instead", paste(sprintf("'%s'", displaced), collapse = " and "))
  } else {
    NULL
  }
  leave_choice <- "--- Neither, leave effect and se unmapped ---"

  if (get_verbosity() >= 3) {
    cli::cli_h2("Confirm how {.field effect} and {.field se} are obtained")
    cli::cli_inform("  {cli::symbol$bullet} {.field {derivation$t_stat}} holds t-statistics and {.field {derivation$dof}} holds degrees of freedom")
    cli::cli_inform("  {cli::symbol$bullet} together they give the partial correlation r = t / sqrt(t^2 + df) and its standard error")
    if (length(displaced) > 0) {
      cli::cli_inform("  {cli::symbol$bullet} {.val {displaced}} match{?es/} by name only, without the canonical spelling of {?its/their} role")
    }
  }

  selected <- select_fn(
    c(derive_choice, keep_choice, leave_choice),
    "How should 'effect' and 'se' be obtained?"
  )

  # Cancelling declines to answer: the proposal recognition made stands.
  if (is.null(selected) || length(selected) != 1 || is.na(selected)) {
    return(take_route())
  }

  if (identical(selected, derive_choice)) {
    return(take_route())
  }

  if (!is.null(keep_choice) && identical(selected, keep_choice)) {
    for (role in names(derivation$replaces)) {
      mapping[[role]] <- derivation$replaces[[role]]
    }
  }

  list(mapping = mapping, derivation = NULL)
}


#' @title Interactive column mapping with climenu
#' @description Allow users to interactively map columns using climenu
#' @param df *\[data.frame\]* The data frame
#' @param auto_mapping *\[list\]* Automatically recognized mapping
#' @param required_only *\[logical\]* If TRUE, only ask for required columns
#' @param show_detected_first *\[logical\]* If TRUE, show detected columns first for confirmation
#' @param is_interactive *\[logical, optional\]* Whether the session is
#'   interactive. Injectable for testing; defaults to `interactive()`.
#' @param provisional *\[list, optional\]* Sub-threshold candidates to confirm
#'   with the user, as produced by `recognize_columns()`. Defaults to the
#'   `provisional` attribute of `auto_mapping`.
#' @param select_fn *\[function, optional\]* Menu function used for the
#'   provisional confirmations. Injectable for testing.
#' @param derived_roles *\[character, optional\]* Required roles the pipeline
#'   computes rather than reads, so they are neither prompted for nor reported
#'   as missing (see `artma / data / derivation`). Defaults to none.
#' @return *\[list\]* User-confirmed column mapping
interactive_column_mapping <- function(df, auto_mapping = list(), required_only = TRUE, show_detected_first = FALSE, is_interactive = interactive(), provisional = attr(auto_mapping, "provisional"), select_fn = NULL, derived_roles = character(0)) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / autonomy[should_prompt_user],
    artma / libs / core / log[log_warn],
    artma / data / column_recognition[
      get_required_column_names,
      get_column_patterns
    ]
  )

  validate(is.data.frame(df), is.list(auto_mapping))

  # Determine which columns to ask about
  patterns <- get_column_patterns()
  required_cols <- get_required_column_names()
  all_std_cols <- names(patterns)

  cols_to_map <- if (required_only) {
    required_cols
  } else {
    all_std_cols
  }

  # Roles the pipeline derives are not missing, however unmapped they look.
  required_cols <- setdiff(required_cols, derived_roles)
  cols_to_map <- setdiff(cols_to_map, derived_roles)

  # Track missing required columns
  missing_required <- setdiff(required_cols, names(auto_mapping))

  if (!should_prompt_user(required_level = "autonomous", is_interactive = is_interactive)) {
    if (get_verbosity() >= 3) {
      cli::cli_inform("Autonomy level is high - using auto-detected column mappings")
    }
    # If all required columns are present, return auto_mapping
    if (length(missing_required) == 0) {
      return(auto_mapping)
    }
    # Some required columns could not be auto-detected. Outside an interactive
    # session the menus below cannot ask; climenu's fallback would pick the
    # first entry, silently mapping e.g. n_obs to effect. Leave the columns
    # unmapped instead: methods that need them are skipped at runtime.
    if (!is_interactive) {
      log_warn(paste(
        "Could not auto-detect required column{?s} {.field {missing_required}}",
        "and cannot prompt for a mapping in a non-interactive session.",
        "Leaving {?it/them} unmapped; methods that need {?it/them} will be skipped.",
        "To map {?it/them} manually, set {.code data.columns} in the options file."
      ))
      return(auto_mapping)
    }
    # Interactive session at autonomous level: fall through so the user can
    # resolve the missing required columns via the prompts below.
  }

  # Candidates recognition declined to accept on its own, plus near-ties it
  # would otherwise resolve silently. One question each, before the mapping is
  # presented, so a confirmed column shows up as a normal detected mapping.
  # Near-ties are asked only where the autonomy level asks about non-critical
  # choices at all; a declined required role is asked about whenever we are
  # already prompting for it.
  if (is.list(provisional) && length(provisional) > 0) {
    auto_mapping <- confirm_provisional_mappings(
      mapping = auto_mapping,
      provisional = provisional,
      allow_ties = should_prompt_user(required_level = "autonomous", is_interactive = is_interactive),
      select_fn = select_fn,
      is_interactive = is_interactive
    )
    missing_required <- setdiff(required_cols, names(auto_mapping))
  }

  # Track user's choice from the initial presentation
  user_choice <- NULL

  # If show_detected_first is TRUE and we have detected columns, present them first
  if (show_detected_first && length(auto_mapping) > 0) {
    user_choice <- present_detected_mapping(
      auto_mapping = auto_mapping,
      df = df,
      required_cols = required_cols,
      all_std_cols = all_std_cols
    )

    if (user_choice == "accept") {
      # User accepted all detected columns
      if (get_verbosity() >= 3) {
        cli::cli_alert_success("Accepted all detected column mappings")
      }
      # Still need to check if required columns are missing
      if (length(missing_required) == 0) {
        return(auto_mapping)
      }
      # Fall through to prompt for missing required columns only
      mapping <- auto_mapping
    } else if (user_choice == "skip_optional") {
      # Remove optional columns, keep only required
      mapping <- auto_mapping[names(auto_mapping) %in% required_cols]
      if (get_verbosity() >= 3) {
        cli::cli_alert_info("Skipped optional columns, keeping only required mappings")
      }
      # Recalculate missing required after removing optional
      missing_required <- setdiff(required_cols, names(mapping))
      # Fall through to check for missing required
    } else {
      # User chose to modify - continue with interactive mapping
      mapping <- auto_mapping
    }
  } else {
    # Not showing detected first, use auto_mapping as starting point
    mapping <- auto_mapping
  }

  # If all required columns are present and user accepted, return early
  if (length(missing_required) == 0 && required_only && !show_detected_first) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_success("All required columns automatically recognized")
    }
    return(mapping)
  }

  # If user accepted all and all required are present, return early
  if (!is.null(user_choice) && user_choice == "accept" && length(missing_required) == 0) {
    return(mapping)
  }

  # Only show interactive mapping messages if user didn't accept or if there are missing required
  should_show_interactive <- is.null(user_choice) ||
    (user_choice != "accept" && user_choice != "skip_optional") ||
    length(missing_required) > 0

  if (should_show_interactive && get_verbosity() >= 3) {
    cli::cli_alert_info("Interactive column mapping")
    if (length(mapping) > 0) {
      cli::cli_inform("Current mappings: {.field {paste(names(mapping), collapse = ', ')}}")
    }
    if (length(missing_required) > 0) {
      cli::cli_inform("Missing required: {.field {paste(missing_required, collapse = ', ')}}")
    }
  }

  available_cols <- names(df)

  # Only show modification section if user explicitly chose to modify
  # Skip if user accepted or skipped optional (unless there are missing required)
  should_show_modify <- show_detected_first &&
    length(mapping) > 0 &&
    !is.null(user_choice) &&
    user_choice == "modify"

  # If user chose to modify, allow editing existing mappings first
  if (should_show_modify) {
    cli::cli_h2("Modify Column Mappings")
    cli::cli_inform("You can modify any of the detected mappings or skip to keep them as-is.")

    # Use multi-select to allow selecting multiple columns to modify
    modify_choices <- names(mapping)
    keep_all_option <- "--- Keep all current mappings (skip modifications) ---"
    modify_all_option <- "--- Modify all mappings ---"

    cli::cli_inform("Select columns to modify (use SPACE to select, ENTER to confirm)")
    selected_indices <- climenu::checkbox(
      choices = c(modify_choices, keep_all_option, modify_all_option),
      prompt = "Select columns to modify, or keep/modify all",
      return_index = TRUE
    )

    if (length(selected_indices) == 0) {
      cli::cli_abort("Column mapping cancelled by user")
    }

    selected_items <- c(modify_choices, keep_all_option, modify_all_option)[selected_indices]

    # Check if user selected "Keep all" option
    if (keep_all_option %in% selected_items) {
      # User wants to keep all mappings as-is, skip modifications
      if (get_verbosity() >= 3) {
        cli::cli_alert_success("Keeping all detected mappings as-is")
      }
      # Continue to check for missing required columns
    } else if (modify_all_option %in% selected_items) {
      # Clear all mappings and re-map everything
      mapping <- list()
      missing_required <- required_cols
    } else {
      # User selected specific columns to modify
      columns_to_modify <- intersect(selected_items, modify_choices)

      if (length(columns_to_modify) > 0) {
        # Loop through each selected column and allow modification
        for (std_col_to_modify in columns_to_modify) {
          pattern_def <- patterns[[std_col_to_modify]]

          cli::cli_h2("Modify mapping: {.field {std_col_to_modify}}")
          cli::cli_inform("Current mapping: {.val {mapping[[std_col_to_modify]]}}")

          # Safely get examples from keywords
          if (!is.null(pattern_def$keywords) && length(pattern_def$keywords) > 0) {
            n_examples <- min(3, length(pattern_def$keywords))
            examples <- pattern_def$keywords[seq_len(n_examples)]
            cli::cli_inform("Examples: {.val {examples}}")
          }

          # Add current mapping back to available if it was removed
          current_mapped <- mapping[[std_col_to_modify]]
          if (!is.null(current_mapped) && !current_mapped %in% available_cols) {
            available_cols <- c(available_cols, current_mapped)
          }

          choices <- c(
            available_cols,
            "--- Keep current mapping ---",
            "--- Remove this mapping ---"
          )

          cli::cli_inform("Select new column for '{std_col_to_modify}'")
          selected <- climenu::menu(choices = choices)

          if (is.null(selected)) {
            cli::cli_abort("Column mapping cancelled by user")
          }

          if (is.na(selected) || !nzchar(trimws(selected))) {
            cli::cli_abort("Invalid column selection: received NA or empty value")
          }

          if (grepl("Keep current", selected, fixed = TRUE)) {
            # Keep as-is, do nothing
          } else if (grepl("Remove", selected, fixed = TRUE)) {
            # Remove this mapping
            mapping <- mapping[names(mapping) != std_col_to_modify]
            if (std_col_to_modify %in% required_cols) {
              missing_required <- unique(c(missing_required, std_col_to_modify))
            }
          } else {
            # Update mapping - ensure selected is a valid non-empty string
            selected_clean <- trimws(selected)
            if (!nzchar(selected_clean)) {
              cli::cli_abort("Cannot map {.field {std_col_to_modify}} to an empty column name")
            }
            mapping[[std_col_to_modify]] <- selected_clean
            available_cols <- setdiff(available_cols, selected_clean)
          }
        }
      }
    }
  }

  # Ask for each missing required column
  for (std_col in missing_required) {
    pattern_def <- patterns[[std_col]]

    cli::cli_h2("Map column: {.field {std_col}}")
    # Safely get examples from keywords
    if (!is.null(pattern_def$keywords) && length(pattern_def$keywords) > 0) {
      n_examples <- min(3, length(pattern_def$keywords))
      examples <- pattern_def$keywords[seq_len(n_examples)]
      cli::cli_inform("Examples: {.val {examples}}")
    }

    # Add "Skip (column not present)" and "None of these" options
    choices <- c(
      available_cols,
      "--- Skip (column not present) ---",
      "--- None of these ---"
    )

    cli::cli_inform("Select the column for '{std_col}' (required)")
    selected <- climenu::menu(choices = choices)

    if (is.null(selected)) {
      cli::cli_abort("Column mapping cancelled by user")
    }

    if (grepl("^---.*Skip.*---$", selected)) {
      if (std_col %in% required_cols) {
        cli::cli_alert_warning("Skipping required column {.field {std_col}}. This may cause errors later.")
      }
      next
    } else if (grepl("^---.*None.*---$", selected)) {
      # Ask user to type column name manually
      typed_col <- readline(sprintf("Enter the exact column name for '%s': ", std_col))
      typed_col <- trimws(typed_col)

      if (nchar(typed_col) == 0) {
        cli::cli_alert_warning("No column name provided, skipping {.field {std_col}}")
        next
      }

      if (!typed_col %in% available_cols) {
        cli::cli_alert_danger("Column {.val {typed_col}} not found in data frame. Skipping.")
        next
      }

      selected <- typed_col
    }

    mapping[[std_col]] <- selected

    # Remove from available columns to avoid duplicate mapping
    available_cols <- setdiff(available_cols, selected)
  }

  # Optionally ask for optional columns
  if (!required_only) {
    optional_cols <- setdiff(cols_to_map, c(names(mapping), missing_required))

    if (length(optional_cols) > 0 && length(available_cols) > 0) {
      cli::cli_h2("Optional columns")
      cli::cli_inform("Would you like to map optional columns? (y/n)")

      response <- tolower(trimws(readline("Map optional columns? [y/N]: ")))

      if (response %in% c("y", "yes")) {
        for (std_col in optional_cols) {
          pattern_def <- patterns[[std_col]]

          choices <- c(
            available_cols,
            "--- Skip ---"
          )

          cli::cli_inform("Select the column for '{std_col}' (optional)")
          selected <- climenu::menu(choices = choices)

          if (is.null(selected) || grepl("^---.*Skip.*---$", selected)) {
            next
          }
          mapping[[std_col]] <- selected
          available_cols <- setdiff(available_cols, selected)
        }
      }
    }
  }

  # Validate all mapping values before returning
  # Remove any NULL, NA, or empty string values and warn user
  invalid_mappings <- character(0)
  for (std_col in names(mapping)) {
    val <- mapping[[std_col]]
    if (is.null(val) || (length(val) == 1 && is.na(val)) || !nzchar(trimws(val))) {
      invalid_mappings <- c(invalid_mappings, std_col)
      mapping <- mapping[names(mapping) != std_col]
      if (std_col %in% required_cols) {
        missing_required <- unique(c(missing_required, std_col))
      }
    }
  }

  if (length(invalid_mappings) > 0 && get_verbosity() >= 2) {
    cli::cli_alert_warning(
      "Removed invalid mappings for: {.field {paste(invalid_mappings, collapse = ', ')}}"
    )
  }

  mapping
}


#' @title Prompt user to confirm or modify column mapping
#' @description Show the mapping and allow user to confirm or modify
#' @param mapping *\[list\]* The column mapping
#' @param required_cols *\[character\]* Required column names
#' @return *\[list\]* Confirmed mapping
confirm_column_mapping <- function(mapping, required_cols) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 3) {
    cli::cli_h2("Column Mapping Summary")

    for (std_col in names(mapping)) {
      is_req <- if (std_col %in% required_cols) " (required)" else " (optional)"
      cli::cli_inform("{.field {std_col}}{is_req} -> {.val {mapping[[std_col]]}}")
    }

    missing <- setdiff(required_cols, names(mapping))
    if (length(missing) > 0) {
      cli::cli_alert_warning("Missing required columns: {.field {paste(missing, collapse = ', ')}}")
    }
  }

  mapping
}


#' @title Full interactive column mapping workflow
#' @description Complete workflow: recognize, interact, confirm, save
#' @param df *\[data.frame\]* The data frame
#' @param auto_mapping *\[list, optional\]* Pre-computed auto mapping
#' @param options_file_name *\[character, optional\]* Options file to save mapping
#' @param min_confidence *\[numeric\]* Minimum confidence for auto-recognition
#' @param force_interactive *\[logical\]* Force interactive mapping even if all columns recognized
#' @return *\[list\]* Final column mapping
column_mapping_workflow <- function(
  df,
  auto_mapping = NULL,
  options_file_name = NULL,
  min_confidence = 0.7,
  force_interactive = FALSE
) {
  box::use(
    artma / data / column_recognition[
      recognize_columns,
      check_mapping_completeness,
      get_required_column_names
    ],
    artma / data_config / column_mapping[save_column_mapping_to_options],
    artma / libs / core / utils[get_verbosity]
  )

  # Auto-recognize if not provided
  if (is.null(auto_mapping)) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("Automatically recognizing columns...")
    }
    auto_mapping <- recognize_columns(df, min_confidence = min_confidence)
  }

  # Check completeness
  completeness <- check_mapping_completeness(auto_mapping)
  required_cols <- get_required_column_names()

  # Always present detected columns if any were found
  has_detected <- length(auto_mapping) > 0

  if (has_detected) {
    # Present detected columns and get user confirmation
    mapping <- interactive_column_mapping(
      df = df,
      auto_mapping = auto_mapping,
      required_only = TRUE,
      show_detected_first = TRUE
    )
  } else if (force_interactive || !completeness$complete) {
    # No detected columns, need to prompt for missing
    mapping <- interactive_column_mapping(
      df = df,
      auto_mapping = auto_mapping,
      required_only = TRUE,
      show_detected_first = FALSE
    )
  } else {
    # Edge case: no detected columns but all required are somehow present
    # (shouldn't happen, but handle gracefully)
    mapping <- auto_mapping
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("No columns detected, but all required columns are present")
    }
  }

  # Confirm mapping
  mapping <- confirm_column_mapping(mapping, required_cols)

  # Check if we have all required columns
  final_check <- check_mapping_completeness(mapping)
  if (!final_check$complete) {
    cli::cli_abort(c(
      "x" = "Column mapping incomplete",
      "i" = "Missing required columns: {.field {paste(final_check$missing, collapse = ', ')}}"
    ))
  }

  # Save to options if requested
  if (!is.null(options_file_name)) {
    save_column_mapping_to_options(mapping, options_file_name)
  }

  mapping
}


box::export(
  confirm_derivation,
  confirm_provisional_mappings,
  format_provisional_evidence,
  interactive_column_mapping,
  confirm_column_mapping,
  column_mapping_workflow,
  present_detected_mapping,
  format_mapping_display
)
