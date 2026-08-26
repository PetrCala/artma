#' @title Interactive session hub
#' @description
#' The menu loop `artma()` enters when the session is interactive and no
#' methods were requested: one session, many runs. The user picks and runs
#' methods, previews the prepared data, opens results, and leaves only when
#' done. Entry conditions, item list, and the return contract:
#' contributingGuides/HUB.md.
NULL

box::use(
  artma / interactive / input[ask_select, ask_yes_no],
  artma / interactive / method_picker[ask_runtime_methods],
  artma / libs / core / utils[data_viewer_available],
  artma / libs / core / validation[validate],
  artma / modules / methods_table[pad_cell, truncate_cell],
  artma / options / inspect[expand_option_tokens, format_option_value, values_equal],
  artma / options / template[flatten_template_options, read_template],
  artma / paths[PATHS]
)

# Width climenu spends in front of every entry: the cursor mark and the
# separating spaces.
MENU_PREFIX_WIDTH <- 6L

# Spaces between the item name and description columns of a label.
COLUMN_GAP <- 2L

# The description column never shrinks below this, even in a narrow console;
# climenu truncates any overflowing line to the console width itself.
MIN_DESCRIPTION_WIDTH <- 12L

#' @title Compose value-keyed hub menu choices
#' @description
#' Build the named character vector `ask_select()` consumes: names are the
#' rendered labels (item name padded to a fixed column, dim description
#' truncated to the remaining width), values are stable action keys, so the
#' loop always dispatches on values and never on labels.
#' @param items *\[list\]* One list per item with `value`, `name`, and
#'   `description` entries.
#' @param width *\[numeric, optional\]* Console width to fit the labels into.
#'   Defaults to the detected console width.
#' @return *\[character\]* Action keys, named by their rendered labels.
compose_hub_choices <- function(items, width = NULL) {
  validate(is.list(items) && length(items) > 0L)

  width <- width %||% cli::console_width()

  values <- vapply(items, function(item) item$value, character(1))
  names <- vapply(items, function(item) item$name, character(1))
  descriptions <- vapply(items, function(item) item$description %||% "", character(1))

  name_width <- max(nchar(names))
  description_width <- max(
    MIN_DESCRIPTION_WIDTH,
    width - MENU_PREFIX_WIDTH - name_width - COLUMN_GAP
  )

  gap <- strrep(" ", COLUMN_GAP)
  labels <- vapply(seq_along(names), function(i) {
    label <- pad_cell(names[[i]], name_width)
    if (nzchar(descriptions[[i]])) {
      label <- paste0(
        label, gap,
        cli::col_grey(truncate_cell(descriptions[[i]], description_width))
      )
    }
    label
  }, character(1))

  stats::setNames(values, labels)
}

# The options users actually re-tune between runs, offered first by the
# Adjust-options item; the method groups of the last selection are appended at
# render time. Every path must name a template option or group.
CURATED_OPTION_PATHS <- c(
  "data.winsorization_level",
  "data.na_handling",
  "calc.precision_type",
  "general.seed",
  "output.number_of_decimals",
  "output.report"
)

#' @title Does an option affect data preparation?
#' @description Editing one of these invalidates the prepared frame: the hub
#'   re-prepares the data before the next run. The whole `data.*` group feeds
#'   the read/preprocess/compute pipeline; every other group only affects how
#'   methods run on the already-prepared frame.
#' @param option_name *\[character\]* Dotted option name, without the package
#'   prefix.
#' @return *\[logical\]* Whether the option feeds data preparation.
option_affects_data <- function(option_name) {
  startsWith(option_name, "data.")
}

#' @title Read an option's current session value
#' @param option_name *\[character\]* Dotted option name, without the prefix.
#' @return *\[any\]* The current value, or `NULL` when the option is unset.
#' @keywords internal
option_current_value <- function(option_name) {
  getOption(paste0("artma.", option_name), default = NULL)
}

#' @title An option definition's template default
#' @param def *\[list\]* A flattened template option definition.
#' @return *\[any\]* The default, `NA` for default-less options that allow it,
#'   or `NULL` for required options.
#' @keywords internal
option_template_default <- function(def) {
  if ("default" %in% names(def)) {
    return(def$default)
  }
  if (isTRUE(def$allow_na)) {
    return(NA)
  }
  NULL
}

#' @title The curated adjustable option definitions
#' @description The curated paths first, then the leaves of the method-specific
#'   option groups (`methods.<name>`) of the given selection. Paths or groups
#'   the template does not define are dropped silently, so a method without
#'   options simply contributes nothing.
#' @param last_methods *\[character\]* The methods of the last confirmed
#'   selection.
#' @param template_path *\[character, optional\]* Path to the options template.
#' @return *\[list\]* Flattened template option definitions, named by option.
adjustable_option_defs <- function(last_methods = character(0), template_path = NULL) {
  validate(is.character(last_methods))
  template_path <- template_path %||% PATHS$FILE_OPTIONS_TEMPLATE

  defs <- flatten_template_options(read_template(template_path))
  names(defs) <- vapply(defs, function(def) def$name, character(1))

  tokens <- c(CURATED_OPTION_PATHS, if (length(last_methods) > 0L) {
    paste0("methods.", unique(last_methods))
  })
  defs[expand_option_tokens(tokens, names(defs))$matched]
}

#' @title Describe an option's current value against its template default
#' @description The description column of the adjust-options menus: always the
#'   current value, plus the template default whenever the current value
#'   deviates from it.
#' @param def *\[list\]* A flattened template option definition.
#' @return *\[character\]* A single-line description.
describe_option_state <- function(def) {
  current <- option_current_value(def$name)
  default <- option_template_default(def)

  current_display <- if (is.null(current)) "(unset)" else format_option_value(current)
  if (!is.null(current) && values_equal(current, default)) {
    return(sprintf("current: %s (default)", current_display))
  }

  default_display <- if (is.null(default) && !("default" %in% names(def))) {
    "(required)"
  } else {
    format_option_value(default)
  }
  sprintf("current: %s (default: %s)", current_display, default_display)
}

#' @title Menu items for a set of option definitions
#' @description One item per option (value and name are the dotted option name,
#'   the description its current-value line), a Browse entry when asked for,
#'   and a closing Back entry. The non-option values are dot-prefixed, which no
#'   template option name can collide with.
#' @param defs *\[list\]* Flattened template option definitions.
#' @param include_browse *\[logical\]* Whether to offer the browse-all entry.
#' @return *\[list\]* Items for `compose_hub_choices()`.
#' @keywords internal
option_menu_items <- function(defs, include_browse = TRUE) {
  items <- lapply(unname(defs), function(def) {
    list(value = def$name, name = def$name, description = describe_option_state(def))
  })
  if (isTRUE(include_browse)) {
    items <- c(items, list(list(
      value = ".browse",
      name = "Browse all options",
      description = "walk the full option template"
    )))
  }
  c(items, list(list(
    value = ".back",
    name = "Back",
    description = "return to the previous menu"
  )))
}

#' @title Walk the template tree to a single option definition
#' @description Two menus: the template's top-level groups, then the options of
#'   the chosen group with their current values. A cancel at either level backs
#'   out without a choice.
#' @param all_defs *\[list\]* Every flattened template definition, named by
#'   option.
#' @param select_fn *\[function\]* Menu backend.
#' @param width *\[numeric, optional\]* Console width for the option menu.
#' @return *\[list\]* The chosen option definition, or `NULL`.
#' @keywords internal
browse_for_option <- function(all_defs, select_fn, width = NULL) {
  all_names <- names(all_defs)
  sections <- unique(vapply(
    strsplit(all_names, ".", fixed = TRUE),
    function(parts) parts[[1]],
    character(1)
  ))

  section <- ask_select(
    question = "Which option group?",
    choices = stats::setNames(sections, sections),
    confirm = FALSE,
    select_fn = select_fn
  )
  if (rlang::is_empty(section)) {
    return(NULL)
  }

  in_section <- all_defs[all_names == section | startsWith(all_names, paste0(section, "."))]
  choice <- ask_select(
    question = sprintf("Which %s option?", section),
    choices = compose_hub_choices(
      option_menu_items(in_section, include_browse = FALSE),
      width = width
    ),
    confirm = FALSE,
    select_fn = select_fn
  )
  if (rlang::is_empty(choice) || identical(choice, ".back")) {
    return(NULL)
  }
  all_defs[[choice]]
}

#' @title The hub's Adjust-options loop
#' @description
#' A picker over the curated options (current values and template defaults in
#' the labels), with a browse-all fallback over the whole template. Each chosen
#' option is edited through the template prompt (typed validation, defaults,
#' hints), applied session-wide via `options()` immediately, and offered for
#' persistence to the options YAML through the save-preference prompt;
#' declining keeps the edit session-only and the YAML untouched.
#' @param last_methods *\[character\]* The last confirmed selection, whose
#'   method-specific option groups join the curated list.
#' @param select_fn *\[function, optional\]* Menu backend. Exposed for testing.
#' @param edit_option *\[function, optional\]* Called with an option definition;
#'   returns the validated new value, or `NULL` to abort the edit. Defaults to
#'   `prompt_user_for_option_value()` (with the current value as the default)
#'   followed by the template coercion. Injectable for testing.
#' @param save_preference *\[function, optional\]* Called with the option name
#'   and the new value; decides session-only vs write to the options YAML.
#'   Defaults to `prompt_save_preference()`. Injectable for testing.
#' @param width *\[numeric, optional\]* Console width for the menus.
#' @param template_path *\[character, optional\]* Path to the options template.
#' @return *\[list\]* `changed` (the dotted names of the options whose values
#'   changed) and `data_changed` (whether any of them affects data
#'   preparation).
run_adjust_options <- function(
  last_methods = character(0),
  select_fn = climenu::select,
  edit_option = NULL,
  save_preference = NULL,
  width = NULL,
  template_path = NULL
) {
  validate(
    is.character(last_methods),
    is.function(select_fn),
    is.null(edit_option) || is.function(edit_option),
    is.null(save_preference) || is.function(save_preference)
  )

  if (is.null(edit_option)) {
    edit_option <- function(def) {
      box::use(artma / options / template[coerce_option_value, prompt_user_for_option_value])
      # Offer the current value as the default, so <Enter> keeps it.
      current <- option_current_value(def$name)
      if (!is.null(current)) {
        def$default <- current
      }
      coerce_option_value(prompt_user_for_option_value(def), def)
    }
  }
  if (is.null(save_preference)) {
    save_preference <- function(option_name, value) {
      box::use(artma / interactive / save_preference[prompt_save_preference])
      # The user explicitly opened the adjust flow, so the persistence question
      # is part of it; autonomy must not silently swallow it.
      prompt_save_preference(
        option_name, value,
        description = sprintf("the new value of %s", option_name),
        respect_autonomy = FALSE
      )
    }
  }

  template_path <- template_path %||% PATHS$FILE_OPTIONS_TEMPLATE
  all_defs <- flatten_template_options(read_template(template_path))
  names(all_defs) <- vapply(all_defs, function(def) def$name, character(1))
  curated <- adjustable_option_defs(last_methods, template_path)

  changed <- character(0)
  data_changed <- FALSE

  repeat {
    # Rebuilt every pass, so an edit shows up in the labels immediately.
    action <- ask_select(
      question = "Adjust analysis options",
      choices = compose_hub_choices(option_menu_items(curated), width = width),
      confirm = FALSE,
      select_fn = select_fn
    )
    if (rlang::is_empty(action) || identical(action, ".back")) {
      break
    }

    def <- if (identical(action, ".browse")) {
      browse_for_option(all_defs, select_fn = select_fn, width = width)
    } else {
      all_defs[[action]]
    }
    if (is.null(def)) {
      next
    }

    old_value <- option_current_value(def$name)
    new_value <- tryCatch(
      edit_option(def),
      error = function(e) {
        cli::cli_alert_danger("Could not update {.field {def$name}}: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(new_value)) {
      next
    }
    if (!is.null(old_value) && values_equal(old_value, new_value)) {
      cli::cli_alert_info("{.field {def$name}} is unchanged.")
      next
    }

    # Session-wide first: the next run picks the value up whether or not it is
    # also persisted to the YAML below.
    do.call(options, stats::setNames(list(new_value), paste0("artma.", def$name)))
    changed <- unique(c(changed, def$name))
    if (option_affects_data(def$name)) {
      data_changed <- TRUE
    }
    cli::cli_alert_success(
      "{.field {def$name}} set to {.val {format_option_value(new_value)}} for this session."
    )

    save_preference(def$name, new_value)
  }

  list(changed = changed, data_changed = data_changed)
}

#' @title The hub's menu items in display order
#' @description
#' The Re-run item only appears once a run has happened in this hub session;
#' its description names the selection it would repeat, plus the options
#' changed since that run.
#' @param has_run *\[logical\]* Whether a run happened in this session.
#' @param last_methods *\[character\]* The previously confirmed selection.
#' @param options_changed *\[character, optional\]* Options changed since the
#'   previous run.
#' @return *\[list\]* Items for `compose_hub_choices()`.
hub_menu_items <- function(has_run, last_methods, options_changed = character(0)) {
  items <- list(
    list(
      value = "run",
      name = "Run methods",
      description = "pick and run analysis methods"
    )
  )
  if (isTRUE(has_run)) {
    description <- paste(last_methods, collapse = ", ")
    if (length(options_changed) > 0L) {
      description <- sprintf(
        "%s (changed: %s)",
        description, paste(options_changed, collapse = ", ")
      )
    }
    items <- c(items, list(list(
      value = "rerun",
      name = "Re-run last selection",
      description = description
    )))
  }
  c(items, list(
    list(
      value = "options",
      name = "Adjust options",
      description = "change analysis options for the next runs"
    ),
    list(
      value = "preview",
      name = "Preview data",
      description = "summary or spreadsheet view of the prepared data"
    ),
    list(
      value = "results",
      name = "Results",
      description = "open results folder, render HTML report"
    ),
    list(
      value = "exit",
      name = "Exit",
      description = "return results to the R session"
    )
  ))
}

#' @title Count the studies of a prepared frame
#' @description The prepared frame carries the standardized `study_id` column
#'   when the data config resolved a study column; without it the count is
#'   unknown.
#' @param df *\[data.frame\]* The prepared data frame.
#' @return *\[integer\]* The number of distinct studies, or `NA` when unknown.
count_studies <- function(df) {
  if (!("study_id" %in% names(df))) {
    return(NA_integer_)
  }
  ids <- df[["study_id"]]
  length(unique(ids[!is.na(ids)]))
}

#' @title Render the hub header line
#' @description One rule line naming the options file behind the session and
#'   the prepared data's dimensions (studies when the config knows the study
#'   column, plain columns otherwise).
#' @param df *\[data.frame\]* The prepared data frame.
#' @return `NULL`, invisibly.
render_hub_header <- function(df) {
  parts <- "artma session"

  file_name <- getOption("artma.temp.file_name", NULL)
  if (is.character(file_name) && length(file_name) == 1L && nzchar(file_name)) {
    parts <- c(parts, file_name)
  }

  n_studies <- count_studies(df)
  dims <- sprintf("%s rows", format(nrow(df), big.mark = ","))
  if (!is.na(n_studies)) {
    dims <- sprintf("%s, %s studies", dims, format(n_studies, big.mark = ","))
  } else {
    dims <- sprintf("%s, %s columns", dims, format(ncol(df), big.mark = ","))
  }
  parts <- c(parts, dims)

  cli::cat_line()
  cli::cli_rule(left = paste(parts, collapse = " - "))
  invisible(NULL)
}

#' @title Print a textual summary of the prepared frame
#' @description Rows and columns, the study count where the config knows the
#'   study column, missing-value counts, and the effect and standard-error
#'   ranges.
#' @param df *\[data.frame\]* The prepared data frame.
#' @return `NULL`, invisibly.
render_data_summary <- function(df) {
  lines <- c(
    "*" = sprintf(
      "%s rows, %s columns",
      format(nrow(df), big.mark = ","), format(ncol(df), big.mark = ",")
    )
  )

  n_studies <- count_studies(df)
  if (!is.na(n_studies)) {
    lines <- c(lines, "*" = sprintf("%s studies", format(n_studies, big.mark = ",")))
  }

  na_by_column <- vapply(df, function(column) sum(is.na(column)), integer(1))
  na_total <- sum(na_by_column)
  if (na_total == 0L) {
    lines <- c(lines, "*" = "No missing values")
  } else {
    lines <- c(lines, "*" = sprintf(
      "%s missing values across %d columns",
      format(na_total, big.mark = ","), sum(na_by_column > 0L)
    ))
  }

  numeric_range_line <- function(column, label) {
    values <- df[[column]]
    if (is.null(values) || !is.numeric(values) || all(is.na(values))) {
      return(NULL)
    }
    bounds <- range(values, na.rm = TRUE)
    sprintf("%s: %s to %s", label, signif(bounds[[1]], 4), signif(bounds[[2]], 4))
  }

  for (line in list(
    numeric_range_line("effect", "Effect range"),
    numeric_range_line("se", "SE range")
  )) {
    if (!is.null(line)) {
      lines <- c(lines, "*" = line)
    }
  }

  cli::cli_inform(lines)
  invisible(NULL)
}

#' @title Fold one run's results into the accumulated session results
#' @description The accumulated list keeps the latest result per method. The
#'   `failed_methods` and `skipped_methods` attributes track the latest status
#'   per method: a method requested again drops its previous entries before the
#'   new run's entries are appended. The `run_info` attribute always describes
#'   the latest run.
#' @param accumulated *\[list\]* The session's results so far.
#' @param results *\[list\]* One run's results, as `execute_run()` returns them.
#' @return *\[list\]* The updated accumulated results.
merge_run_results <- function(accumulated, results) {
  run_info <- attr(results, "run_info")
  requested <- run_info$methods_requested %||% names(results)

  for (name in names(results)) {
    accumulated[[name]] <- results[[name]]
  }

  for (status in c("failed_methods", "skipped_methods")) {
    previous <- attr(accumulated, status)
    previous <- previous[!(names(previous) %in% requested)]
    merged <- c(previous, attr(results, status))
    attr(accumulated, status) <- if (length(merged) > 0L) merged else NULL
  }

  attr(accumulated, "run_info") <- run_info
  accumulated
}

#' @title Run the interactive session hub
#' @description
#' The menu loop behind interactive `artma()` calls without `methods`. Each
#' "Run methods" pass opens the metadata-decorated method picker and hands the
#' confirmed selection to `run_methods`, which owns the full run pipeline
#' (export, manifest, run summary). Results accumulate across runs (latest
#' result per method); Exit and a cancelled menu both leave the loop and
#' return them.
#'
#' The hub itself is deliberately not gated by `should_prompt_user()`: it is
#' the interactive experience the user asked for. Autonomy keeps governing the
#' prompts inside the flows the hub launches.
#' @param df *\[data.frame\]* The prepared data frame of the session.
#' @param run_methods *\[function\]* Called with the selected method names; runs
#'   them through the full run and summarize steps and returns the run's
#'   results list (carrying the `run_info` attribute).
#' @param rebuild_data *\[function, optional\]* Called with the selected method
#'   names when a data-preparation option changed since the last run; returns
#'   a freshly prepared data frame (and repoints the run pipeline at it).
#'   Without it, a stale frame is kept with a warning.
#' @param edit_option *\[function, optional\]* Passed to
#'   `run_adjust_options()`. Injectable for testing.
#' @param save_preference *\[function, optional\]* Passed to
#'   `run_adjust_options()`. Injectable for testing.
#' @param template_path *\[character, optional\]* Passed to
#'   `run_adjust_options()`. Injectable for testing.
#' @param methods_table *\[data.frame, optional\]* A
#'   `build_methods_table(available_for = df)` frame for the picker. Defaults
#'   to building one on the first "Run methods" pass. Injectable for testing.
#' @param view_data *\[function, optional\]* Opens the prepared frame in a
#'   spreadsheet viewer. Defaults to `artma::data_preview()` on the frame.
#'   Injectable for testing.
#' @param open_results *\[function, optional\]* Opens the results folder.
#'   Defaults to `artma::results_open()`. Injectable for testing.
#' @param render_report *\[function, optional\]* Renders the HTML report from
#'   the accumulated results. Defaults to `artma::report_render()`. Injectable
#'   for testing.
#' @param select_fn *\[function, optional\]* Menu backend for single-choice
#'   menus. Defaults to `climenu::select`. Exposed for testing.
#' @param checkbox_fn *\[function, optional\]* Menu backend for the method
#'   picker. Defaults to `climenu::checkbox`. Exposed for testing.
#' @param width *\[numeric, optional\]* Console width to fit menu labels into.
#'   Defaults to the detected console width.
#' @return *\[list\]* The accumulated results, invisibly: the latest result per
#'   method, with a `runs` attribute holding one entry per run (`methods`,
#'   `seed`, `timestamp`, and `options_changed`, the options edited since the
#'   previous run). Empty when the user exits before any run.
run_session_hub <- function(
  df,
  run_methods,
  rebuild_data = NULL,
  methods_table = NULL,
  view_data = NULL,
  open_results = NULL,
  render_report = NULL,
  select_fn = climenu::select,
  checkbox_fn = climenu::checkbox,
  edit_option = NULL,
  save_preference = NULL,
  width = NULL,
  template_path = NULL
) {
  validate(
    is.data.frame(df),
    is.function(run_methods),
    is.null(rebuild_data) || is.function(rebuild_data),
    is.null(methods_table) || is.data.frame(methods_table),
    is.null(view_data) || is.function(view_data),
    is.null(open_results) || is.function(open_results),
    is.null(render_report) || is.function(render_report),
    is.function(select_fn),
    is.function(checkbox_fn)
  )

  if (is.null(view_data)) {
    view_data <- function(frame) artma::data_preview(data = frame, preprocess = FALSE)
  }
  if (is.null(open_results)) {
    open_results <- function() artma::results_open()
  }
  if (is.null(render_report)) {
    render_report <- function(results) artma::report_render(results)
  }

  # Session state, kept in an environment so the loop's helpers can update it
  # without non-local assignment.
  state <- new.env(parent = emptyenv())
  state$df <- df
  state$methods_frame <- methods_table
  state$accumulated <- list()
  state$runs <- list()
  state$last_methods <- character(0)
  state$has_run <- FALSE
  state$data_stale <- FALSE
  state$changed_since_run <- character(0)

  get_methods_frame <- function() {
    if (is.null(state$methods_frame)) {
      box::use(artma / modules / methods_table[build_methods_table])
      state$methods_frame <- build_methods_table(available_for = state$df)
    }
    state$methods_frame
  }

  # A data option changed since the last preparation: rebuild the frame lazily,
  # right before the run that needs it (the selection decides which columns the
  # preparation must resolve). Returns whether the run may proceed.
  refresh_stale_data <- function(selection) {
    if (!isTRUE(state$data_stale)) {
      return(TRUE)
    }
    if (!is.function(rebuild_data)) {
      cli::cli_alert_warning(
        "Data options changed but this session cannot re-prepare the data; the run uses the existing frame."
      )
      state$data_stale <- FALSE
      return(TRUE)
    }
    cli::cli_alert_info("Data options changed: re-preparing the data for this run.")
    new_df <- tryCatch(
      rebuild_data(selection),
      error = function(e) {
        cli::cli_alert_danger("Re-preparing the data failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.data.frame(new_df)) {
      return(FALSE)
    }
    state$df <- new_df
    state$data_stale <- FALSE
    TRUE
  }

  run_and_record <- function(selection) {
    if (!refresh_stale_data(selection)) {
      return(invisible(NULL))
    }
    results <- tryCatch(
      run_methods(selection),
      error = function(e) {
        cli::cli_alert_danger("The run failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(results)) {
      return(invisible(NULL))
    }

    run_info <- attr(results, "run_info")
    state$accumulated <- merge_run_results(state$accumulated, results)
    state$runs <- c(state$runs, list(list(
      methods = run_info$methods_requested %||% selection,
      seed = run_info$seed,
      timestamp = Sys.time(),
      options_changed = state$changed_since_run
    )))
    state$changed_since_run <- character(0)
    state$has_run <- TRUE
    invisible(NULL)
  }

  repeat {
    render_hub_header(state$df)

    action <- ask_select(
      question = "What would you like to do?",
      choices = compose_hub_choices(
        hub_menu_items(state$has_run, state$last_methods, state$changed_since_run),
        width = width
      ),
      confirm = FALSE,
      select_fn = select_fn
    )

    # A cancelled menu behaves like Exit: the accumulated results are the
    # user's work and must survive an Esc.
    if (rlang::is_empty(action) || identical(action, "exit")) {
      break
    }

    if (identical(action, "run")) {
      selection <- ask_runtime_methods(
        get_methods_frame(),
        default = if (length(state$last_methods) > 0L) {
          state$last_methods
        } else {
          getOption("artma.temp.last_methods", NULL)
        },
        width = width,
        checkbox_fn = checkbox_fn
      )
      if (length(selection) == 0L) {
        cli::cli_alert_info("No methods selected.")
        next
      }
      state$last_methods <- selection
      # Preselect this confirmed selection on the next interactive pick of the
      # session, matching the linear path's behavior.
      options(artma.temp.last_methods = selection)
      run_and_record(selection)
    } else if (identical(action, "rerun")) {
      run_and_record(state$last_methods)
    } else if (identical(action, "options")) {
      outcome <- run_adjust_options(
        last_methods = state$last_methods,
        select_fn = select_fn,
        edit_option = edit_option,
        save_preference = save_preference,
        width = width,
        template_path = template_path
      )
      if (length(outcome$changed) > 0L) {
        state$changed_since_run <- unique(c(state$changed_since_run, outcome$changed))
        if (isTRUE(outcome$data_changed) && !isTRUE(state$data_stale)) {
          state$data_stale <- TRUE
          cli::cli_alert_info(
            "Data preparation options changed: the data will be re-prepared before the next run."
          )
        }
      }
    } else if (identical(action, "preview")) {
      render_data_summary(state$df)
      if (data_viewer_available()) {
        should_view <- ask_yes_no(
          "Open the data in a spreadsheet viewer?",
          default = FALSE,
          select_fn = select_fn
        )
        if (should_view) {
          tryCatch(
            view_data(state$df),
            error = function(e) cli::cli_alert_warning("Could not open the viewer: {conditionMessage(e)}")
          )
        }
      }
    } else if (identical(action, "results")) {
      results_action <- ask_select(
        question = "Results",
        choices = compose_hub_choices(
          list(
            list(value = "open", name = "Open results folder", description = "in the system file browser"),
            list(value = "report", name = "Render HTML report", description = "one self-contained file from this session's results"),
            list(value = "back", name = "Back", description = "return to the session menu")
          ),
          width = width
        ),
        confirm = FALSE,
        select_fn = select_fn
      )
      if (rlang::is_empty(results_action) || identical(results_action, "back")) {
        next
      }
      if (!state$has_run) {
        cli::cli_alert_info("Nothing to show yet: run methods first.")
        next
      }
      if (identical(results_action, "open")) {
        tryCatch(
          open_results(),
          error = function(e) cli::cli_alert_warning("Could not open the results folder: {conditionMessage(e)}")
        )
      } else if (identical(results_action, "report")) {
        tryCatch(
          render_report(state$accumulated),
          error = function(e) cli::cli_alert_warning("Could not render the report: {conditionMessage(e)}")
        )
      }
    }
  }

  results <- state$accumulated
  attr(results, "runs") <- state$runs
  invisible(results)
}

box::export(
  adjustable_option_defs,
  compose_hub_choices,
  count_studies,
  describe_option_state,
  hub_menu_items,
  merge_run_results,
  option_affects_data,
  render_data_summary,
  run_adjust_options,
  run_session_hub
)
