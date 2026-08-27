#' @title Interactive session hub
#' @description
#' The menu loop `artma()` enters when the session is interactive and no
#' methods were requested: one session, many runs. The user picks and runs
#' methods, previews the prepared data, opens results, adjusts session
#' settings, switches the options file, browses help, and leaves only when
#' done. Entry conditions, item list, and the return contract:
#' contributingGuides/HUB.md.
NULL

box::use(
  artma / const[CONST],
  artma / interactive / input[ask_select, ask_yes_no],
  artma / interactive / method_picker[ask_runtime_methods],
  artma / interactive / welcome[open_url_in_browser],
  artma / libs / core / autonomy[get_autonomy_level, get_default_autonomy_level],
  artma / libs / core / utils[data_viewer_available],
  artma / libs / core / validation[validate],
  artma / modules / methods_table[pad_cell, print_methods_table, truncate_cell],
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
#' changed since that run. The Switch item only appears when the caller wired
#' a `switch_options` handler.
#' @param has_run *\[logical\]* Whether a run happened in this session.
#' @param last_methods *\[character\]* The previously confirmed selection.
#' @param options_changed *\[character, optional\]* Options changed since the
#'   previous run.
#' @param can_switch *\[logical, optional\]* Whether an options-file switch
#'   handler is available. Defaults to `TRUE`.
#' @return *\[list\]* Items for `compose_hub_choices()`.
hub_menu_items <- function(has_run, last_methods, options_changed = character(0), can_switch = TRUE) {
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
  items <- c(items, list(
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
      value = "settings",
      name = "Settings",
      description = "theme, verbosity, autonomy, caching"
    )
  ))
  if (isTRUE(can_switch)) {
    items <- c(items, list(list(
      value = "switch",
      name = "Switch options file",
      description = "load another options file and re-prepare the data"
    )))
  }
  c(items, list(
    list(
      value = "help",
      name = "Help",
      description = "methods overview, options help, vignettes"
    ),
    list(
      value = "exit",
      name = "Exit",
      description = "return results to the R session"
    )
  ))
}

#' @title The Settings submenu items, decorated with the current values
#' @return *\[list\]* Items for `compose_hub_choices()`.
settings_menu_items <- function() {
  caching <- if (isTRUE(getOption("artma.cache.use_cache", TRUE))) "on" else "off"
  list(
    list(
      value = "theme",
      name = "Visualization theme",
      description = sprintf("current: %s", getOption("artma.visualization.theme", "blue"))
    ),
    list(
      value = "verbosity",
      name = "Verbosity",
      description = sprintf("current: %s", getOption("artma.verbose", 3))
    ),
    list(
      value = "autonomy",
      name = "Autonomy level",
      description = sprintf("current: %s", get_autonomy_level() %||% get_default_autonomy_level())
    ),
    list(
      value = "cache",
      name = "Result caching",
      description = sprintf("current: %s", caching)
    ),
    list(
      value = "back",
      name = "Back",
      description = "return to the session menu"
    )
  )
}

#' @title The Help submenu items
#' @return *\[list\]* Items for `compose_hub_choices()`.
help_menu_items <- function() {
  list(
    list(
      value = "methods",
      name = "Methods overview",
      description = "the runtime methods and their status for this data"
    ),
    list(
      value = "options",
      name = "Options overview",
      description = "the full options tree with defaults and help texts"
    ),
    list(
      value = "vignette_getting_started",
      name = "Getting Started vignette",
      description = "open in the browser"
    ),
    list(
      value = "vignette_options_files",
      name = "Options Files vignette",
      description = "open in the browser"
    ),
    list(
      value = "website",
      name = "Package website",
      description = "open in the browser"
    ),
    list(
      value = "back",
      name = "Back",
      description = "return to the session menu"
    )
  )
}

#' @title Compose picker items for the available options files
#' @description Decorate each file of an `options_list(details = TRUE)` frame
#'   with its data source basename, last run time, and count of non-default
#'   options; the session's current file is marked as such.
#' @param details *\[data.frame\]* An `options_list(details = TRUE)` frame.
#' @param current_file *\[character, optional\]* The options file behind the
#'   session, marked "current" in its description.
#' @return *\[list\]* Items for `compose_hub_choices()`, one per file, with the
#'   file name as the value.
options_file_items <- function(details, current_file = NULL) {
  lapply(seq_len(nrow(details)), function(i) {
    source_path <- details$data_source_path[[i]]
    last_run <- details$last_run[[i]]
    n_non_default <- details$n_non_default[[i]]

    parts <- c(
      if (is.character(source_path) && !is.na(source_path) && nzchar(source_path)) {
        basename(source_path)
      } else {
        "no data source"
      },
      if (!is.na(last_run)) {
        sprintf("last run %s", format(last_run, CONST$DATE_FORMAT))
      } else {
        "never run"
      },
      if (n_non_default > 0L) {
        sprintf("%d non-default", n_non_default)
      } else {
        "all defaults"
      }
    )
    if (!is.null(current_file) && identical(details$file[[i]], current_file)) {
      parts <- c("current", parts)
    }

    list(
      value = details$file[[i]],
      name = details$file[[i]],
      description = paste(parts, collapse = ", ")
    )
  })
}

#' @title Run the Settings submenu once
#' @description One pick from the Settings submenu, applied for the session
#'   only: none of the toggles touch the options file on disk.
#' @param select_fn *\[function\]* Menu backend.
#' @param width *\[numeric, optional\]* Console width for the menu labels.
#' @param set_theme *\[function\]* Called with the chosen theme name.
#' @param set_autonomy *\[function\]* Called with the chosen autonomy level.
#' @return `NULL`, invisibly.
run_settings_menu <- function(select_fn, width, set_theme, set_autonomy) {
  action <- ask_select(
    question = "Settings (session only; the options file is not changed)",
    choices = compose_hub_choices(settings_menu_items(), width = width),
    confirm = FALSE,
    select_fn = select_fn
  )
  if (rlang::is_empty(action) || identical(action, "back")) {
    return(invisible(NULL))
  }

  if (identical(action, "theme")) {
    themes <- artma::viz_themes()
    current <- getOption("artma.visualization.theme", "blue")
    theme <- ask_select(
      question = "Select a theme",
      choices = themes,
      default = if (current %in% themes) current else NULL,
      confirm = FALSE,
      select_fn = select_fn
    )
    if (rlang::is_empty(theme)) {
      return(invisible(NULL))
    }
    set_theme(theme)
  } else if (identical(action, "verbosity")) {
    current <- as.character(getOption("artma.verbose", 3))
    level <- ask_select(
      question = "Select the verbosity level",
      choices = c(
        "1 - errors only" = "1",
        "2 - errors and warnings" = "2",
        "3 - standard output" = "3",
        "4 - debug" = "4"
      ),
      default = if (current %in% as.character(1:4)) current else NULL,
      confirm = FALSE,
      select_fn = select_fn
    )
    if (rlang::is_empty(level)) {
      return(invisible(NULL))
    }
    options(artma.verbose = as.integer(level))
    cli::cli_alert_success("Verbosity set to {level} for this session.")
  } else if (identical(action, "autonomy")) {
    level <- ask_select(
      question = "Select the autonomy level",
      choices = c(
        "ask_more - prompt for most decisions" = "ask_more",
        "balanced - prompt for the important decisions" = "balanced",
        "autonomous - prompt only when unavoidable" = "autonomous"
      ),
      default = get_autonomy_level() %||% get_default_autonomy_level(),
      confirm = FALSE,
      select_fn = select_fn
    )
    if (rlang::is_empty(level)) {
      return(invisible(NULL))
    }
    set_autonomy(level)
    cli::cli_alert_success("Autonomy level set to {level} for this session.")
  } else if (identical(action, "cache")) {
    use_cache <- ask_yes_no(
      "Use cached results where available?",
      default = isTRUE(getOption("artma.cache.use_cache", TRUE)),
      select_fn = select_fn
    )
    options(artma.cache.use_cache = use_cache)
    state_word <- if (use_cache) "enabled" else "disabled"
    cli::cli_alert_success("Result caching {state_word} for this session.")
  }

  invisible(NULL)
}

#' @title Run the Help submenu once
#' @param select_fn *\[function\]* Menu backend.
#' @param width *\[numeric, optional\]* Console width for the menu labels.
#' @param get_methods_frame *\[function\]* Returns the methods table frame for
#'   the overview.
#' @param show_options_help *\[function\]* Prints the options overview.
#' @param open_url *\[function\]* Called with a URL and a description; opens it
#'   in the browser.
#' @return `NULL`, invisibly.
run_help_menu <- function(select_fn, width, get_methods_frame, show_options_help, open_url) {
  action <- ask_select(
    question = "Help",
    choices = compose_hub_choices(help_menu_items(), width = width),
    confirm = FALSE,
    select_fn = select_fn
  )
  if (rlang::is_empty(action) || identical(action, "back")) {
    return(invisible(NULL))
  }

  if (identical(action, "methods")) {
    print_methods_table(get_methods_frame())
  } else if (identical(action, "options")) {
    show_options_help()
    cli::cli_alert_info(
      "Run {.code artma::options_help(\"<option or group>\")} for details on a single option or group."
    )
  } else if (identical(action, "vignette_getting_started")) {
    open_url(
      paste0(CONST$URLS$VIGNETTE_BASE, "/getting-started.html"),
      "'Getting Started' vignette"
    )
  } else if (identical(action, "vignette_options_files")) {
    open_url(
      paste0(CONST$URLS$VIGNETTE_BASE, "/options-files.html"),
      "'Options Files' vignette"
    )
  } else if (identical(action, "website")) {
    open_url(CONST$URLS$PACKAGE_BASE, "artma package website")
  }

  invisible(NULL)
}

# Sentinel for the Back entry of the options-file picker; a value no real
# options file name can take.
SWITCH_BACK_VALUE <- "__back__"

#' @title Run the options-file switch menu once
#' @description Offer the decorated options-file picker and hand the selection
#'   to the switch handler. Any failure (listing, loading, re-preparing)
#'   reports and returns `NULL`, leaving the session on its current file.
#' @param select_fn *\[function\]* Menu backend.
#' @param width *\[numeric, optional\]* Console width for the menu labels.
#' @param list_options *\[function\]* Returns the `options_list(details = TRUE)`
#'   frame of the available options files.
#' @param switch_options *\[function\]* Called with the selected file name; loads
#'   that file's options for the session and returns the freshly prepared data
#'   frame.
#' @return *\[data.frame | NULL\]* The freshly prepared data frame, or `NULL`
#'   when the switch did not happen.
run_switch_menu <- function(select_fn, width, list_options, switch_options) {
  details <- tryCatch(
    list_options(),
    error = function(e) {
      cli::cli_alert_warning("Could not list the options files: {conditionMessage(e)}")
      NULL
    }
  )
  if (!is.data.frame(details) || nrow(details) == 0L) {
    if (!is.null(details)) {
      cli::cli_alert_info("No options files found.")
    }
    return(NULL)
  }

  items <- c(
    options_file_items(details, current_file = getOption("artma.temp.file_name", NULL)),
    list(list(
      value = SWITCH_BACK_VALUE,
      name = "Back",
      description = "keep the current options file"
    ))
  )
  choice <- ask_select(
    question = "Switch to which options file?",
    choices = compose_hub_choices(items, width = width),
    confirm = FALSE,
    select_fn = select_fn
  )
  if (rlang::is_empty(choice) || identical(choice, SWITCH_BACK_VALUE)) {
    return(NULL)
  }

  new_df <- tryCatch(
    switch_options(choice),
    error = function(e) {
      cli::cli_alert_danger("Could not switch to {.file {choice}}: {conditionMessage(e)}")
      NULL
    }
  )
  if (!is.data.frame(new_df)) {
    return(NULL)
  }

  cli::cli_alert_success("Switched to {.file {choice}}: options reloaded, data re-prepared.")
  new_df
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
#' @param list_options *\[function, optional\]* Returns the
#'   `options_list(details = TRUE)` frame behind the options-file picker.
#'   Defaults to `artma::options_list(details = TRUE)`. Injectable for testing.
#' @param switch_options *\[function, optional\]* Called with the selected
#'   options file name; must load that file's options for the rest of the
#'   session and return the freshly prepared data frame. `NULL` (the default)
#'   hides the "Switch options file" item; `artma()` wires the real handler.
#' @param set_theme *\[function, optional\]* Applies a visualization theme for
#'   the session. Defaults to `artma::viz_set(theme = ...)`. Injectable for
#'   testing.
#' @param set_autonomy *\[function, optional\]* Applies an autonomy level for
#'   the session. Defaults to `artma::autonomy_set()`. Injectable for testing.
#' @param show_options_help *\[function, optional\]* Prints the options
#'   overview. Defaults to `artma::options_help()`. Injectable for testing.
#' @param open_url *\[function, optional\]* Opens a URL in the browser; receives
#'   the URL and a description. Defaults to the welcome module's
#'   `open_url_in_browser()`. Injectable for testing.
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
  list_options = NULL,
  switch_options = NULL,
  set_theme = NULL,
  set_autonomy = NULL,
  show_options_help = NULL,
  open_url = NULL,
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
    is.null(list_options) || is.function(list_options),
    is.null(switch_options) || is.function(switch_options),
    is.null(set_theme) || is.function(set_theme),
    is.null(set_autonomy) || is.function(set_autonomy),
    is.null(show_options_help) || is.function(show_options_help),
    is.null(open_url) || is.function(open_url),
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
  if (is.null(list_options)) {
    list_options <- function() artma::options_list(details = TRUE)
  }
  if (is.null(set_theme)) {
    set_theme <- function(theme) artma::viz_set(theme = theme)
  }
  if (is.null(set_autonomy)) {
    set_autonomy <- function(level) artma::autonomy_set(level)
  }
  if (is.null(show_options_help)) {
    show_options_help <- function() artma::options_help()
  }
  if (is.null(open_url)) {
    open_url <- open_url_in_browser
  }

  # Session state, kept in an environment so the loop's helpers can update it
  # without non-local assignment. The prepared frame lives here too: an
  # options-file switch replaces it mid-session.
  state <- new.env(parent = emptyenv())
  state$df <- df
  state$methods_frame <- methods_table
  # An injected frame is kept as-is across an options-file switch; a self-built
  # one is rebuilt for the new data on the next pass that needs it.
  state$methods_frame_injected <- !is.null(methods_table)
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
        hub_menu_items(
          state$has_run,
          state$last_methods,
          options_changed = state$changed_since_run,
          can_switch = !is.null(switch_options)
        ),
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
        width = width,
        checkbox_fn = checkbox_fn
      )
      if (length(selection) == 0L) {
        cli::cli_alert_info("No methods selected.")
        next
      }
      state$last_methods <- selection
      # Not fed back as the checkbox default: each "Run methods" pick starts
      # blank. Still mirrored into artma.temp.last_methods for the linear
      # path's own default (see R/artma.R).
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
    } else if (identical(action, "settings")) {
      run_settings_menu(
        select_fn = select_fn,
        width = width,
        set_theme = set_theme,
        set_autonomy = set_autonomy
      )
    } else if (identical(action, "switch")) {
      new_df <- run_switch_menu(
        select_fn = select_fn,
        width = width,
        list_options = list_options,
        switch_options = switch_options
      )
      if (!is.null(new_df)) {
        state$df <- new_df
        # The switch handler prepared the frame under the new file's options,
        # so any pending staleness from earlier option edits is resolved.
        state$data_stale <- FALSE
        if (!state$methods_frame_injected) {
          state$methods_frame <- NULL
        }
      }
    } else if (identical(action, "help")) {
      run_help_menu(
        select_fn = select_fn,
        width = width,
        get_methods_frame = get_methods_frame,
        show_options_help = show_options_help,
        open_url = open_url
      )
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
  help_menu_items,
  hub_menu_items,
  merge_run_results,
  option_affects_data,
  options_file_items,
  render_data_summary,
  run_adjust_options,
  run_session_hub,
  settings_menu_items
)
