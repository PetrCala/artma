#' @title The hub's options-file menu
#' @description
#' Everything the session hub offers around options files: selecting the one
#' the session runs on, and managing the files themselves (create, duplicate,
#' edit, repair, compare, open, delete). Selecting a file is the only action
#' that changes what the session is bound to; the rest are file-management
#' side trips that leave the session where it was, except when they touch the
#' file the session is already running on.
#'
#' Item list and semantics: contributingGuides/HUB.md.
NULL

box::use(
  artma / const[CONST],
  artma / interactive / input[ask_select, ask_yes_no],
  artma / interactive / menu[compose_menu_choices, menu_item],
  artma / libs / core / validation[validate],
  artma / options / last_used[prune_last_used_file, write_last_used_file]
)

# Sentinels for the picker entries that are not options file names; no real
# file name can take these values.
BACK_VALUE <- "__back__"
CREATE_VALUE <- "__create__"

#' @title The file-management actions the menu delegates to
#' @description The public options API, pre-bound to the session's options
#'   directory, plus the last-used marker maintenance (`remember_last_used`,
#'   `prune_last_used`). Injectable as a whole so tests never reach the real
#'   files.
#' @param options_dir *\[character, optional\]* Directory holding the options
#'   files. `NULL` uses the package default.
#' @return *\[list\]* One function per management action.
default_file_actions <- function(options_dir = NULL) {
  list(
    remember_last_used = function(file_name) {
      write_last_used_file(file_name, options_dir = options_dir)
    },
    prune_last_used = function(existing_files) {
      prune_last_used_file(existing_files, options_dir = options_dir)
    },
    list = function() artma::options_list(options_dir = options_dir, details = TRUE),
    create = function() artma::options_create(options_dir = options_dir),
    duplicate = function() artma::options_copy(options_dir = options_dir),
    edit = function(file) {
      artma::options_modify(options_file_name = file, options_dir = options_dir)
    },
    repair = function(file) {
      artma::options_fix(options_file_name = file, options_dir = options_dir)
    },
    compare = function() artma::options_diff(options_dir = options_dir),
    open = function(file) {
      artma::options_open(options_file_name = file, options_dir = options_dir)
    },
    delete = function() artma::options_delete(options_dir = options_dir)
  )
}

#' @title Compose picker items for the available options files
#' @description Decorate each file of an `options_list(details = TRUE)` frame
#'   with its data source basename, last run time, and count of non-default
#'   options; the session's current file is marked as such.
#' @param details *\[data.frame\]* An `options_list(details = TRUE)` frame.
#' @param current_file *\[character, optional\]* The options file behind the
#'   session, marked "current" in its description.
#' @return *\[list\]* Items for `compose_menu_choices()`, one per file, with the
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

    menu_item(
      value = details$file[[i]],
      name = details$file[[i]],
      description = paste(parts, collapse = ", ")
    )
  })
}

#' @title The options-file menu items in display order
#' @description Selecting a file comes first and reads differently depending on
#'   whether the session is bound to one: an unbound session is choosing its
#'   file, a bound one is switching away from it. The actions that need a file
#'   to work on are dropped while no options file exists yet.
#' @param current_file *\[character, optional\]* The session's options file;
#'   `NULL` when the session is unbound.
#' @param n_files *\[integer\]* How many options files exist.
#' @return *\[list\]* Items for `compose_menu_choices()`.
options_file_menu_items <- function(current_file = NULL, n_files = 0L) {
  has_files <- n_files > 0L
  target <- current_file %||% "the file you pick"

  items <- list()
  if (has_files) {
    items <- c(items, list(menu_item(
      value = "select",
      name = if (is.null(current_file)) "Select a file" else "Switch file",
      description = if (is.null(current_file)) {
        if (n_files == 1L) {
          "run this session on the one that exists"
        } else {
          sprintf("run this session on one of the %d that exist", n_files)
        }
      } else {
        sprintf("load another file instead of %s", current_file)
      }
    )))
  }
  items <- c(items, list(menu_item(
    value = "create",
    name = "Create a new file",
    description = "guided setup: data source, columns, method options"
  )))
  if (has_files) {
    items <- c(items, list(
      menu_item(
        value = "edit",
        name = "Edit a file",
        description = sprintf("walk through the stored values of %s", target)
      ),
      menu_item(
        value = "duplicate",
        name = "Duplicate a file",
        description = "copy an existing file under a new name"
      ),
      menu_item(
        value = "repair",
        name = "Repair a file",
        description = sprintf("bring %s back in line with the template", target)
      ),
      menu_item(
        value = "compare",
        name = "Compare two files",
        description = "list the options whose values differ"
      ),
      menu_item(
        value = "open",
        name = "Open a file in an editor",
        description = sprintf("edit the YAML of %s by hand", target)
      ),
      menu_item(
        value = "delete",
        name = "Delete files",
        description = "remove options files permanently"
      )
    ))
  }
  c(items, list(menu_item(
    value = BACK_VALUE,
    name = "Back",
    description = "return to the session menu"
  )))
}

#' @title Ask which options file to load
#' @description The decorated picker over the existing files, plus a create
#'   entry, so an unbound session never dead-ends on an empty list.
#' @param details *\[data.frame\]* An `options_list(details = TRUE)` frame.
#' @param current_file *\[character, optional\]* The session's options file.
#' @param select_fn *\[function\]* Menu backend.
#' @param width *\[numeric, optional\]* Console width for the menu labels.
#' @return *\[character\]* The chosen file name, `CREATE_VALUE`, or `BACK_VALUE`.
ask_for_file_to_load <- function(details, current_file, select_fn, width = NULL) {
  items <- c(
    options_file_items(details, current_file = current_file),
    list(
      menu_item(
        value = CREATE_VALUE,
        name = "Create a new file",
        description = "none of these fit"
      ),
      menu_item(
        value = BACK_VALUE,
        name = "Back",
        description = if (is.null(current_file)) {
          "decide later"
        } else {
          sprintf("keep %s", current_file)
        }
      )
    )
  )
  choice <- ask_select(
    question = if (is.null(current_file)) {
      "Which options file should this session run on?"
    } else {
      "Switch to which options file?"
    },
    choices = compose_menu_choices(items, width = width),
    confirm = FALSE,
    select_fn = select_fn
  )
  if (rlang::is_empty(choice)) BACK_VALUE else choice
}

#' @title Run the hub's options-file menu
#' @description
#' A loop over the file actions. It returns as soon as the session's options
#' file changes (the hub has a new header to draw and data to prepare) or the
#' user backs out; the management actions keep the loop open, so a create can
#' be followed by a select without leaving the submenu.
#'
#' Editing or repairing the file the session is already running on reloads it,
#' so the session never keeps values the file no longer holds. Deleting it
#' leaves the session unbound.
#' @param bind_options *\[function\]* Called with an options file name; must load
#'   that file's options for the rest of the session. Errors are reported and
#'   leave the session on its current file.
#' @param current_file *\[character, optional\]* The session's options file;
#'   `NULL` when the session is not bound to one yet.
#' @param select_fn *\[function, optional\]* Menu backend.
#' @param file_actions *\[list, optional\]* The management actions; see
#'   `default_file_actions()`. Injectable for testing.
#' @param width *\[numeric, optional\]* Console width for the menu labels.
#' @return *\[list\]* `file` (the session's options file after the menu, `NULL`
#'   when it is unbound) and `changed` (whether the loaded options changed, so
#'   the prepared data must be rebuilt).
run_options_file_menu <- function(
  bind_options,
  current_file = NULL,
  select_fn = climenu::select,
  file_actions = NULL,
  width = NULL
) {
  validate(
    is.null(current_file) || is.character(current_file),
    is.function(select_fn),
    is.function(bind_options),
    is.null(file_actions) || is.list(file_actions)
  )
  actions <- file_actions %||% default_file_actions()

  state <- new.env(parent = emptyenv())
  state$file <- current_file
  state$changed <- FALSE

  outcome <- function() list(file = state$file, changed = state$changed)

  # Every listing goes through here, so a menu pass always sees the files as
  # they are on disk after the previous action.
  list_files <- function() {
    details <- tryCatch(
      actions$list(),
      error = function(e) {
        cli::cli_alert_warning("Could not list the options files: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.data.frame(details)) NULL else details
  }

  bind <- function(file_name, reloaded = FALSE) {
    ok <- tryCatch(
      {
        bind_options(file_name)
        TRUE
      },
      error = function(e) {
        cli::cli_alert_danger("Could not load {.file {file_name}}: {conditionMessage(e)}")
        FALSE
      }
    )
    if (!ok) {
      return(FALSE)
    }
    state$file <- file_name
    state$changed <- TRUE
    # Every successful bind is remembered, so the next bare artma() call can
    # resume on the file the user last ran.
    if (is.function(actions$remember_last_used)) {
      actions$remember_last_used(file_name)
    }
    if (isTRUE(reloaded)) {
      cli::cli_alert_success("Reloaded {.file {file_name}}.")
    } else {
      cli::cli_alert_success("Now running on {.file {file_name}}.")
    }
    TRUE
  }

  # A freshly created file is what an unbound session was missing, so it is
  # loaded straight away; a bound session is asked, since it may have been
  # preparing the file for later.
  offer_to_load <- function(file_name) {
    if (!is.character(file_name) || length(file_name) != 1L || is.na(file_name)) {
      return(FALSE)
    }
    if (!is.null(state$file)) {
      if (!ask_yes_no(
        sprintf("Run this session on %s now?", file_name),
        default = FALSE,
        select_fn = select_fn
      )) {
        return(FALSE)
      }
    }
    bind(file_name)
  }

  # Management actions never abort the hub: a failure is reported and the menu
  # stays open.
  attempt <- function(label, action) {
    tryCatch(
      action(),
      error = function(e) {
        cli::cli_alert_warning("{label} failed: {conditionMessage(e)}")
        NULL
      }
    )
  }

  repeat {
    details <- list_files()
    n_files <- if (is.null(details)) 0L else nrow(details)

    action <- ask_select(
      question = "Options files",
      choices = compose_menu_choices(
        options_file_menu_items(current_file = state$file, n_files = n_files),
        width = width
      ),
      confirm = FALSE,
      select_fn = select_fn
    )
    if (rlang::is_empty(action) || identical(action, BACK_VALUE)) {
      return(outcome())
    }

    if (identical(action, "select")) {
      choice <- ask_for_file_to_load(details, state$file, select_fn = select_fn, width = width)
      if (identical(choice, BACK_VALUE)) {
        next
      }
      if (identical(choice, CREATE_VALUE)) {
        if (offer_to_load(attempt("Creating an options file", actions$create))) {
          return(outcome())
        }
        next
      }
      if (identical(choice, state$file)) {
        cli::cli_alert_info("Already running on {.file {choice}}.")
        next
      }
      if (bind(choice)) {
        return(outcome())
      }
      next
    }

    if (identical(action, "create")) {
      if (offer_to_load(attempt("Creating an options file", actions$create))) {
        return(outcome())
      }
      next
    }

    if (identical(action, "duplicate")) {
      attempt("Copying an options file", actions$duplicate)
      next
    }

    if (identical(action, "compare")) {
      attempt("Comparing options files", actions$compare)
      next
    }

    if (action %in% c("edit", "repair", "open")) {
      label <- switch(action,
        edit = "Editing an options file",
        repair = "Repairing an options file",
        open = "Opening an options file"
      )
      # A bound session edits its own file by default; an unbound one lets the
      # underlying prompt ask which file to work on.
      target <- state$file
      attempt(label, function() actions[[action]](target))
      # The session's own file may now hold different values than the ones
      # loaded into this session; reload so the two cannot drift apart.
      if (!is.null(target) && action %in% c("edit", "repair")) {
        bind(target, reloaded = TRUE)
      }
      next
    }

    if (identical(action, "delete")) {
      attempt("Deleting options files", actions$delete)
      remaining <- list_files()
      # A deleted file must not be resumed next session, whether or not it was
      # the one this session ran on. Skipped when the listing itself failed:
      # the files may all still exist.
      if (!is.null(remaining) && is.function(actions$prune_last_used)) {
        actions$prune_last_used(remaining$file)
      }
      if (!is.null(state$file) && !is.null(remaining) && !(state$file %in% remaining$file)) {
        cli::cli_alert_info(
          "{.file {state$file}} is gone; select or create an options file before running anything."
        )
        state$file <- NULL
        state$changed <- TRUE
        return(outcome())
      }
      next
    }
  }
}

box::export(
  default_file_actions,
  options_file_items,
  options_file_menu_items,
  run_options_file_menu
)
