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
  artma / modules / methods_table[pad_cell, truncate_cell]
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

#' @title The hub's menu items in display order
#' @description
#' The Re-run item only appears once a run has happened in this hub session;
#' its description names the selection it would repeat.
#' @param has_run *\[logical\]* Whether a run happened in this session.
#' @param last_methods *\[character\]* The previously confirmed selection.
#' @return *\[list\]* Items for `compose_hub_choices()`.
hub_menu_items <- function(has_run, last_methods) {
  items <- list(
    list(
      value = "run",
      name = "Run methods",
      description = "pick and run analysis methods"
    )
  )
  if (isTRUE(has_run)) {
    items <- c(items, list(list(
      value = "rerun",
      name = "Re-run last selection",
      description = paste(last_methods, collapse = ", ")
    )))
  }
  c(items, list(
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
#'   `seed`, `timestamp`). Empty when the user exits before any run.
run_session_hub <- function(
  df,
  run_methods,
  methods_table = NULL,
  view_data = NULL,
  open_results = NULL,
  render_report = NULL,
  select_fn = climenu::select,
  checkbox_fn = climenu::checkbox,
  width = NULL
) {
  validate(
    is.data.frame(df),
    is.function(run_methods),
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
  state$methods_frame <- methods_table
  state$accumulated <- list()
  state$runs <- list()
  state$last_methods <- character(0)
  state$has_run <- FALSE

  get_methods_frame <- function() {
    if (is.null(state$methods_frame)) {
      box::use(artma / modules / methods_table[build_methods_table])
      state$methods_frame <- build_methods_table(available_for = df)
    }
    state$methods_frame
  }

  run_and_record <- function(selection) {
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
      timestamp = Sys.time()
    )))
    state$has_run <- TRUE
    invisible(NULL)
  }

  repeat {
    render_hub_header(df)

    action <- ask_select(
      question = "What would you like to do?",
      choices = compose_hub_choices(
        hub_menu_items(state$has_run, state$last_methods),
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
    } else if (identical(action, "preview")) {
      render_data_summary(df)
      if (data_viewer_available()) {
        should_view <- ask_yes_no(
          "Open the data in a spreadsheet viewer?",
          default = FALSE,
          select_fn = select_fn
        )
        if (should_view) {
          tryCatch(
            view_data(df),
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
  compose_hub_choices,
  count_studies,
  hub_menu_items,
  merge_run_results,
  render_data_summary,
  run_session_hub
)
