#' @title Metadata-decorated runtime method picker
#' @description
#' Interactive multi-select over the runtime methods, fed by the overview frame
#' `build_methods_table()` returns. Each menu entry pairs the method name with
#' its description and dim preflight markers (missing columns, missing
#' packages, opt-in), so users see before selecting which methods would be
#' skipped.
NULL

box::use(
  artma / interactive / input[ask_checkbox],
  artma / libs / core / validation[assert, validate],
  artma / modules / methods_table[pad_cell, truncate_cell]
)

# Width climenu spends in front of every entry: the cursor mark, the checkbox
# mark, and the separating spaces.
MENU_PREFIX_WIDTH <- 6L

# Spaces between the name, description, and status columns of a label.
COLUMN_GAP <- 2L

# The description column never shrinks below this, even in a narrow console;
# climenu truncates any overflowing line to the console width itself.
MIN_DESCRIPTION_WIDTH <- 12L

#' @title Preflight status markers for each method
#' @description
#' One short marker string per row of a `build_methods_table()` frame: which
#' required columns the data lacks (only for frames built with
#' `available_for`), which suggested packages are not installed, and whether
#' the method is opt-in. Methods that would run cleanly get an empty string.
#' @param methods_df *\[data.frame\]* A frame built by `build_methods_table()`.
#' @return *\[character\]* One marker string per row, `""` when clean.
method_status_markers <- function(methods_df) {
  vapply(seq_len(nrow(methods_df)), function(i) {
    parts <- character()
    if (!is.null(methods_df$missing_columns) && nzchar(methods_df$missing_columns[[i]])) {
      parts <- c(parts, paste0("needs: ", methods_df$missing_columns[[i]]))
    }
    if (nzchar(methods_df$missing_packages[[i]])) {
      parts <- c(parts, paste0("install ", methods_df$missing_packages[[i]]))
    }
    if (isTRUE(methods_df$opt_in[[i]])) {
      parts <- c(parts, "opt-in")
    }
    paste(parts, collapse = " . ")
  }, character(1))
}

#' @title Compose value-keyed picker choices from the method overview frame
#' @description
#' Build the named character vector `ask_checkbox()` consumes: names are the
#' rendered labels (method name padded to a fixed column, description truncated
#' to the remaining width, dim status markers appended), values are the plain
#' method names, so the picker always returns names and never labels.
#' @param methods_df *\[data.frame\]* A frame built by `build_methods_table()`.
#' @param width *\[numeric, optional\]* Console width to fit the labels into.
#'   Defaults to the detected console width.
#' @return *\[character\]* Method names, named by their rendered labels.
compose_method_choices <- function(methods_df, width = NULL) {
  assert(
    is.data.frame(methods_df) && nrow(methods_df) > 0L,
    "`methods_df` must be a data frame with at least one method."
  )

  width <- width %||% cli::console_width()
  available <- max(MIN_DESCRIPTION_WIDTH, width - MENU_PREFIX_WIDTH)

  names <- methods_df$method
  descriptions <- methods_df$description
  descriptions[is.na(descriptions)] <- ""
  markers <- method_status_markers(methods_df)

  name_width <- max(nchar(names))
  marker_width <- max(nchar(markers))
  description_width <- available - name_width - COLUMN_GAP
  if (marker_width > 0L) {
    description_width <- description_width - marker_width - COLUMN_GAP
  }
  description_width <- max(MIN_DESCRIPTION_WIDTH, description_width)

  gap <- strrep(" ", COLUMN_GAP)
  labels <- vapply(seq_along(names), function(i) {
    label <- paste0(
      pad_cell(names[[i]], name_width),
      gap,
      pad_cell(truncate_cell(descriptions[[i]], description_width), description_width)
    )
    if (nzchar(markers[[i]])) {
      label <- paste0(label, gap, cli::col_grey(markers[[i]]))
    }
    trimws(label, which = "right")
  }, character(1))

  stats::setNames(methods_df$method, labels)
}

#' @title Ask which runtime methods to run
#' @description
#' Render the metadata-decorated checkbox over the methods of a
#' `build_methods_table()` frame. Methods with preflight markers stay
#' selectable; the post-selection gates in the method runner remain the safety
#' net. A cancelled menu or empty confirmation returns `character(0)`, leaving
#' the abort decision to the caller.
#' @param methods_df *\[data.frame\]* A frame built by `build_methods_table()`.
#' @param default *\[character, optional\]* Method names to preselect, e.g. the
#'   previous confirmed selection. Names not present in the frame are dropped.
#'   Defaults to `NULL` (nothing preselected).
#' @param width *\[numeric, optional\]* Console width to fit the labels into.
#'   Defaults to the detected console width.
#' @param checkbox_fn *\[function, optional\]* Menu backend passed through to
#'   `ask_checkbox`. Defaults to `climenu::checkbox`. Exposed for testing.
#' @return *\[character\]* The selected method names, or `character(0)`.
ask_runtime_methods <- function(
  methods_df,
  default = NULL,
  width = NULL,
  checkbox_fn = climenu::checkbox
) {
  validate(
    is.null(default) || is.character(default),
    is.function(checkbox_fn)
  )

  choices <- compose_method_choices(methods_df, width = width)

  default <- intersect(default, methods_df$method)
  if (length(default) == 0L) {
    default <- NULL
  }

  ask_checkbox(
    question = "No runtime methods were provided. Select the methods to run:",
    hints = "Dim markers flag methods the run would skip: 'needs' lists absent data columns, 'install' lists absent packages; 'opt-in' methods are excluded from 'all'.",
    choices = choices,
    default = default,
    allow_select_all = TRUE,
    checkbox_fn = checkbox_fn
  )
}

box::export(ask_runtime_methods, compose_method_choices, method_status_markers)
