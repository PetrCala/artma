#' @title Runtime method overview table
#' @description
#' Turns the declarative metadata every runtime method registers (see
#' `register_runtime_method()`) into the frame `artma::methods_list()` returns,
#' plus the fixed-width rendering of that frame for the console.
NULL

box::use(
  artma / libs / core / validation[assert],
  artma / modules / runtime_methods[
    get_method_metadata,
    get_runtime_method_modules,
    missing_required_columns,
    missing_suggested_packages
  ]
)

# Columns of the frame built for a call without `available_for`.
BASE_COLUMNS <- c(
  "method", "description", "required_columns", "depends_on",
  "suggests", "missing_packages", "installed", "opt_in"
)

# The extra columns a pre-flight check against a data frame adds.
AVAILABILITY_COLUMNS <- c("missing_columns", "available")

# Placeholder printed for an empty cell, so a column never reads as truncated.
EMPTY_CELL <- "-"

join_values <- function(values) {
  if (length(values) == 0L) {
    return("")
  }
  paste(as.character(values), collapse = ", ")
}

#' @title Build the runtime method overview frame
#' @description
#' Collect every discovered runtime method's registered metadata into one
#' `data.frame`, one row per method, ordered by method name. Multi-value fields
#' (`required_columns`, `depends_on`, `suggests`, `missing_packages`,
#' `missing_columns`) are comma-separated strings, empty when the method
#' declares nothing.
#' @param available_for *\[data.frame, optional\]* Data frame to check each
#'   method's `required_columns` against. When supplied, the frame gains a
#'   `missing_columns` column and an `available` flag combining the column and
#'   the package checks.
#' @param modules *\[list, optional\]* Discovered method modules, keyed by
#'   method name. Injectable for testing; defaults to the crawled modules.
#' @param is_installed *\[function, optional\]* Predicate testing whether a
#'   package is installed. Injectable for testing.
#' @return *\[data.frame\]* One row per method.
build_methods_table <- function(available_for = NULL, modules = NULL, is_installed = NULL) {
  if (!is.null(available_for)) {
    assert(
      is.data.frame(available_for),
      "`available_for` must be a data frame, or `NULL`."
    )
  }

  if (is.null(modules)) {
    modules <- get_runtime_method_modules()
  }

  columns <- BASE_COLUMNS
  if (!is.null(available_for)) {
    columns <- c(columns, AVAILABILITY_COLUMNS)
  }

  method_names <- sort(names(modules))
  if (length(method_names) == 0L) {
    empty <- lapply(columns, function(column) {
      if (column %in% c("installed", "opt_in", "available")) logical() else character()
    })
    names(empty) <- columns
    return(as.data.frame(empty, stringsAsFactors = FALSE))
  }

  rows <- lapply(method_names, function(name) {
    meta <- get_method_metadata(modules[[name]][["run"]], name = name)
    missing_packages <- missing_suggested_packages(meta$suggests, is_installed)

    row <- data.frame(
      method = name,
      description = as.character(meta$description)[1L],
      required_columns = join_values(meta$required_columns),
      depends_on = join_values(meta$depends_on),
      suggests = join_values(meta$suggests),
      missing_packages = join_values(missing_packages),
      installed = length(missing_packages) == 0L,
      opt_in = isTRUE(meta$opt_in),
      stringsAsFactors = FALSE
    )

    if (!is.null(available_for)) {
      missing_columns <- missing_required_columns(available_for, meta$required_columns)
      row$missing_columns <- join_values(missing_columns)
      row$available <- length(missing_columns) == 0L && length(missing_packages) == 0L
    }

    row
  })

  df <- do.call(rbind, rows)
  rownames(df) <- NULL
  df
}

#' @title Status cell for each method
#' @description
#' Condense the reasons a method would not run into one short cell: whether it
#' is opt-in, which suggested packages are absent, and, for a pre-flight check
#' against a data frame, which required columns the data lacks. `ok` means the
#' method runs as part of `methods = "all"` on this machine.
#' @param df *\[data.frame\]* A frame built by `build_methods_table()`.
#' @return *\[character\]* One status string per row.
methods_status <- function(df) {
  vapply(seq_len(nrow(df)), function(i) {
    parts <- character()
    if (isTRUE(df$opt_in[[i]])) {
      parts <- c(parts, "opt-in")
    }
    if (nzchar(df$missing_packages[[i]])) {
      parts <- c(parts, paste0("install ", df$missing_packages[[i]]))
    }
    if (!is.null(df$missing_columns) && nzchar(df$missing_columns[[i]])) {
      parts <- c(parts, paste0("needs ", df$missing_columns[[i]]))
    }
    if (length(parts) == 0L) "ok" else paste(parts, collapse = "; ")
  }, character(1))
}

#' @title Truncate cell values to a width, marking the cut with an ellipsis
#' @param values *\[character\]* Cell values.
#' @param width *\[numeric\]* Maximum width in characters.
#' @return *\[character\]* The truncated values.
truncate_cell <- function(values, width) {
  ellipsis <- cli::symbol$ellipsis
  keep <- width - nchar(ellipsis)
  vapply(values, function(value) {
    if (nchar(value) <= width) {
      return(value)
    }
    if (keep < 1L) {
      return(substr(value, 1L, width))
    }
    paste0(substr(value, 1L, keep), ellipsis)
  }, character(1), USE.NAMES = FALSE)
}

#' @title Left-align cell values into a fixed width
#' @param values *\[character\]* Cell values.
#' @param width *\[numeric\]* Column width in characters.
#' @return *\[character\]* The padded values.
pad_cell <- function(values, width) {
  formatC(values, width = width, flag = "-")
}

#' @title Fit column widths into the available console width
#' @description
#' Shrink the shrinkable columns, in the given order and never below their
#' minimum, until the row fits. Columns absent from `shrink_order` (the method
#' names) keep their natural width, so a row can still overflow a very narrow
#' console rather than mangling the one column a user must copy verbatim.
#' @param natural *\[list\]* Named list of natural (untruncated) widths.
#' @param minimum *\[list\]* Named list of minimum widths.
#' @param shrink_order *\[character\]* Column names, most shrinkable first.
#' @param available *\[numeric\]* Total width to fit into.
#' @param gap *\[numeric, optional\]* Spaces between columns. Defaults to 2.
#' @return *\[list\]* The fitted widths, in the order of `natural`.
fit_column_widths <- function(natural, minimum, shrink_order, available, gap = 2L) {
  widths <- natural
  row_width <- function() sum(unlist(widths)) + gap * (length(widths) - 1L)

  for (column in shrink_order) {
    excess <- row_width() - available
    if (excess <= 0L) {
      break
    }
    widths[[column]] <- max(minimum[[column]], widths[[column]] - excess)
  }

  widths
}

#' @title Render the method overview frame as fixed-width lines
#' @description
#' Lay the frame out as a plain table sized to the console. Width is taken away
#' in two passes, so no single column collapses while another still has slack:
#' first down to comfortable floors, then, only if the row still does not fit,
#' down to hard minimums. Any width left over goes back to the description.
#' Cells are truncated rather than wrapped, keeping one row per method.
#' @param df *\[data.frame\]* A frame built by `build_methods_table()`.
#' @param width *\[numeric, optional\]* Console width. Defaults to the detected
#'   console width.
#' @return *\[character\]* The header, rule, and one line per method.
format_methods_table <- function(df, width = NULL) {
  if (nrow(df) == 0L) {
    return(character())
  }

  width <- width %||% cli::console_width()

  cells <- list(
    method = df$method,
    description = df$description,
    requires = df$required_columns,
    depends = df$depends_on,
    packages = df$suggests,
    status = methods_status(df)
  )
  headers <- list(
    method = "method",
    description = "description",
    requires = "requires",
    depends = "deps",
    packages = "packages",
    status = "status"
  )
  comfortable <- list(
    method = 0L,
    description = 40L,
    requires = 20L,
    depends = 0L,
    packages = 13L,
    status = 18L
  )
  minimum <- list(
    method = 0L,
    description = 20L,
    requires = 10L,
    depends = 0L,
    packages = 8L,
    status = 10L
  )
  shrink_order <- c("description", "requires", "status", "packages")

  cells <- lapply(cells, function(column) {
    column[is.na(column) | !nzchar(column)] <- EMPTY_CELL
    column
  })

  natural <- lapply(names(cells), function(name) {
    max(nchar(c(headers[[name]], cells[[name]])))
  })
  names(natural) <- names(cells)
  cap_at_natural <- function(floors) {
    capped <- lapply(names(cells), function(name) min(floors[[name]], natural[[name]]))
    names(capped) <- names(cells)
    capped
  }

  widths <- fit_column_widths(
    natural = natural,
    minimum = cap_at_natural(comfortable),
    shrink_order = shrink_order,
    available = width
  )
  widths <- fit_column_widths(
    natural = widths,
    minimum = cap_at_natural(minimum),
    shrink_order = shrink_order,
    available = width
  )
  widths$description <- min(
    natural$description,
    widths$description + max(0L, width - (sum(unlist(widths)) + 2L * (length(widths) - 1L)))
  )

  build_row <- function(values) {
    padded <- vapply(names(cells), function(name) {
      pad_cell(truncate_cell(values[[name]], widths[[name]]), widths[[name]])
    }, character(1))
    trimws(paste(padded, collapse = "  "), which = "right")
  }

  rule <- build_row(lapply(widths, function(w) strrep("-", w)))
  rows <- vapply(seq_len(nrow(df)), function(i) {
    build_row(lapply(cells, function(column) column[[i]]))
  }, character(1))

  c(build_row(headers), rule, rows)
}

#' @title Print the method overview table
#' @param df *\[data.frame\]* A frame built by `build_methods_table()`.
#' @param width *\[numeric, optional\]* Console width. Defaults to the detected
#'   console width.
#' @return `NULL` (invisible)
print_methods_table <- function(df, width = NULL) {
  if (nrow(df) == 0L) {
    cli::cli_alert_warning("No runtime methods were found.")
    return(invisible(NULL))
  }

  for (line in format_methods_table(df, width = width)) {
    cli::cli_verbatim(line)
  }

  invisible(NULL)
}

box::export(
  build_methods_table,
  fit_column_widths,
  format_methods_table,
  methods_status,
  pad_cell,
  print_methods_table,
  truncate_cell
)
