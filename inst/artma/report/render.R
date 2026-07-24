#' @title HTML report renderer
#' @description
#' Assemble a run's results (the named list of `new_method_result` objects that
#' `artma()` returns) into a single self-contained HTML report. The report has
#' no external dependencies of any kind: all styling is inline, and every plot
#' is embedded as a base64 PNG data URI, so the file can be shared on its own.
#'
#' The HTML skeleton lives in `inst/artma/report/partials/` (header, section,
#' footer) and is assembled here by simple placeholder substitution.

box::use(
  artma / paths[PATHS],
  artma / libs / formatting / results[format_number]
)

# Filename base-names some methods use for their exported PNGs differ from the
# method name. Map the exceptions so the report can find their graphics.
PLOT_NAME_ALIASES <- list(
  prima_facie_graphs = "prima_facie",
  nonlinear_tests = "stem"
)

#' Escape HTML-significant characters in a scalar or vector of strings.
#' @param x *\[character\]* Text to escape.
#' @return *\[character\]* Escaped text.
#' @keywords internal
html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' Turn a name into a stable HTML anchor id.
#' @param name *\[character\]* Section name.
#' @return *\[character\]* A safe anchor id.
#' @keywords internal
sanitize_anchor <- function(name) {
  anchor <- tolower(as.character(name))
  anchor <- gsub("[^a-z0-9]+", "-", anchor)
  anchor <- gsub("^-+|-+$", "", anchor)
  if (!nzchar(anchor)) "section" else paste0("section-", anchor)
}

#' Turn a snake_case method name into a human-readable title.
#' @param name *\[character\]* The method name.
#' @return *\[character\]* Title-cased text.
#' @keywords internal
humanize_name <- function(name) {
  words <- strsplit(gsub("[_.]+", " ", as.character(name)), " ", fixed = TRUE)[[1]]
  words <- words[nzchar(words)]
  if (length(words) == 0L) {
    return(as.character(name))
  }
  substr(words, 1, 1) <- toupper(substr(words, 1, 1))
  paste(words, collapse = " ")
}

#' Read one of the report HTML partials.
#' @param name *\[character\]* Partial base-name (without extension).
#' @return *\[character\]* The partial's contents as a single string.
#' @keywords internal
read_partial <- function(name) {
  path <- file.path(PATHS$DIR_REPORT_PARTIALS, paste0(name, ".html"))
  if (!file.exists(path)) {
    cli::cli_abort("Report partial {.file {name}.html} is missing at {.path {path}}.")
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

#' Substitute `{{key}}` placeholders in a template.
#' @param template *\[character\]* The template string.
#' @param values *\[list\]* Named list of replacement strings.
#' @return *\[character\]* The filled template.
#' @keywords internal
fill_template <- function(template, values) {
  for (key in names(values)) {
    template <- gsub(paste0("{{", key, "}}"), values[[key]], template, fixed = TRUE)
  }
  template
}

#' Format a single data-frame column for display.
#'
#' Numeric columns are rounded to `round_to` decimals; the significance marks a
#' table already carries live in its character columns and are passed through
#' untouched. Missing values render as an empty cell.
#'
#' @param column *\[vector\]* One data-frame column.
#' @param round_to *\[integer\]* Number of decimals for numeric columns.
#' @return *\[character\]* The formatted, HTML-escaped cell strings.
#' @keywords internal
format_column <- function(column, round_to) {
  if (is.numeric(column)) {
    formatted <- format_number(column, round_to)
    formatted[is.na(formatted)] <- ""
    return(html_escape(formatted))
  }
  html_escape(column)
}

#' Whether a column should be left-aligned (text) rather than right-aligned.
#' @keywords internal
is_text_column <- function(column) {
  !is.numeric(column)
}

#' Render a single data frame as an HTML table.
#' @param df *\[data.frame\]* The table to render.
#' @param round_to *\[integer\]* Number of decimals for numeric columns.
#' @return *\[character\]* An HTML fragment (wrapped in a scroll container).
#' @keywords internal
render_table_html <- function(df, round_to) {
  if (!is.data.frame(df) || ncol(df) == 0L) {
    return("")
  }

  col_names <- names(df)
  if (is.null(col_names)) {
    col_names <- paste0("V", seq_len(ncol(df)))
  }

  # Include row names only when they carry meaning (not the default 1..n).
  row_labels <- rownames(df)
  has_row_labels <- !is.null(row_labels) &&
    !identical(as.character(row_labels), as.character(seq_len(nrow(df))))

  text_flags <- vapply(df, is_text_column, logical(1))
  cell_class <- rep("", length(text_flags))
  cell_class[text_flags] <- " class=\"text-cell\""

  header_cells <- character()
  if (has_row_labels) {
    header_cells <- "<th class=\"text-cell\"></th>"
  }
  header_cells <- c(
    header_cells,
    paste0("<th", cell_class, ">", html_escape(col_names), "</th>")
  )
  header_row <- paste0("<tr>", paste(header_cells, collapse = ""), "</tr>")

  formatted_cols <- lapply(df, format_column, round_to = round_to)

  body_rows <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    cells <- character()
    if (has_row_labels) {
      cells <- paste0("<td class=\"text-cell\">", html_escape(row_labels[[i]]), "</td>")
    }
    row_cells <- vapply(seq_along(formatted_cols), function(j) {
      paste0("<td", cell_class[[j]], ">", formatted_cols[[j]][[i]], "</td>")
    }, character(1))
    body_rows[[i]] <- paste0("<tr>", paste(c(cells, row_cells), collapse = ""), "</tr>")
  }

  paste0(
    "<div class=\"table-scroll\"><table class=\"report-table\">",
    "<thead>", header_row, "</thead>",
    "<tbody>", paste(body_rows, collapse = ""), "</tbody>",
    "</table></div>"
  )
}

#' Locate the PNG files a method exported.
#'
#' Matches files in `graphics_dir` whose stem equals, or begins with, the method
#' name (or one of its known base-name aliases). Returns an empty vector when the
#' directory is absent or nothing matches, so callers can degrade gracefully.
#'
#' @param method_name *\[character\]* The method name.
#' @param graphics_dir *\[character or NULL\]* Directory the run wrote graphics to.
#' @return *\[character\]* Absolute paths to matching PNG files.
#' @keywords internal
resolve_method_pngs <- function(method_name, graphics_dir) {
  if (is.null(graphics_dir) || !dir.exists(graphics_dir)) {
    return(character())
  }

  prefixes <- unique(c(method_name, PLOT_NAME_ALIASES[[method_name]]))
  files <- list.files(graphics_dir, pattern = "\\.png$", full.names = TRUE)
  if (length(files) == 0L) {
    return(character())
  }

  stems <- tools::file_path_sans_ext(basename(files))
  keep <- vapply(stems, function(stem) {
    any(vapply(prefixes, function(p) {
      identical(stem, p) || startsWith(stem, paste0(p, "_"))
    }, logical(1)))
  }, logical(1))

  sort(files[keep])
}

#' Embed a PNG file as a base64 data URI.
#' @param path *\[character\]* Path to the PNG file.
#' @return *\[character\]* A `data:image/png;base64,...` URI.
#' @keywords internal
embed_png_data_uri <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Rendering the HTML report needs the {.pkg jsonlite} package to embed plots.",
      "i" = "Install it with {.code install.packages(\"jsonlite\")}."
    ))
  }
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
}

#' Build the plots portion of a method section.
#' @param pngs *\[character\]* PNG paths from `resolve_method_pngs()`.
#' @param has_plots *\[logical\]* Whether the method produced plot objects.
#' @return *\[character\]* An HTML fragment (possibly empty).
#' @keywords internal
render_plots_html <- function(pngs, has_plots) {
  if (length(pngs) > 0L) {
    imgs <- vapply(pngs, function(path) {
      alt <- html_escape(tools::file_path_sans_ext(basename(path)))
      paste0(
        "<figure class=\"report-plot\"><img src=\"", embed_png_data_uri(path),
        "\" alt=\"", alt, "\"></figure>"
      )
    }, character(1))
    return(paste(imgs, collapse = "\n"))
  }

  if (isTRUE(has_plots)) {
    return(paste0(
      "<p class=\"report-note\">This method produced plots, but their image ",
      "files were not found in the output directory (they may not have been ",
      "rewritten on a cached run).</p>"
    ))
  }

  ""
}

#' Count the plot objects a result carries.
#' @keywords internal
count_plots <- function(result) {
  plots <- result$plots
  if (!is.list(plots) || length(plots) == 0L) {
    return(0L)
  }
  sum(!vapply(plots, is.null, logical(1)))
}

#' Build the inner HTML of a method section.
#' @param result *\[list\]* A `new_method_result`-shaped list.
#' @param method_name *\[character\]* The method name.
#' @param graphics_dir *\[character or NULL\]* Directory holding exported graphics.
#' @param round_to *\[integer\]* Number of decimals for numeric columns.
#' @return *\[character\]* The section body HTML.
#' @keywords internal
build_section_body <- function(result, method_name, graphics_dir, round_to) {
  # A method that ran but skipped itself records the reason under meta$skipped.
  skip_reason <- result$meta$skipped
  if (!is.null(skip_reason) && nzchar(as.character(skip_reason)[[1]])) {
    return(paste0(
      "<div class=\"report-skip\">This method was skipped: ",
      html_escape(as.character(skip_reason)[[1]]), "</div>"
    ))
  }

  parts <- character()

  tables <- result$tables
  if (is.list(tables) && length(tables) > 0L) {
    table_names <- names(tables)
    generic_keys <- c("summary", "coefficients", "table")
    for (i in seq_along(tables)) {
      tbl <- tables[[i]]
      if (!is.data.frame(tbl) || nrow(tbl) == 0L) {
        next
      }
      key <- if (is.null(table_names)) NULL else table_names[[i]]
      show_caption <- !is.null(key) && nzchar(key) &&
        !identical(key, method_name) && !(key %in% generic_keys)
      if (show_caption) {
        parts <- c(parts, paste0(
          "<h3 class=\"table-caption\">", html_escape(humanize_name(key)), "</h3>"
        ))
      }
      parts <- c(parts, render_table_html(tbl, round_to))
    }
  }

  plots_html <- render_plots_html(
    resolve_method_pngs(method_name, graphics_dir),
    has_plots = count_plots(result) > 0L
  )
  if (nzchar(plots_html)) {
    parts <- c(parts, plots_html)
  }

  if (length(parts) == 0L) {
    parts <- "<p class=\"report-note\">This method produced no tabular or graphical output.</p>"
  }

  paste(parts, collapse = "\n")
}

#' Build one section entry: its TOC line and its full HTML.
#' @keywords internal
build_section <- function(anchor, title, body, section_tpl) {
  toc_line <- paste0(
    "<li><a href=\"#", anchor, "\">", html_escape(title), "</a></li>"
  )
  html <- fill_template(section_tpl, list(anchor = anchor, title = title, body = body))
  list(toc = toc_line, html = html)
}

#' @title Gather report header metadata from the current session
#' @description
#' Read the run metadata the header shows (package version, render date, data
#' source, options file, autonomy level) from the loaded options. Fields that
#' are not currently set render as "Not recorded", so this works both inside a
#' run (options live) and standalone after one (options restored).
#' @return *\[list\]* Named strings for [build_report_html()]'s `report_meta`.
gather_report_meta <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("artma")),
    error = function(e) "Not recorded"
  )
  source_path <- getOption("artma.data.source_path", NULL)
  options_file <- getOption("artma.temp.file_name", NULL)
  autonomy <- getOption("artma.autonomy.level", NULL)

  list(
    package_version = version,
    render_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    data_source = if (is.null(source_path)) NULL else as.character(source_path),
    options_file = if (is.null(options_file)) NULL else basename(as.character(options_file)),
    autonomy_level = autonomy
  )
}

#' @title Assemble the full HTML report as a string
#' @description
#' Turn a run's results into one self-contained HTML document. Every method
#' becomes a section (skipped and failed methods included, with their reason),
#' preceded by a metadata header and an anchor-linked table of contents.
#'
#' @param results *\[list\]* Named list of method results from `artma()`. The
#'   `skipped_methods` and `failed_methods` attributes, when present, add
#'   sections for methods that never ran.
#' @param report_meta *\[list, optional\]* Named strings for the header:
#'   `package_version`, `render_date`, `data_source`, `options_file`,
#'   `autonomy_level`. Missing entries render as "Not recorded".
#' @param graphics_dir *\[character or NULL, optional\]* Directory the run wrote
#'   its PNG graphics to. When `NULL`, no plots are embedded.
#' @param round_to *\[integer, optional\]* Number of decimals for numeric table
#'   cells. Defaults to the `artma.output.number_of_decimals` option (3).
#' @return *\[character\]* The complete HTML document as a single string.
build_report_html <- function(results,
                              report_meta = list(),
                              graphics_dir = NULL,
                              round_to = NULL) {
  if (!is.list(results)) {
    cli::cli_abort("`results` must be the named list returned by artma().")
  }
  if (is.null(round_to)) {
    round_to <- as.integer(getOption("artma.output.number_of_decimals", 3))
  }

  meta_defaults <- list(
    title = "artma Analysis Report",
    package_version = "Not recorded",
    render_date = "Not recorded",
    data_source = "Not recorded",
    options_file = "Not recorded",
    autonomy_level = "Not recorded"
  )
  meta <- utils::modifyList(meta_defaults, report_meta)
  meta <- lapply(meta, function(v) {
    if (is.null(v) || length(v) != 1L || is.na(v) || !nzchar(as.character(v))) {
      return("Not recorded")
    }
    as.character(v)
  })
  meta$title <- report_meta$title %||% meta_defaults$title

  header_tpl <- read_partial("header")
  section_tpl <- read_partial("section")
  footer_tpl <- read_partial("footer")

  skipped_attr <- attr(results, "skipped_methods")
  failed_attr <- attr(results, "failed_methods")

  toc_lines <- character()
  section_html <- character()

  # Track anchors already used so duplicate method names get distinct ids.
  # An environment avoids `<<-` while keeping the state across make_anchor calls.
  anchor_state <- new.env(parent = emptyenv())
  anchor_state$used <- character()

  make_anchor <- function(name) {
    anchor <- sanitize_anchor(name)
    candidate <- anchor
    n <- 1L
    while (candidate %in% anchor_state$used) {
      n <- n + 1L
      candidate <- paste0(anchor, "-", n)
    }
    anchor_state$used <- c(anchor_state$used, candidate)
    candidate
  }

  # Sections for every method that returned a result (includes self-skipped
  # methods, whose body renders their skip reason).
  for (method_name in names(results)) {
    result <- results[[method_name]]
    if (is.null(result)) {
      next
    }
    title <- humanize_name(method_name)
    body <- build_section_body(result, method_name, graphics_dir, round_to)
    entry <- build_section(make_anchor(method_name), title, body, section_tpl)
    toc_lines <- c(toc_lines, entry$toc)
    section_html <- c(section_html, entry$html)
  }

  # Sections for methods that never ran because a gate skipped them.
  for (method_name in names(skipped_attr)) {
    if (method_name %in% names(results)) {
      next
    }
    reason <- html_escape(as.character(skipped_attr[[method_name]]))
    body <- paste0("<div class=\"report-skip\">This method was skipped: ", reason, "</div>")
    entry <- build_section(
      make_anchor(method_name), humanize_name(method_name), body, section_tpl
    )
    toc_lines <- c(toc_lines, entry$toc)
    section_html <- c(section_html, entry$html)
  }

  # Sections for methods that failed with an error.
  for (method_name in names(failed_attr)) {
    if (method_name %in% names(results)) {
      next
    }
    reason <- html_escape(as.character(failed_attr[[method_name]]))
    body <- paste0("<div class=\"report-skip\">This method failed: ", reason, "</div>")
    entry <- build_section(
      make_anchor(method_name), humanize_name(method_name), body, section_tpl
    )
    toc_lines <- c(toc_lines, entry$toc)
    section_html <- c(section_html, entry$html)
  }

  if (length(toc_lines) == 0L) {
    toc_html <- "<p class=\"report-note\">This run produced no method results.</p>"
  } else {
    toc_html <- paste0("<ul>", paste(toc_lines, collapse = ""), "</ul>")
  }

  header <- fill_template(header_tpl, list(
    title = html_escape(meta$title),
    package_version = html_escape(meta$package_version),
    render_date = html_escape(meta$render_date),
    data_source = html_escape(meta$data_source),
    options_file = html_escape(meta$options_file),
    autonomy_level = html_escape(meta$autonomy_level),
    toc = toc_html
  ))

  paste0(
    header, "\n",
    paste(section_html, collapse = "\n"), "\n",
    footer_tpl
  )
}

#' @title Render a run's results to a self-contained HTML file
#' @description
#' Build the HTML report (see [build_report_html()]) and write it to
#' `output_file`. Optionally opens it in the browser afterwards.
#'
#' @param results *\[list\]* Named list of method results from `artma()`.
#' @param output_file *\[character\]* Absolute path of the HTML file to write.
#' @param graphics_dir *\[character or NULL, optional\]* Directory holding the
#'   run's exported graphics. Defaults to `NULL` (no plots embedded).
#' @param report_meta *\[list, optional\]* Header metadata (see
#'   [build_report_html()]).
#' @param round_to *\[integer, optional\]* Decimals for numeric table cells.
#' @param open *\[logical, optional\]* Whether to open the file in a browser.
#'   Only ever honoured in an interactive session. Defaults to `FALSE`.
#' @return *\[character\]* The path of the written file (invisibly).
render_report <- function(results,
                          output_file,
                          graphics_dir = NULL,
                          report_meta = list(),
                          round_to = NULL,
                          open = FALSE) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (!is.character(output_file) || length(output_file) != 1L || !nzchar(output_file)) {
    cli::cli_abort("`output_file` must be a single non-empty file path.")
  }

  html <- build_report_html(
    results,
    report_meta = report_meta,
    graphics_dir = graphics_dir,
    round_to = round_to
  )

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(html, output_file, useBytes = TRUE)

  if (get_verbosity() >= 3) {
    cli::cli_alert_success("HTML report written to {.path {output_file}}")
  }

  if (isTRUE(open) && interactive()) {
    tryCatch(
      utils::browseURL(output_file),
      error = function(e) {
        if (get_verbosity() >= 2) {
          cli::cli_alert_warning("Could not open the report: {e$message}")
        }
      }
    )
  }

  invisible(output_file)
}

box::export(
  build_report_html,
  embed_png_data_uri,
  gather_report_meta,
  humanize_name,
  render_report,
  render_table_html,
  resolve_method_pngs,
  sanitize_anchor
)
