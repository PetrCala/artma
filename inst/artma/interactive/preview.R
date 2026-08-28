#' @title Data preview submenu
#' @description
#' The hub's "Preview data" flow: an overview screen printed on entry (size,
#' missingness, and a meta-analysis health panel over the effect and standard
#' error columns), then a small submenu offering the per-column profile, the
#' per-study profile, and the spreadsheet viewer when one is available.
#' Everything renders as plain terminal output, so the flow degrades to
#' nothing worse than text in a viewer-less session (SSH, bare R).
NULL

box::use(
  artma / const[CONST],
  artma / data / utils[get_colnames_map],
  artma / interactive / input[ask_select],
  artma / interactive / menu[compose_menu_choices, menu_item],
  artma / libs / core / utils[data_viewer_available],
  artma / libs / core / validation[validate],
  artma / modules / methods_table[fit_column_widths, pad_cell, truncate_cell]
)

# How many studies the Studies screen lists before cutting off.
MAX_STUDY_ROWS <- 10L

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

#' @title A frame's numeric column, or NULL
#' @keywords internal
numeric_column <- function(df, name) {
  values <- df[[name]]
  if (is.numeric(values)) values else NULL
}

#' @title Format a number for the preview screens
#' @keywords internal
format_stat <- function(value) {
  format(signif(value, 4), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' @title The meta-analysis health lines of the overview screen
#' @description Mean and precision-weighted mean effect, the share of
#'   estimates significant at the 5% level, the sign split, and the largest
#'   |t|; every line is guarded, so a frame without numeric `effect` or `se`
#'   columns simply contributes fewer lines.
#' @param df *\[data.frame\]* The prepared data frame.
#' @return *\[character\]* Zero or more bullet lines.
#' @keywords internal
health_lines <- function(df) {
  effect <- numeric_column(df, "effect")
  se <- numeric_column(df, "se")
  lines <- character(0)

  if (!is.null(effect) && any(!is.na(effect))) {
    line <- sprintf("Mean effect: %s", format_stat(mean(effect, na.rm = TRUE)))
    if (!is.null(se)) {
      valid <- !is.na(effect) & !is.na(se) & se > 0
      if (any(valid)) {
        weights <- 1 / se[valid]^2
        weighted <- sum(weights * effect[valid]) / sum(weights)
        line <- sprintf("%s (precision-weighted: %s)", line, format_stat(weighted))
      }
    }
    lines <- c(lines, line)

    n_positive <- sum(effect > 0, na.rm = TRUE)
    n_negative <- sum(effect < 0, na.rm = TRUE)
    n_zero <- sum(effect == 0, na.rm = TRUE)
    sign_parts <- c(
      sprintf("%s positive", format(n_positive, big.mark = ",")),
      sprintf("%s negative", format(n_negative, big.mark = ",")),
      if (n_zero > 0L) sprintf("%s zero", format(n_zero, big.mark = ","))
    )
    lines <- c(lines, sprintf("Sign split: %s", paste(sign_parts, collapse = ", ")))
  }

  if (!is.null(effect) && !is.null(se)) {
    valid <- !is.na(effect) & !is.na(se) & se > 0
    if (any(valid)) {
      t_values <- effect[valid] / se[valid]
      n_significant <- sum(abs(t_values) >= stats::qnorm(0.975))
      lines <- c(
        lines,
        sprintf(
          "Significant at 5%%: %s of %s estimates (%.0f%%)",
          format(n_significant, big.mark = ","),
          format(length(t_values), big.mark = ","),
          100 * n_significant / length(t_values)
        ),
        sprintf("Largest |t|: %s", format_stat(max(abs(t_values))))
      )
    }
  }

  lines
}

#' @title Print the overview screen of the prepared frame
#' @description Rows and columns, the study count where the config knows the
#'   study column, missing-value counts, the meta-analysis health panel, and
#'   the effect and standard-error ranges.
#' @param df *\[data.frame\]* The prepared data frame.
#' @return `NULL`, invisibly.
render_data_overview <- function(df) {
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

  for (line in health_lines(df)) {
    lines <- c(lines, "*" = line)
  }

  numeric_range_line <- function(column, label) {
    values <- numeric_column(df, column)
    if (is.null(values) || all(is.na(values))) {
      return(NULL)
    }
    bounds <- range(values, na.rm = TRUE)
    sprintf("%s: %s to %s", label, format_stat(bounds[[1]]), format_stat(bounds[[2]]))
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

#' @title A prepared column's pipeline role
#' @description `required` and `computed` follow the constant column sets;
#'   everything else in the prepared frame is a candidate moderator.
#' @param name *\[character\]* A prepared frame column name.
#' @return *\[character\]* One of `required`, `computed`, `moderator`.
#' @keywords internal
column_role <- function(name) {
  if (name %in% CONST$DATA$REQUIRED_COLNAMES) {
    return("required")
  }
  if (name %in% CONST$DATA$COMPUTED_COLNAMES) {
    return("computed")
  }
  "moderator"
}

#' @title Summarize one column's values in a phrase
#' @keywords internal
summarize_column <- function(values) {
  non_missing <- values[!is.na(values)]
  if (length(non_missing) == 0L) {
    return("all missing")
  }
  if (is.numeric(values)) {
    return(sprintf(
      "min %s, median %s, max %s",
      format_stat(min(non_missing)),
      format_stat(stats::median(non_missing)),
      format_stat(max(non_missing))
    ))
  }
  n_unique <- length(unique(non_missing))
  sprintf("%d unique value%s", n_unique, if (n_unique == 1L) "" else "s")
}

#' @title The per-column profile of the prepared frame
#' @description One row per column: the pipeline role, the source column it
#'   was renamed from (empty for identity mappings and computed columns), the
#'   storage type, the missing count, and a value summary.
#' @param df *\[data.frame\]* The prepared data frame.
#' @return *\[data.frame\]* Columns `column`, `role`, `source`, `type`,
#'   `missing`, `summary`.
column_overview_frame <- function(df) {
  validate(is.data.frame(df))
  colnames_map <- get_colnames_map()

  rows <- lapply(names(df), function(name) {
    source_name <- colnames_map[[name]]
    if (is.null(source_name) || identical(source_name, name)) {
      source_name <- ""
    }
    data.frame(
      column = name,
      role = column_role(name),
      source = source_name,
      type = class(df[[name]])[[1]],
      missing = sum(is.na(df[[name]])),
      summary = summarize_column(df[[name]]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @title Render a small preview table as text lines
#' @description Header, dash rule, one line per row; the columns named by
#'   `shrink_order` give way first when the console is narrow.
#' @param df *\[data.frame\]* The frame to render; all cells are formatted with
#'   `format()`.
#' @param shrink_order *\[character\]* Column names, most shrinkable first.
#' @param width *\[numeric, optional\]* Console width to fit into.
#' @return *\[character\]* The rendered lines.
#' @keywords internal
format_preview_table <- function(df, shrink_order, width = NULL) {
  width <- width %||% cli::console_width()

  cells <- lapply(df, function(column) format(column, trim = TRUE, big.mark = ","))
  headers <- names(df)

  natural <- stats::setNames(lapply(seq_along(headers), function(i) {
    max(nchar(c(headers[[i]], cells[[i]])))
  }), headers)
  minimum <- stats::setNames(as.list(rep(8L, length(headers))), headers)
  widths <- fit_column_widths(natural, minimum, shrink_order, width)

  build_row <- function(values) {
    padded <- vapply(headers, function(name) {
      pad_cell(truncate_cell(values[[name]], widths[[name]]), widths[[name]])
    }, character(1))
    trimws(paste(padded, collapse = "  "), which = "right")
  }

  rule <- build_row(stats::setNames(
    lapply(widths, function(w) strrep("-", w)), headers
  ))
  rows <- vapply(seq_len(nrow(df)), function(i) {
    build_row(stats::setNames(
      lapply(cells, function(column) column[[i]]), headers
    ))
  }, character(1))

  c(build_row(stats::setNames(as.list(headers), headers)), rule, rows)
}

#' @title Print the per-column profile
#' @param df *\[data.frame\]* The prepared data frame.
#' @param width *\[numeric, optional\]* Console width.
#' @return `NULL`, invisibly.
render_column_table <- function(df, width = NULL) {
  frame <- column_overview_frame(df)
  for (line in format_preview_table(frame, shrink_order = c("summary", "source", "column"), width = width)) {
    cli::cli_verbatim(line)
  }
  invisible(NULL)
}

#' @title The per-study profile of the prepared frame
#' @description One row per study, sorted by estimate count (largest first):
#'   the estimate count, and the mean effect and standard error where those
#'   columns are numeric.
#' @param df *\[data.frame\]* The prepared data frame; must carry `study_id`.
#' @return *\[data.frame\]* Columns `study`, `estimates`, and (when available)
#'   `mean_effect` and `mean_se`.
study_overview_frame <- function(df) {
  validate(is.data.frame(df), "study_id" %in% names(df))

  ids <- df[["study_id"]]
  keep <- !is.na(ids)
  groups <- split(seq_len(nrow(df))[keep], as.character(ids[keep]))

  effect <- numeric_column(df, "effect")
  se <- numeric_column(df, "se")

  frame <- data.frame(
    study = names(groups),
    estimates = vapply(groups, length, integer(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  group_mean <- function(values) {
    vapply(groups, function(rows) mean(values[rows], na.rm = TRUE), numeric(1))
  }
  if (!is.null(effect)) {
    frame$mean_effect <- signif(group_mean(effect), 4)
  }
  if (!is.null(se)) {
    frame$mean_se <- signif(group_mean(se), 4)
  }

  frame[order(-frame$estimates, frame$study), , drop = FALSE]
}

#' @title Print the per-study profile
#' @description A one-line spread of estimates per study, then the largest
#'   studies as a table (capped at `MAX_STUDY_ROWS` rows, saying so when the
#'   cap bites). A frame without a resolved study column reports that instead.
#' @param df *\[data.frame\]* The prepared data frame.
#' @param width *\[numeric, optional\]* Console width.
#' @return `NULL`, invisibly.
render_study_summary <- function(df, width = NULL) {
  if (!("study_id" %in% names(df))) {
    cli::cli_alert_info(
      "The study column is not resolved for this data, so there is no per-study view."
    )
    return(invisible(NULL))
  }

  frame <- study_overview_frame(df)
  counts <- frame$estimates
  cli::cli_inform(c("*" = sprintf(
    "%s studies; estimates per study: min %s, median %s, max %s",
    format(nrow(frame), big.mark = ","),
    format_stat(min(counts)), format_stat(stats::median(counts)), format_stat(max(counts))
  )))

  shown <- utils::head(frame, MAX_STUDY_ROWS)
  for (line in format_preview_table(shown, shrink_order = "study", width = width)) {
    cli::cli_verbatim(line)
  }
  if (nrow(frame) > nrow(shown)) {
    cli::cli_alert_info(
      "Showing the {nrow(shown)} largest of {nrow(frame)} studies; open the spreadsheet viewer for the full data."
    )
  }
  invisible(NULL)
}

#' @title The preview submenu items
#' @param viewer_available *\[logical\]* Whether the spreadsheet-viewer item
#'   is offered.
#' @return *\[list\]* Items for `compose_menu_choices()`.
preview_menu_items <- function(viewer_available) {
  items <- list(
    menu_item(
      value = "overview",
      name = "Overview",
      description = "size, missingness, effect and significance summary"
    ),
    menu_item(
      value = "columns",
      name = "Columns",
      description = "role, type, missingness and range per column"
    ),
    menu_item(
      value = "studies",
      name = "Studies",
      description = "per-study estimate counts and mean effects"
    )
  )
  if (isTRUE(viewer_available)) {
    items <- c(items, list(menu_item(
      value = "view",
      name = "Open spreadsheet viewer",
      description = "the prepared frame in the data viewer"
    )))
  }
  c(items, list(menu_item(
    value = "back",
    name = "Back",
    description = "return to the session menu"
  )))
}

#' @title Run the preview submenu
#' @description Prints the overview screen on entry, then loops the submenu
#'   until Back or a cancelled menu.
#' @param df *\[data.frame\]* The prepared data frame.
#' @param view_data *\[function\]* Opens the frame in a spreadsheet viewer.
#' @param select_fn *\[function, optional\]* Menu backend. Exposed for testing.
#' @param width *\[numeric, optional\]* Console width for the tables; the menu sizes itself.
#' @param viewer_available *\[function, optional\]* Availability probe for the
#'   spreadsheet viewer. Defaults to `data_viewer_available()`. Injectable for
#'   testing.
#' @return `NULL`, invisibly.
run_preview_menu <- function(
  df,
  view_data,
  select_fn = climenu::select,
  width = NULL,
  viewer_available = data_viewer_available
) {
  validate(
    is.data.frame(df),
    is.function(view_data),
    is.function(select_fn),
    is.function(viewer_available)
  )

  render_data_overview(df)

  repeat {
    preview_menu <- compose_menu_choices(preview_menu_items(isTRUE(viewer_available())))
    action <- ask_select(
      question = "Preview data",
      choices = preview_menu$choices,
      descriptions = preview_menu$descriptions,
      select_fn = select_fn
    )
    if (rlang::is_empty(action) || identical(action, "back")) {
      break
    }

    if (identical(action, "overview")) {
      render_data_overview(df)
    } else if (identical(action, "columns")) {
      render_column_table(df, width = width)
    } else if (identical(action, "studies")) {
      render_study_summary(df, width = width)
    } else if (identical(action, "view")) {
      tryCatch(
        view_data(df),
        error = function(e) cli::cli_alert_warning("Could not open the viewer: {conditionMessage(e)}")
      )
    }
  }

  invisible(NULL)
}

box::export(
  column_overview_frame,
  count_studies,
  preview_menu_items,
  render_column_table,
  render_data_overview,
  render_study_summary,
  run_preview_menu,
  study_overview_frame
)
