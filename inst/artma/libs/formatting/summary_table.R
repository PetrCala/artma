#' @title Shared summary-table builder
#' @description
#' One pivoted-table builder shared by the publication-bias/effect summary
#' tables (linear tests, non-linear tests, exogeneity tests): a `Metric`
#' column of row labels plus one column per model/method, each holding
#' already-formatted character values.
NULL

box::use(
  artma / libs / core / validation[validate]
)

#' @title Build a pivoted publication-bias/effect summary table
#' @description
#' Assembles the `Metric` + one-column-per-model summary tables used across
#' the testing methods. Callers pass already-formatted character columns; this
#' function pads short columns and replaces `NA` entries with `missing_value`,
#' so a metric a given model could not produce (e.g. a zero-length test
#' statistic) does not abort the table assembly.
#' @param row_labels *[character]* Row labels. Becomes the `Metric` column and
#'   the data frame's `row.names`.
#' @param columns *[list]* Named list of character vectors, one per model/
#'   method column. Each vector is padded to `length(row_labels)` with
#'   `missing_value` when shorter, and truncated when longer.
#' @param missing_value *[character, optional]* Value used to pad short
#'   columns and to replace `NA` entries. Defaults to `NA_character_`, which
#'   leaves `NA` entries as `NA` (no replacement).
#' @return *[data.frame]* `Metric` column plus one column per entry in
#'   `columns`, with `row.names` set to `row_labels`.
build_summary_table <- function(row_labels, columns, missing_value = NA_character_) {
  validate(is.character(row_labels), is.list(columns))

  n <- length(row_labels)
  summary <- data.frame(
    Metric = row_labels,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (column_name in names(columns)) {
    values <- as.character(columns[[column_name]])
    if (length(values) < n) {
      values <- c(values, rep(missing_value, n - length(values)))
    }
    values <- values[seq_len(n)]
    values[is.na(values)] <- missing_value
    summary[[column_name]] <- values
  }

  attr(summary, "row.names") <- row_labels
  summary
}

box::export(build_summary_table)
