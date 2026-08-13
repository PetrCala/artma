#' @title Closing run summary
#' @description
#' The console rendering of what a run did: which methods ran, which were
#' skipped or failed and why, how many tables and plots were written, and
#' where. It reads the same sources as the per-run manifest
#' (`artma/output/run_manifest`), so `run.json` and the closing block on the
#' console never disagree.
#'
#' The summary prints on every run, not only when something went wrong; a clean
#' run is exactly the case where the reader has no other way of telling what
#' was produced. It is an info-level block, gated once in
#' `render_run_summary()`.

box::use(
  artma / libs / core / log[is_info_enabled]
)

#' @title Summarise a finished run
#' @description Collect the facts the closing block reports. Split from the
#'   rendering so the content can be tested without capturing console output.
#' @param results *\[list\]* The value returned by `invoke_runtime_methods()`,
#'   carrying the `run_info`, `skipped_methods` and `failed_methods`
#'   attributes.
#' @param output_dir *\[character, optional\]* The run's output directory, or
#'   `NULL` when results were not saved.
#' @param run_files *\[character, optional\]* Paths recorded outside the
#'   methods themselves (exported tables, the HTML report).
#' @return *\[list\]* With `requested`, `ran`, `skipped`, `failed`, the
#'   `tables`, `graphics` and `other` file counts, and `output_dir`.
summarise_run <- function(results, output_dir = NULL, run_files = character()) {
  box::use(artma / output / run_manifest[classify_run_files])

  run_info <- attr(results, "run_info")
  if (!is.list(run_info)) {
    run_info <- list()
  }

  method_files <- run_info$output_files
  if (!is.list(method_files)) {
    method_files <- list()
  }

  # `ma_table` is assembled by the orchestrator from other methods' estimates
  # rather than run as a method; the manifest leaves it out and so does this.
  ran <- setdiff(names(results) %||% character(), "ma_table")
  requested <- as.character(run_info$methods_requested %||% ran)

  has_dir <- !is.null(output_dir) &&
    length(output_dir) == 1L &&
    !is.na(output_dir) &&
    nzchar(output_dir)

  counts <- list(tables = 0L, graphics = 0L, other = 0L)
  if (has_dir) {
    files <- unique(c(
      as.character(run_files),
      as.character(unlist(method_files, use.names = FALSE))
    ))
    counts <- lapply(classify_run_files(files, output_dir), length)
  }

  list(
    requested = requested,
    ran = ran,
    skipped = attr(results, "skipped_methods") %||% character(),
    failed = attr(results, "failed_methods") %||% character(),
    tables = counts$tables,
    graphics = counts$graphics,
    other = counts$other,
    output_dir = if (has_dir) output_dir else NULL
  )
}

#' @title Print the closing run summary
#' @inheritParams summarise_run
#' @return *\[list\]* The summary that was rendered, invisibly.
render_run_summary <- function(results, output_dir = NULL, run_files = character()) {
  run <- summarise_run(results, output_dir = output_dir, run_files = run_files)

  if (!is_info_enabled()) {
    return(invisible(run))
  }

  cli::cli_h1("Run summary")

  n_requested <- length(run$requested)
  if (length(run$ran) > 0L) {
    cli::cli_alert_success(
      "Ran {length(run$ran)} of {n_requested} requested method{?s}: {.code {run$ran}}"
    )
  } else {
    cli::cli_alert_warning("No method produced results.")
  }

  for (method_name in names(run$skipped)) {
    cli::cli_alert_info("Skipped {.code {method_name}}: {run$skipped[[method_name]]}")
  }
  for (method_name in names(run$failed)) {
    cli::cli_alert_danger("Failed {.code {method_name}}: {run$failed[[method_name]]}")
  }

  if (is.null(run$output_dir)) {
    cli::cli_alert_info("Results were not saved to disk.")
  } else {
    cli::cli_alert_info(
      "Wrote {run$tables} table{?s} and {run$graphics} plot{?s} to {.path {run$output_dir}}"
    )
  }

  invisible(run)
}

box::export(
  render_run_summary,
  summarise_run
)
