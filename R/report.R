#' @title Render an HTML report of a run's results
#' @description
#' Turn the results of an `artma()` run into a single self-contained HTML file:
#' a metadata header, a table of contents, and one section per method with its
#' tables and plots. The file has no external dependencies (all styling is
#' inline and every plot is embedded as a base64 PNG), so it can be shared on
#' its own.
#'
#' The report can also be produced automatically at the end of every run by
#' setting the `output.report` option to `TRUE`.
#'
#' @param results *\[list\]* The named list returned by [artma()], one
#'   `new_method_result` per method.
#' @param output_file *\[character, optional\]* Absolute path of the HTML file to
#'   write. When `NULL` (default), the report is written as `report.html` inside
#'   the most recent run's resolved output directory.
#' @param open *\[logical, optional\]* Whether to open the report in a browser
#'   after writing it. Only ever honoured in an interactive session. Defaults to
#'   `interactive()`.
#'
#' @details
#' Plots come from the `run.json` manifest a run leaves in its output directory:
#' it records which files each method wrote, so the report embeds exactly those.
#' A results directory without a manifest (written before manifests existed, or
#' by a run with `output.save_results` off) yields a report with tables only.
#'
#' @return *\[character\]* The path of the written report file (invisibly).
#' @export
#' @examples
#' \dontrun{
#' res <- artma(methods = c("funnel_plot", "effect_summary_stats"), options = "my.yaml")
#' report_render(res)
#'
#' # Write to a specific location and open it
#' report_render(res, output_file = "~/analysis/report.html", open = TRUE)
#' }
report_render <- function(results, output_file = NULL, open = interactive()) {
  box::use(
    artma / output / run_manifest[manifest_plot_index, read_run_manifest],
    artma / report / render[gather_report_meta, render_report]
  )

  if (!is.list(results)) {
    cli::cli_abort(c(
      "x" = "{.arg results} must be the named list returned by {.code artma()}.",
      "i" = "Run {.code res <- artma(...)} first, then {.code report_render(res)}."
    ))
  }

  output_dir <- NULL
  if (is.null(output_file)) {
    output_dir <- read_last_export_dir() # nolint: box_usage_linter. # Package function from R/results.R
    if (is.null(output_dir)) {
      cli::cli_abort(c(
        "x" = "Could not determine where to write the report.",
        "i" = "Pass an explicit {.arg output_file}, or run {.code artma()} first so a results directory exists."
      ))
    }
    output_file <- file.path(output_dir, "report.html")
  } else {
    output_dir <- dirname(output_file)
  }

  # The manifest lives with the run's outputs. When the report is written
  # somewhere else entirely, fall back to the last run's directory, which is
  # the run the results in hand came from.
  manifest <- read_run_manifest(output_dir)
  if (is.null(manifest)) {
    manifest <- read_run_manifest(read_last_export_dir()) # nolint: box_usage_linter. # Package function from R/results.R
  }

  render_report(
    results = results,
    output_file = output_file,
    plot_index = manifest_plot_index(manifest),
    report_meta = gather_report_meta(),
    open = open
  )
}

# Deprecated dotted alias (see contributingGuides/API.md). Removed after 1.0.

#' @rdname artma-deprecated
#' @export
report.render <- function(...) {
  lifecycle::deprecate_warn("0.4.0", "report.render()", "report_render()")
  report_render(...)
}
