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
#' @return *\[character\]* The path of the written report file (invisibly).
#' @export
#' @examples
#' \dontrun{
#' res <- artma(methods = c("funnel_plot", "effect_summary_stats"), options = "my.yaml")
#' report.render(res)
#'
#' # Write to a specific location and open it
#' report.render(res, output_file = "~/analysis/report.html", open = TRUE)
#' }
report.render <- function(results, output_file = NULL, open = interactive()) {
  box::use(
    artma / output / export[resolve_graphics_dir],
    artma / report / render[gather_report_meta, render_report]
  )

  if (!is.list(results)) {
    cli::cli_abort(c(
      "x" = "{.arg results} must be the named list returned by {.code artma()}.",
      "i" = "Run {.code res <- artma(...)} first, then {.code report.render(res)}."
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

  graphics_dir <- resolve_graphics_dir(output_dir)

  render_report(
    results = results,
    output_file = output_file,
    graphics_dir = graphics_dir,
    report_meta = gather_report_meta(),
    open = open
  )
}
