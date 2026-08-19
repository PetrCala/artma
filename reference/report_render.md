# Render an HTML report of a run's results

Turn the results of an
[`artma()`](https://petrcala.github.io/artma/reference/artma.md) run
into a single self-contained HTML file: a metadata header, a table of
contents, and one section per method with its tables and plots. The file
has no external dependencies (all styling is inline and every plot is
embedded as a base64 PNG), so it can be shared on its own.

The report can also be produced automatically at the end of every run by
setting the `output.report` option to `TRUE`.

## Usage

``` r
report_render(results, output_file = NULL, open = interactive())
```

## Arguments

- results:

  *\[list\]* The named list returned by
  [`artma()`](https://petrcala.github.io/artma/reference/artma.md), one
  `new_method_result` per method.

- output_file:

  *\[character, optional\]* Absolute path of the HTML file to write.
  When `NULL` (default), the report is written as `report.html` inside
  the most recent run's resolved output directory.

- open:

  *\[logical, optional\]* Whether to open the report in a browser after
  writing it. Only ever honoured in an interactive session. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

*\[character\]* The path of the written report file (invisibly).

## Details

Plots come from the `run.json` manifest a run leaves in its output
directory: it records which files each method wrote, so the report
embeds exactly those. A results directory without a manifest (written
before manifests existed, or by a run with `output.save_results` off)
yields a report with tables only.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- artma(methods = c("funnel_plot", "effect_summary_stats"), options = "my.yaml")
report_render(res)

# Write to a specific location and open it
report_render(res, output_file = "~/analysis/report.html", open = TRUE)
} # }
```
