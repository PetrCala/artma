# Runtime Methods

Runtime methods are the analytical functions users invoke via `artma::artma(methods = c("method_name"))`. Each lives in `inst/artma/methods/<method_name>.R` as a plain implementation function; `register_runtime_method()` (from `inst/artma/modules/runtime_methods.R`) produces the exported `run` wrapper, adding the shared caching layer and declarative metadata.

## Registration

```r
box::use(
  artma / modules / runtime_methods[new_method_result, register_runtime_method]
)

my_method <- function(df, bma_result = NULL, ...) {
  new_method_result(
    tables = list(summary = summary_df),    # rounded display tables
    estimates = my_method_estimates(model), # unrounded, fixed schema
    plots = list(),
    meta = list()
  )
}

run <- register_runtime_method(
  my_method,
  stage = "my_method",       # conventionally matches the implementation name
  description = "One line on what the method does",
  depends_on = "bma",        # methods that must run first
  required_columns = c("effect", "se"),
  suggests = "BMS"           # optional packages the method needs
)

box::export(my_method, run)
```

Export `run` plus the implementation (tests import it); do not export other internals unless another module genuinely reuses them. Methods are auto-discovered by scanning `inst/artma/methods/`; `artma::methods_list()` renders the registered metadata of every discovered method as a console table and returns it as a data frame (`inst/artma/modules/methods_table.R`), and the methods-overview vignette repeats the same facts under a parity test. `df` is the preprocessed data frame; other arguments come from the options system.

## Metadata

`description` is required in practice: `tests/testthat/test-methods-table.R` fails when a discovered method registers none. It is the one-line summary `artma::methods_list()` prints, so keep it to a single line and lead with what the method produces; the printed table truncates it to the console width.

The remaining metadata arguments are optional:

- `depends_on`: the orchestrator (`invoke_runtime_methods()` in `R/artma.R`) topologically sorts by these edges (erroring on cycles) and passes each upstream result as a `<dependency>_result` argument, so `depends_on = "bma"` yields a `bma_result` parameter. Discovery order is preserved among independent methods.
- `required_columns`: a method whose columns are missing from the data is skipped with an explanation instead of aborting the run.
- `suggests`: the single declarative gate for optional packages. Missing packages soft-skip the method with a message, plus an interactive install offer. Exception: a non-interactive run requesting exactly that single method hard-aborts so scripts get a clear signal. Packages powering only an optional sub-model (e.g. `bayesm` in `nonlinear_tests`) stay as call-site `requireNamespace()` guards instead.

## Return contract

Every method returns `new_method_result()`, a list with four slots:

- `tables`: named list of `data.frame`s exported by `export_method_result` (`inst/artma/output/export.R`). These are the display artifacts: rounded, formatted, laid out for a human.
- `estimates`: a single long-format `data.frame` of the method's unrounded numbers, built with `new_estimates()`. This is the machine-readable artifact.
- `plots`: named list of plot objects for programmatic access and printing. Graphics files are written by each method during execution, not by the exporter.
- `meta`: anything else downstream consumers need (models, fit params, skip reasons, auxiliary frames).

Methods with a custom print method pass `class =` to `new_method_result()` and read their fields from `meta`/`plots` in `R/print.R`.

## Export naming

- Table keys `summary`/`coefficients`/`table` (or a key equal to the method name) export as `<method>.csv`; any other key as `<method>_<key>.csv`.
- When a result carries a non-empty `estimates` frame, the estimates take the `<method>.csv` name and the display table moves to `<method>_display.csv`. An empty frame is treated as no estimates at all, so no header-only CSV is written.
- LaTeX output stays driven by the display table under its original `<method>.tex` name, since it is inherently presentational.

## Estimates schema

`new_estimates()` (`inst/artma/modules/runtime_methods.R`) normalises a method's numbers into one schema shared by every method, so results bind across methods without knowing which produced them. The columns are fixed; unknown ones are an error, and anything outside the schema belongs in `meta`:

`method`, `model`, `term`, `estimate`, `std_error`, `statistic`, `p_value`, `conf_low`, `conf_high`, `n_obs`, `n_clusters`, `note`

Columns a method does not fill come back as typed `NA`s. `linear_tests` (`linear_tests_estimates()`) is the reference implementation. Every method builds its frame in a standalone `*_estimates()` function that takes the method's own numeric results and returns the shared schema, so the mapping is unit-testable without running the method.

Every method that reports numbers must fill the slot: `tests/testthat/test-method-estimates-contract.R` walks each discovered method's parse tree and fails when `new_method_result()` is called without an `estimates` argument. Plot-only methods (`funnel_plot`, `box_plot`, `t_stat_histogram`, `prima_facie_graphs`) are listed there as the explicit exception.

Rounding is a display concern: nothing on the `estimates` path may call `format_number()` or read `artma.output.number_of_decimals`.
