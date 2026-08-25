# Run meta-analysis with artma

Main entry point for the artma package. This function orchestrates the
complete meta-analysis workflow: loading options, preparing data, and
running specified analytical methods.

## Usage

``` r
artma(
  data = NULL,
  methods = NULL,
  options = NULL,
  options_dir = NULL,
  open_results = FALSE,
  ...
)
```

## Arguments

- data:

  *\[data.frame, optional\]* Data frame to analyze. If `NULL`, data will
  be loaded from the options file (see `options` parameter). When
  provided, this data will be used directly, bypassing the data reading
  step.

- methods:

  *\[character, optional\]* A character vector of method names to run.
  Use `"all"` to run all available methods. If `NULL` and running
  interactively, the session hub opens: a menu loop for picking and
  running methods, previewing data, and opening results across repeated
  runs. See
  [`artma::methods_list()`](https://petrcala.github.io/artma/reference/methods_list.md)
  for available methods.

- options:

  *\[character, optional\]* Name of the options file (with or without
  `.yaml` extension) to use. If `NULL` and running interactively, you
  will be prompted to create or select an options file.

- options_dir:

  *\[character, optional\]* Directory containing the options file. If
  `NULL`, uses the default options directory.

- open_results:

  *\[logical, optional\]* Whether to open the results directory after
  exporting results. Defaults to `FALSE`.

- ...:

  Additional arguments passed to the runtime methods.

## Value

*\[list\]* A named list containing results from each method, indexed by
method name. The structure of each result depends on the specific
method. Methods that fail are omitted from the list; their names and
error messages are attached as the `failed_methods` attribute. The
`run_info` attribute carries the run's identity: the methods requested,
the effective seed, and the files each method wrote. A session hub call
additionally attaches a `runs` attribute with one entry per run
(methods, seed, timestamp) and keeps the latest result per method across
the session's runs.

## Details

The `artma()` function is the primary way to interact with the artma
package. It handles the complete workflow:

1.  **Options Loading**: Loads configuration from an options file (or
    prompts for creation in interactive mode)

2.  **Data Preparation**: Reads and prepares your data (unless `data` is
    provided)

3.  **Method Execution**: Runs the specified analytical methods on your
    data

4.  **Results**: Returns a structured list of results

### Options Files

Options files are YAML configuration files that store all settings for
your analysis, including data paths, column mappings, method parameters,
and output preferences. They ensure reproducibility and make it easy to
manage multiple analysis configurations.

### Methods

Methods are analytical functions that perform specific meta-analysis
tasks (e.g., funnel plots, Bayesian Model Averaging, effect size
calculations). You can run multiple methods in a single call, and they
will execute in a predefined order.

### Data Parameter

When `data` is provided, it bypasses the data reading step and uses your
data frame directly. The data will still be preprocessed and validated
according to your options configuration. This is useful when you already
have data loaded in R or want to analyze data programmatically.

### Parallel Execution

Methods that do not depend on one another form a dependency layer and
run concurrently in forked workers, so a run costs roughly the slowest
method per layer rather than the sum of all of them. Each method's CLI
output is captured and replayed in discovery order once the layer
finishes, so the console reads like a sequential run.

Set `general.parallel` to `FALSE` in the options file to disable this.
Execution also falls back to sequential automatically on Windows, on
single-core machines, and in interactive sessions whose autonomy level
still allows methods to prompt.

Every method receives its own L'Ecuyer-CMRG stream derived from the
`general.seed` option and the method's name, so stochastic methods
(bootstrap, MCMC) draw the same numbers whether the run was parallel or
sequential, and regardless of which other methods ran alongside. Setting
`general.seed` to `NA` derives the run seed from the session RNG
instead, so calling [`set.seed()`](https://rdrr.io/r/base/Random.html)
before `artma()` governs reproducibility the way it does for any
stochastic R function.

### Run Manifest

Every run that saves results writes a `run.json` into its output
directory: when it ran, the options file and data source behind it, the
methods requested, run, skipped and failed, the effective seed, and the
files it wrote. The file list is recorded as the files are written, so
it describes this run rather than the accumulated contents of the
directory, and it is what the HTML report uses to find each method's
plots.

The manifest is overwritten on every run into the same output directory:
it always describes the latest run, never a history. Runs driven by
different options files already get their own output directory, so keep
a run by copying its directory or by pointing `output.dir` somewhere
per-run.

### Method Failures

A method that throws an error does not abort the run. The failing method
is skipped with a warning, the remaining methods still execute, and
results from the methods that succeeded are exported as usual. A summary
of successes and failures is printed at the end of the run. The run
itself never signals an error because of a method failure; when every
requested method fails, a final warning is emitted instead. Failed
method names and their error messages are available in the
`failed_methods` attribute of the returned list.

## See also

- [`artma::methods_list()`](https://petrcala.github.io/artma/reference/methods_list.md) -
  List available methods

- [`artma::options_create()`](https://petrcala.github.io/artma/reference/options_create.md) -
  Create a new options file

- `artma::prepare_data()` - Prepare data manually

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive mode - will prompt for options and methods
results <- artma()

# Run specific methods with an options file
results <- artma(
  methods = c("funnel_plot", "bma", "fma"),
  options = "my_analysis.yaml"
)

# Run all methods
results <- artma(methods = "all", options = "my_analysis.yaml")

# Use data directly (bypasses file reading)
my_data <- data.frame(
  effect = c(0.5, 0.3, 0.7),
  se = c(0.1, 0.15, 0.12),
  study_id = c("Study A", "Study B", "Study C")
)
results <- artma(data = my_data, methods = "funnel_plot")

# Access results
funnel_result <- results$funnel_plot
} # }
```
