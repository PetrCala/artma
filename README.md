<div align="center">
    <h1>
        Automatic Replication Tools for Meta-analysis
    </h1>
    <h4>
    Effortlessly bridging the gap between data and models
    </h4>

  <!-- badges: start -->

[![R build status](https://github.com/PetrCala/artma/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PetrCala/artma/actions/workflows/R-CMD-check.yaml)
[![CRAN Status](https://www.r-pkg.org/badges/version/artma)](https://cran.r-project.org/package=artma)
[![codecov](https://codecov.io/gh/PetrCala/artma/graph/badge.svg?token=6XNXVDOT80)](https://codecov.io/gh/PetrCala/artma)

  <!-- badges: end -->

</div>

**artma** provides a unified interface for running a wide range of meta-analysis methods directly on your data. Point it at a dataset, answer a few questions (or configure everything up front in a YAML file), and it runs the analyses, exports tables and plots, and returns the results as R objects.

- [Installation](#installation)
- [Quick start](#quick-start)
- [How a run works](#how-a-run-works)
- [Available methods](#available-methods)
- [Options files](#options-files)
  - [Creating and loading](#creating-and-loading)
  - [Modifying options](#modifying-options)
  - [Verbosity](#verbosity)
- [Data](#data)
  - [Supported formats](#supported-formats)
  - [Per-variable configuration](#per-variable-configuration)
- [Autonomy: how much artma asks](#autonomy-how-much-artma-asks)
- [Results and visualization](#results-and-visualization)
- [Custom methods](#custom-methods)
- [Learn more](#learn-more)

# Installation

From CRAN:

```r
install.packages("artma")
```

From GitHub (development version):

```r
remotes::install_github("PetrCala/artma")
```

For local development, clone the repository (see [README-dev.md](README-dev.md)) and load it with `devtools::load_all()`.

# Quick start

The simplest way to use artma is fully interactively:

```r
library(artma)
results <- artma()
```

In an interactive session, artma walks you through creating an options file, locating your data, mapping your columns, and choosing which methods to run. Everything you decide is saved to the options file, so subsequent runs are reproducible.

Once you have an options file, runs can be fully scripted:

```r
# Run specific methods
results <- artma(
  methods = c("funnel_plot", "bma", "fma"),
  options = "my_analysis.yaml"
)

# Run everything
results <- artma(methods = "all", options = "my_analysis.yaml")

# Analyze a data frame you already have in memory
results <- artma(data = my_df, methods = c("effect_summary_stats"))

# Open the results folder when done
results <- artma(methods = "all", options = "my_analysis.yaml", open_results = TRUE)
```

# How a run works

A call to `artma()` orchestrates four steps:

1. **Options loading**: your YAML options file is read and its values are loaded into the R `options()` namespace (prefixed with `artma.`) for the duration of the call.
2. **Data preparation**: the dataset is read, column names are standardized, missing values are handled, and derived columns (effect sizes, standard errors, and similar) are computed. Skipped when you pass `data` directly, apart from preprocessing and validation.
3. **Method execution**: the requested methods run in dependency order. Methods that build on another method's output (for example, `best_practice_estimate` builds on `bma`) automatically receive that upstream result, so shared models are computed once.
4. **Export and return**: each method's tables are exported as CSV and its plots are written to the results directory. The function returns a named list of results, one entry per method.

Runs are fault tolerant. A method that errors is skipped with a warning while the rest continue, and results from successful methods are still exported. Failed method names and their error messages are attached to the returned list as the `failed_methods` attribute. Methods whose required columns are missing from your data, or whose optional packages aren't installed, are skipped with an explanation rather than aborting the run.

Expensive computations are cached on disk between runs, so rerunning an analysis with unchanged inputs is near-instant. A cached result is reused only when the data, the upstream method results, every user-authored `artma.*` option, the data source and its modification time, and the package source code all match the run that produced it; a method whose exported plot files have since been removed is rerun so the files come back. Disable caching with `options(artma.cache.use_cache = FALSE)`, and clear it with `make clear-cache`.

# Available methods

List them at any time with `artma::methods.list()`. The current set:

| Method | What it does |
| --- | --- |
| `effect_summary_stats` | Summary statistics of the main effect, grouped by flagged variables |
| `variable_summary_stats` | Summary statistics for selected variables in the dataset |
| `funnel_plot` | Funnel plot of effect estimates against precision, for spotting publication bias |
| `box_plot` | Box plots of effects grouped by a categorical variable |
| `prima_facie_graphs` | Density and histogram plots of effect distributions by category |
| `t_stat_histogram` | Histogram of t-statistics with significance reference lines |
| `linear_tests` | Linear publication bias diagnostics (fixed, random, and weighted variants) |
| `nonlinear_tests` | Publication bias diagnostics based on non-linear estimators |
| `exogeneity_tests` | Diagnostics that relax the exogeneity assumption (IV regression, p-uniform*) |
| `p_hacking_tests` | A suite of tests for p-hacking and selective reporting |
| `maive` | The MAIVE estimator, correcting for publication bias, p-hacking, and spurious precision |
| `bma` | Bayesian Model Averaging over moderator variables for heterogeneity analysis |
| `fma` | Frequentist Model Averaging, reusing the BMA model to order predictors |
| `best_practice_estimate` | Best-practice estimates computed from the BMA coefficients |

Some methods rely on optional packages (for example, `bma` needs `BMS`). If one is missing, artma tells you and offers to install it in interactive sessions; in scripts the method is skipped, unless it's the only method you asked for, in which case the run stops with a clear error.

# Options files

All settings for an analysis live in a hierarchical YAML options file: the data path, column mappings, method parameters, output preferences, and so on. Options files make analyses reproducible and let you keep separate configurations for different projects side by side.

When loaded, every option is available under the `artma.` prefix:

```r
getOption("artma.verbose") # e.g. 3
```

## Creating and loading

If you call a runtime method without an options file, artma prompts you to create one. You can also do it explicitly:

```r
artma::options.create()
```

Pass the file name to `artma()` to use it for a run:

```r
artma::artma(
  options = "my_analysis.yaml",
  options_dir = "path/to/your/options" # optional, defaults to the standard directory
)
```

Options are loaded only for the duration of the call, so different configurations never leak across sessions or invocations.

Useful helpers:

```r
artma::options.list()              # names of your existing options files
artma::options.open()              # open a file for editing
artma::options.help()              # documentation for every available option
artma::options.validate()          # check a file against the template
artma::options.fix()               # fill in missing options with defaults
artma::options.copy()              # duplicate a configuration
artma::options.delete()            # remove one
artma::options.print_default_dir() # where the files live
```

## Modifying options

```r
artma::options.modify(
  options_file_name = "my_analysis.yaml",
  user_input = list("verbose" = 4)
)
```

## Verbosity

The `verbose` option (1 to 4) controls how much artma prints:

| Level | Output |
| --- | --- |
| 1 | Errors only |
| 2 | Warnings and errors |
| 3 | Info: short progress and high-level messages (default) |
| 4 | Debug: everything, including internals |

# Data

## Supported formats

artma reads CSV, TSV, Excel (`.xlsx`/`.xls`), JSON, Stata (`.dta`), and RDS files. You can also skip file reading entirely by passing a data frame via `artma(data = ...)`; it still goes through preprocessing and validation.

Preview any dataset (from a path, a data frame, or your options file) with:

```r
artma::data.preview()
```

## Per-variable configuration

Each column in your dataset has a configuration entry that controls how it participates in the analyses: its type, which methods use it, grouping flags, and so on. Sensible defaults are auto-detected from the data; only your deviations from the defaults are persisted in the options file.

```r
artma::config.get()                          # the fully resolved config
artma::config.get(var_name = "study_size")   # one variable's entry
artma::config.set("study_size", bma = TRUE)  # override specific fields
artma::config.overrides()                    # see only what you've overridden
artma::config.reset("study_size")            # back to auto-detected defaults
artma::config.fix()                          # regenerate the config from the data
```

# Autonomy: how much artma asks

The autonomy level controls how many interactive prompts you get during a run:

| Level | Behavior |
| --- | --- |
| `ask_more` | Prompt for most decisions, including non-critical ones |
| `balanced` | Prompt for important decisions only |
| `autonomous` (default) | Minimal prompts; rely on defaults and auto-detection |

```r
artma::autonomy.get()
artma::autonomy.set("balanced")
```

Non-interactive sessions (scripts, CI) never prompt, regardless of the level.

# Results and visualization

Tables are exported as CSV and plots as graphics files into the results directory:

```r
artma::results.dir()  # print and return the path
artma::results.open() # open it in your file browser
```

Plot appearance is configurable per session:

```r
artma::viz.themes()             # available themes
artma::viz.get()                # current settings
artma::viz.set(theme = "purple")
```

# Custom methods

Runtime methods live in `inst/artma/methods/`, one file per method, and are discovered automatically. Each method is a plain function registered with `register_runtime_method()`, which declares its dependencies, required data columns, and optional packages, and wires in caching:

```r
box::use(
  artma / modules / runtime_methods[new_method_result, register_runtime_method]
)

my_method <- function(df, ...) {
  new_method_result(
    tables = list(summary = my_summary_df),
    plots = list(),
    meta = list()
  )
}

run <- register_runtime_method(
  my_method,
  stage = "my_method",
  required_columns = c("effect", "se")
)

box::export(my_method, run)
```

Method parameters beyond the data frame come from the options system, so custom methods are configured the same way as built-in ones. See [README-dev.md](README-dev.md) for the full developer setup.

# Learn more

- The package vignettes cover the workflow in depth: [Getting Started](vignettes/getting-started.Rmd) and [Options Files](vignettes/options-files.Rmd), also available via `browseVignettes("artma")` or [on CRAN](https://cran.r-project.org/package=artma).
- Developers should start with [README-dev.md](README-dev.md).
- Bugs and feature requests: [GitHub issues](https://github.com/PetrCala/artma/issues).
