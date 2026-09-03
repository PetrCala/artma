# Prepare a run

First step of the
[`artma()`](https://petrcala.github.io/artma/reference/artma.md)
pipeline: resolve the output directory, open the run's output-file
capture, and produce the prepared data frame. Either loads the data
through `prepare_data()` or, when a data frame is supplied, mirrors that
pipeline's preprocess and compute phases on it.

The returned context is what the run and summarize steps consume. The
capture frame it opens stays open on success: the caller owns it and
must close it (the run step does so when it writes the manifest, and
[`artma()`](https://petrcala.github.io/artma/reference/artma.md)
registers an [`on.exit()`](https://rdrr.io/r/base/on.exit.html)
fallback). A failure inside this function closes the frame before
propagating, so an aborted preparation leaves no dangling frame.

Callable repeatedly within one
[`runtime_setup()`](https://petrcala.github.io/artma/reference/runtime_setup.md)
extent, which is what the session hub needs to re-prepare data from its
menu loop.

## Usage

``` r
prepare_run_context(data = NULL, methods = NULL)
```

## Arguments

- data:

  *\[data.frame, optional\]* Data frame to analyze. When `NULL`, the
  data is loaded from the options file.

- methods:

  *\[character, optional\]* The methods the run will invoke, used to
  decide which columns the data preparation must resolve.

## Value

*\[list\]* The run context: `df` (the prepared data frame),
`unwinsorized_df` (a function returning the same frame prepared without
winsorization, built on first call and then kept; handed to methods that
register `winsorize = FALSE`), `output_dir` (the base output directory;
`NULL` when results are not saved, and replaced by the run's own
directory in
[`execute_run()`](https://petrcala.github.io/artma/reference/execute_run.md)),
`save_results` and `capture` (the open output-file capture frame
identifier).
