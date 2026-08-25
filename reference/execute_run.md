# Run the methods of a prepared run

Second step of the
[`artma()`](https://petrcala.github.io/artma/reference/artma.md)
pipeline: invoke the runtime methods on the prepared data frame and,
when the run saves results, export the tables, optionally render the
HTML report, and write the run manifest.

The output-file capture opened by
[`prepare_run_context()`](https://petrcala.github.io/artma/reference/prepare_run_context.md)
is closed here, after the export and the report, so the manifest lists
the exported tables and the report alongside the graphics the methods
wrote.

## Usage

``` r
execute_run(context, methods = NULL, ...)
```

## Arguments

- context:

  *\[list\]* The run context returned by
  [`prepare_run_context()`](https://petrcala.github.io/artma/reference/prepare_run_context.md).

- methods:

  *\[character, optional\]* A character vector of the methods to invoke.
  `NULL` prompts for a selection.

- ...:

  *\[any\]* Additional arguments passed to the runtime methods.

## Value

*\[list\]* A list with `results` (the invocation results, carrying the
`run_info`, `failed_methods` and `skipped_methods` attributes) and
`run_files` (the files this run wrote; empty when results are not
saved).
