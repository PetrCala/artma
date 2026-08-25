# Summarize a finished run

Third step of the
[`artma()`](https://petrcala.github.io/artma/reference/artma.md)
pipeline: print the run summary, optionally open the results directory,
and emit the closing messages.

## Usage

``` r
summarize_run(results, context, run_files = character(), open_results = FALSE)
```

## Arguments

- results:

  *\[list\]* The results returned by
  [`execute_run()`](https://petrcala.github.io/artma/reference/execute_run.md).

- context:

  *\[list\]* The run context returned by
  [`prepare_run_context()`](https://petrcala.github.io/artma/reference/prepare_run_context.md).

- run_files:

  *\[character, optional\]* The files the run wrote.

- open_results:

  *\[logical, optional\]* Whether to open the results directory. Only
  honoured in interactive sessions that saved results.

## Value

*\[list\]* The `results`, invisibly.
