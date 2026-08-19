# Build the JSON run manifest

Summarise an
[`artma()`](https://petrcala.github.io/artma/reference/artma.md) result
into the manifest emitted in `--json` mode: methods that ran, methods
that were skipped or failed (with reasons), the resolved output
directory, the files exported there, the effective seed and the package
version.

The run's own `run.json` is the source of truth, so the CLI reports the
same run identity as the R API rather than whatever has accumulated in
the output directory. A run that wrote no manifest
(`output.save_results` off, or `jsonlite` unavailable) falls back to
what the results object carries, with an empty file list.

## Usage

``` r
cli_build_run_manifest(results)
```

## Arguments

- results:

  *\[list\]* The value returned by
  [`artma()`](https://petrcala.github.io/artma/reference/artma.md).

## Value

*\[list\]* The manifest, ready for
[`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
