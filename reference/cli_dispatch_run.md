# Dispatch the `run` subcommand

Apply the flag-derived options overlay around an
[`artma()`](https://petrcala.github.io/artma/reference/artma.md) call.
In `--json` mode stray stdout from the run is redirected to stderr so
the manifest is the only thing on stdout.

## Usage

``` r
cli_dispatch_run(flags)
```

## Arguments

- flags:

  *\[list\]* Parsed `run` flags.

## Value

`NULL` (invisible)
