# Emit lines to stderr

Route CLI usage and error text to stderr, keeping stdout clean for
machine-readable output (the JSON manifest in `--json` mode).

## Usage

``` r
cli_emit_to_stderr(lines)
```

## Arguments

- lines:

  *\[character\]* Lines to print.

## Value

`NULL` (invisible)
