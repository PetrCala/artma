# Dispatch a parsed CLI invocation

Translate a `dispatch` parse result into a call on the public API. Kept
separate from
[`cli_run()`](https://petrcala.github.io/artma/reference/cli_run.md) so
the top-level error boundary and exit-code contract stay small and
testable.

## Usage

``` r
cli_dispatch(parsed)
```

## Arguments

- parsed:

  *\[list\]* A parse result with `action == "dispatch"`.

## Value

`NULL` (invisible)
