# Check if Fully Autonomous

Check if the package is running in fully autonomous mode, i.e. the
autonomy level is set to "autonomous", or the session is non-interactive
(where prompts never happen regardless of the configured level).

## Usage

``` r
autonomy_is_full()
```

## Value

*\[logical\]* TRUE if fully autonomous, FALSE otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
if (autonomy_is_full()) {
  message("Running in fully autonomous mode")
}
} # }
```
