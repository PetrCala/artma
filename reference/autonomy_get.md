# Get Autonomy Level

Get the current autonomy level. Autonomy controls how much user
interaction is required during analysis.
[`interactive()`](https://rdrr.io/r/base/interactive.html) is the hard
gate: non-interactive sessions never prompt, regardless of this setting.

## Usage

``` r
autonomy_get()
```

## Value

*\[character or NULL\]* The current autonomy level ("ask_more",
"balanced", or "autonomous"), or NULL if not set.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get current autonomy level
level <- autonomy_get()
print(level)
} # }
```
