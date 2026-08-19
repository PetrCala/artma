# Set Autonomy Level

Set the autonomy level for the current session. This setting controls
how much user interaction is required during analysis.

## Usage

``` r
autonomy_set(level)
```

## Arguments

- level:

  *\[character\]* The autonomy level to set.

  - "ask_more": Prompt for most decisions, including non-critical ones.

  - "balanced": Prompt for important decisions only.

  - "autonomous" (default): Minimal prompts; use defaults and
    auto-detection for most decisions.

  Legacy numeric levels (1-5) are still accepted and translated, with a
  warning (1-2 -\> "ask_more", 3 -\> "balanced", 4-5 -\> "autonomous").

## Value

`NULL` (invisible)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set to fully autonomous mode
autonomy_set("autonomous")

# Set to balanced mode
autonomy_set("balanced")
} # }
```
