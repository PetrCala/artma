# Get Visualization Settings

Get the current visualization settings. Returns all settings as a list,
or a single setting by name.

## Usage

``` r
viz_get(option = NULL)
```

## Arguments

- option:

  *\[character, optional\]* Name of a specific option to retrieve. One
  of: `"theme"`, `"export_graphics"`, `"export_path"`, `"graph_scale"`.
  If NULL (default), returns all options as a named list.

## Value

A named list of all visualization settings, or a single setting value.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all visualization settings
viz_get()

# Get just the current theme
viz_get("theme")

# Get export path
viz_get("export_path")
} # }
```
