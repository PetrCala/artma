# Set Visualization Settings

Set visualization options for the current session. Only provided
arguments are changed; others remain unchanged.

## Usage

``` r
viz_set(
  theme = NULL,
  export_graphics = NULL,
  export_path = NULL,
  graph_scale = NULL
)
```

## Arguments

- theme:

  *\[character, optional\]* Color theme. Use
  [`viz_themes()`](https://petrcala.github.io/artma/reference/viz_themes.md)
  to see available themes.

- export_graphics:

  *\[logical, optional\]* If TRUE, export plots to files.

- export_path:

  *\[character, optional\]* Directory path for exported plots.

- graph_scale:

  *\[numeric, optional\]* Scaling factor for exported graphics. Values
  \> 1 increase resolution.

## Value

Previous settings (invisibly), enabling easy restoration.

## Examples

``` r
if (FALSE) { # \dontrun{
# Change theme
viz_set(theme = "purple")

# Enable export with custom path
viz_set(export_graphics = TRUE, export_path = "./output/plots")

# Save and restore settings
prev <- viz_set(theme = "red")
# ... do work ...
do.call(viz_set, prev)
} # }
```
