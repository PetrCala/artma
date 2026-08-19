# Open Results Directory

Opens the output directory in the system file browser (Finder on macOS,
Explorer on Windows, or the default file manager on Linux). When called
without arguments, tries to open the most recently exported results
directory without prompting for an options file.

## Usage

``` r
results_open(options = NULL, options_dir = NULL, use_last = TRUE)
```

## Arguments

- options:

  *\[character, optional\]* Name of the options file (with or without
  `.yaml` extension). If `NULL`, the function first checks for a recent
  export marker. If no marker is found and running interactively, you
  will be prompted to select an options file.

- options_dir:

  *\[character, optional\]* Directory containing the options file. If
  `NULL`, uses the default options directory.

- use_last:

  *\[logical\]* If `TRUE` (default) and no `options`/`options_dir` are
  provided, automatically open the most recently exported results
  directory. Set to `FALSE` to always resolve via the options file.

## Value

*\[character\]* The resolved output directory path (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
# Open the most recent results (no prompt if a recent export exists)
results_open()

# Force options-based resolution (will prompt if needed)
results_open(use_last = FALSE)

# Open results for a specific options file
results_open(options = "my_analysis.yaml")
} # }
```
