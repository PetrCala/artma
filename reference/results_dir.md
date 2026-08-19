# Get Results Directory Path

Returns the resolved path to the output directory where analysis results
(tables, graphics) are saved. The path is printed and returned
invisibly. When called without arguments, tries to use the most recently
exported directory without prompting for an options file.

## Usage

``` r
results_dir(options = NULL, options_dir = NULL)
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

## Value

*\[character\]* The resolved output directory path (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the most recent results directory
results_dir()

# Get results dir for a specific options file
results_dir(options = "my_analysis.yaml")
} # }
```
