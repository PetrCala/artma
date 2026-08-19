# Get the resolved data config

Returns the fully-resolved data config (base defaults merged with sparse
overrides). If `var_name` is provided, returns only that variable's
config entry.

## Usage

``` r
config_get(var_name = NULL, options_file_name = NULL, options_dir = NULL)
```

## Arguments

- var_name:

  *\[character, optional\]* A specific variable name to retrieve. If
  `NULL` (default), returns the entire config.

- options_file_name:

  *\[character, optional\]* The name of the options file. If `NULL`
  (default), the user will be prompted interactively.

- options_dir:

  *\[character, optional\]* The directory containing options files. If
  `NULL` (default), the default directory is used.

## Value

*\[list\]* The fully-resolved data config (or a single entry).
