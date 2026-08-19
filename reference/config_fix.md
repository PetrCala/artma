# Fix the data config

Regenerate the data config from the dataframe, clearing all overrides.

## Usage

``` r
config_fix(options_file_name = NULL, options_dir = NULL)
```

## Arguments

- options_file_name:

  *\[character, optional\]* The name of the options file. If `NULL`
  (default), the user will be prompted interactively.

- options_dir:

  *\[character, optional\]* The directory containing options files. If
  `NULL` (default), the default directory is used.

## Value

*\[list\]* The fixed data config.
