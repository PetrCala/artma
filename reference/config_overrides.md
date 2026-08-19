# View sparse config overrides

Returns only the sparse overrides that are actually persisted in the
options file – i.e., only non-default field values.

## Usage

``` r
config_overrides(options_file_name = NULL, options_dir = NULL)
```

## Arguments

- options_file_name:

  *\[character, optional\]* The name of the options file. If `NULL`
  (default), the user will be prompted interactively.

- options_dir:

  *\[character, optional\]* The directory containing options files. If
  `NULL` (default), the default directory is used.

## Value

*\[list\]* The sparse overrides (only non-default values).
