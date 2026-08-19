# Set per-variable config overrides

Sets specific config fields for a variable. Only non-default values are
persisted to the options file.

## Usage

``` r
config_set(var_name, ..., options_file_name = NULL, options_dir = NULL)
```

## Arguments

- var_name:

  *\[character\]* The variable name to configure.

- ...:

  Named arguments for config fields to set (e.g.,
  `bma = TRUE, bma_to_log = TRUE`). Column mappings are set the same
  way: `source_name = "study_name"` maps the variable to that column in
  the data file, and `drop_conflicting_raw = TRUE` keeps such a mapping
  while dropping a different raw column that occupies the standard name.

- options_file_name:

  *\[character, optional\]* The name of the options file. If `NULL`
  (default), the user will be prompted interactively.

- options_dir:

  *\[character, optional\]* The directory containing options files. If
  `NULL` (default), the default directory is used.

## Value

*\[list\]* The updated fully-resolved data config (invisibly).
