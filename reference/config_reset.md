# Reset variable config to defaults

Removes all overrides for a specific variable (or all variables),
resetting them to auto-detected defaults.

## Usage

``` r
config_reset(var_name = NULL, options_file_name = NULL, options_dir = NULL)
```

## Arguments

- var_name:

  *\[character, optional\]* The variable name to reset. If `NULL`
  (default), resets all overrides.

- options_file_name:

  *\[character, optional\]* The name of the options file. If `NULL`
  (default), the user will be prompted interactively.

- options_dir:

  *\[character, optional\]* The directory containing options files. If
  `NULL` (default), the default directory is used.

## Value

*\[list\]* The updated fully-resolved data config (invisibly).
