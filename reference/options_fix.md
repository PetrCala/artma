# Fix user options file

Fix a user options file by setting the default values for missing
options.

## Usage

``` r
options_fix(
  options_file_name = NULL,
  options_dir = NULL,
  template_path = NULL,
  force_default_overwrites = TRUE
)
```

## Arguments

- options_file_name:

  *\[character, optional\]* Name of the options file to fix, including
  the .yaml suffix. Defaults to `NULL`.

- options_dir:

  *\[character, optional\]* Path to the folder in which to look for user
  options files. Defaults to `NULL`.

- template_path:

  *\[character, optional\]* Path to the options template file. Defaults
  to `NULL`.

- force_default_overwrites:

  *\[logical, optional\]* If set to TRUE, the function will overwrite
  the existing options file with the default values. Defaults to TRUE.

## Value

`NULL` Fixes the user options file.

## Details

The function will attempt to load the user options file and validate it.
If any errors are found, the function will attempt to fix them by
setting the default values for the missing options.
