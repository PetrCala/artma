# Copy user options

Provide a name of a user options file to copy from, and a name of a file
to copy to, and copy from the 'from' file to the 'to' file.

## Usage

``` r
options_copy(
  options_file_name_from = NULL,
  options_file_name_to = NULL,
  options_dir = NULL,
  should_overwrite = NULL
)
```

## Arguments

- options_file_name_from:

  *\[character, optional\]* Name of the options file to copy from. If
  not provided, the user will be prompted. Defaults to `NULL`.

- options_file_name_to:

  *\[character, optional\]* Name of the options file to copy to. If not
  provided, the user will be prompted. Defaults to `NULL`.

- options_dir:

  *\[character, optional\]* Full path to the folder that contains user
  options files. If not provided, the default folder is chosen. Defaults
  to `NULL`.

- should_overwrite:

  *\[logical, optional\]* Whether to overwrite an existing file without
  asking. If `TRUE`, the file will be overwritten without prompting. If
  `FALSE`, the function will abort if the file already exists. If `NULL`
  (default), the user will be prompted.

## Value

`NULL`
