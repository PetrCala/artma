# Delete user options

Provide a name of a user options file to delete, and delete that file.
`options_remove()` is an alias.

## Usage

``` r
options_delete(
  options_file_name = NULL,
  options_dir = NULL,
  skip_confirmation = FALSE
)

options_remove(
  options_file_name = NULL,
  options_dir = NULL,
  skip_confirmation = FALSE
)
```

## Arguments

- options_file_name:

  *\[character, optional\]* Name of the options file to delete. If not
  provided, the user will be prompted. Defaults to `NULL`.

- options_dir:

  *\[character, optional\]* Full path to the folder that contains user
  options files. If not provided, the default folder is chosen. Defaults
  to `NULL`.

- skip_confirmation:

  *\[boolean, optional\]* If passed as TRUE, the user will not be
  prompted for deletion confirmation. Defaults to FALSE.

## Value

`NULL`
