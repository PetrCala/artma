# Load user options

Load user options by their name and return them as a list.

## Usage

``` r
options_load(
  options_file_name = NULL,
  options_dir = NULL,
  create_options_if_null = TRUE,
  load_with_prefix = TRUE,
  template_path = NULL,
  should_validate = TRUE,
  should_add_temp_options = FALSE,
  should_return = TRUE
)
```

## Arguments

- options_file_name:

  *\[character, optional\]* Name of the options to load. The `.yaml`
  suffix is appended automatically if missing. Defaults to `NULL`.

- options_dir:

  *\[character, optional\]* Path to the folder in which to look for user
  options files. Defaults to `NULL`.

- create_options_if_null:

  *\[logical, optional\]* If set to TRUE and the options file name is
  set to NULL, the function will prompt the user to create a new options
  file. Defaults to TRUE.

- load_with_prefix:

  *\[logical, optional\]* Whether the options should be loaded with the
  package prefix. Defaults to TRUE.

- template_path:

  *\[character, optional\]* Path to the template YAML file. Defaults to
  `NULL`.

- should_validate:

  *\[logical, optional\]* Whether the options should be validated after
  loading. Defaults to TRUE.

- should_add_temp_options:

  *\[logical, optional\]* Whether the options should be added to the
  temporary options. Defaults to FALSE.

- should_return:

  *\[logical, optional\]* Whether the function should return the list of
  options. Defaults to FALSE.

## Value

*\[list\|NULL\]* The loaded options as a list or `NULL`.

## Details

In case the options name is not passed, the function will attempt to
load the current options configuration. If none is found, it will then
attempt to load the default options. If that fails too, an error is
raised.
