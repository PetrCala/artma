# Create user options

Create a new user options file from an options template.

## Usage

``` r
options_create(
  options_file_name = NULL,
  options_dir = NULL,
  template_path = NULL,
  user_input = list(),
  should_validate = TRUE,
  should_overwrite = FALSE,
  action_name = "creating"
)
```

## Arguments

- options_file_name:

  *\[character\]* Name of the new user options file, including the
  suffix.

- options_dir:

  *\[character, optional\]* Full path to the folder that contains user
  options files. If not provided, the default folder is chosen. Defaults
  to `NULL`.

- template_path:

  *\[character, optional\]* Full path to the options template file.

- user_input:

  *\[list, optional\]* A named list of user-supplied values for these
  options, using either flat dotted-path names (e.g.
  `list("data.source_path" = "...")`) or nested lists that mirror the
  YAML structure (e.g. `list(data = list(source_path = "..."))`). When
  overwriting an existing file, a partial edit to a list-type option
  (e.g. one entry of `data.columns`) is merged into the existing value
  instead of replacing it. If `NULL` or missing entries exist, the
  function will prompt the user via
  [`readline()`](https://rdrr.io/r/base/readline.html) (for required
  entries) or use defaults (for optional ones).

- should_validate:

  *\[logical, optional\]* If TRUE, validate the new options file against
  the template. Defaults to TRUE.

- should_overwrite:

  *\[logical, optional\]* If TRUE, overwrite the file if it already
  exists. Defaults to FALSE, in which case the user is prompted to
  confirm the overwrite.

- action_name:

  *\[character, optional\]* A name for the action being performed. This
  is used for logging purposes. Defaults to "create". `character` Name
  of the newly created user options file as a character.

## Value

`NULL`
