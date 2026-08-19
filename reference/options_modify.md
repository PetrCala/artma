# Modify User Options

Modify an existing user options file with new values.

## Usage

``` r
options_modify(
  options_file_name = NULL,
  options_dir = NULL,
  template_path = NULL,
  user_input = list(),
  should_validate = TRUE
)
```

## Arguments

- options_file_name:

  *\[character, optional\]* Name of the user options file to modify,
  including the suffix.

- options_dir:

  *\[character, optional\]* Full path to the folder that contains user
  options files. If not provided, the default folder is chosen. Defaults
  to `NULL`.

- template_path:

  *\[character, optional\]* Full path to the options template file.
  Defaults to `NULL`.

- user_input:

  *\[list, optional\]* A named list of user-supplied values for these
  options, using either flat dotted-path names (e.g.
  `list("data.source_path" = "...")`, matching what
  `options_load(load_with_prefix = FALSE)` returns) or nested lists that
  mirror the YAML structure (e.g.
  `list(data = list(source_path = "..."))`); both are flattened against
  the template before merging, so a partial edit to a list-type option
  (e.g. one entry of `data.columns`) is merged into the existing value
  instead of replacing it. If `NULL` or missing entries exist, the
  function will prompt the user via
  [`readline()`](https://rdrr.io/r/base/readline.html) (for required
  entries) or use defaults (for optional ones).

- should_validate:

  *\[logical, optional\]* If TRUE, validate the modified options file
  against the template. Defaults to TRUE.

## Value

`NULL`
