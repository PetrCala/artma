# Validate a user options file against an options template.

This function reads a YAML template and an options file, flattens both
structures, and then checks that:

- Every option defined in the template is present in the options file.

- The value for each option is of the correct type.

- (Optionally) It warns about extra options in the file that are not
  defined in the template.

## Usage

``` r
options_validate(
  options_file_name = NULL,
  options_dir = NULL,
  should_flag_redundant = FALSE,
  template_path = NULL,
  failure_action = "abort_verbose"
)
```

## Arguments

- options_file_name:

  *\[character\]* Name of the user options file to validate, including
  the suffix.

- options_dir:

  *\[character, optional\]* Full path to the folder that contains user
  options files. If not provided, the default folder is chosen. Defaults
  to `NULL`.

- should_flag_redundant:

  *\[logical, optional\]* If TRUE, warn the user about any extraneous
  options (i.e., options not defined in the options template, such as
  custom options that the user might have added). Defaults to FALSE.

- template_path:

  *\[character, optional\]* Full path to the options template file.
  Defaults to `NULL`.

- failure_action:

  *\[character\]* Action to take if validation fails. Can be one of:
  'abort_verbose', 'abort_quiet', 'return_errors_verbose',
  'return_errors_quiet'. Defaults to 'abort_verbose'. `list` Invisibly
  returns a list of error messages (empty if no errors).

## Value

*\[list\]* The validation errors

## Details

For each problem found (missing option or type mismatch), an error
message is printed.
