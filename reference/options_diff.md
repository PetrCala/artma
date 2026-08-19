# Diff two user options files

Compares two user options files, printing the options whose values
differ between them, followed by each file's deviations from the
template defaults.

## Usage

``` r
options_diff(
  options_file_name_a = NULL,
  options_file_name_b = NULL,
  options_dir = NULL,
  template_path = NULL
)
```

## Arguments

- options_file_name_a:

  *\[character, optional\]* Name of the first options file to compare.
  If not provided, the user will be prompted. Defaults to `NULL`.

- options_file_name_b:

  *\[character, optional\]* Name of the second options file to compare.
  If not provided, the user will be prompted. Defaults to `NULL`.

- options_dir:

  *\[character, optional\]* Full path to the folder that contains user
  options files. If not provided, the default folder is chosen. Defaults
  to `NULL`.

- template_path:

  *\[character, optional\]* Full path to the options template file.
  Defaults to `NULL`.

## Value

*\[list\]* Invisibly, a list with the compared `files`, a `differences`
data frame, and a `deviations` list holding one data frame of
template-default deviations per file.

## Details

List-typed options (such as `data.columns`) are compared entry by entry,
so the diff names the individual column mappings that differ rather than
reporting the whole store as changed.
