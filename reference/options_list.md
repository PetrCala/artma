# List available user options

Retrieves the list of the existing options files and returns their names
as a character vector. By default, this retrieves the names of the files
including the yaml suffix, but can be modified to retrieve options
verbose names instead.

## Usage

``` r
options_list(
  options_dir = NULL,
  should_return_verbose_names = FALSE,
  details = FALSE,
  template_path = NULL
)
```

## Arguments

- options_dir:

  *\[character, optional\]* Full path to the folder that contains user
  options files. If not provided, the default folder is chosen. Defaults
  to `NULL`.

- should_return_verbose_names:

  *\[logical, optional\]* If set to TRUE, the custom names of each of
  the options files are read and returned instead of file names.
  Defaults to FALSE.

- details:

  *\[logical, optional\]* If set to TRUE, a `data.frame` describing each
  options file is returned instead of a character vector: the file name,
  its resolved `data.source_path`, when the file was last modified, when
  it was last run, and how many of its options deviate from the template
  defaults. The last run time is read from the file's output directory,
  and is `NA` when the file has never produced any results. Defaults to
  FALSE.

- template_path:

  *\[character, optional\]* Full path to the options template file, used
  to resolve the defaults when `details = TRUE`. Defaults to `NULL`.

## Value

*\[vector, character \| data.frame\]* A character vector with the names
of the options available, or a `data.frame` when `details = TRUE`.
