# Options Open

Open an options file for editing. Must be run interactively. The editor
is resolved from: (1) cli.editor option, (2) VISUAL/EDITOR env vars, or
(3) system default file handler.

## Usage

``` r
options_open(options_file_name = NULL, options_dir = NULL)
```

## Arguments

- options_file_name:

  *\[character, optional\]* Name of the user options file to modify,
  including the suffix.

- options_dir:

  *\[character, optional\]* Full path to the folder that contains user
  options files. If not provided, the default folder is chosen. Defaults
  to `NULL`.

## Value

`NULL` Opens the file for editing
