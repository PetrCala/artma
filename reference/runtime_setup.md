# Runtime Setup

A function user as a wrapper for runtime functions invocation to ensure
crucial fucntionality, such as imports, etc., all work as expected.

## Usage

``` r
runtime_setup(
  FUN,
  options_file_name = NULL,
  options_dir = NULL,
  allow_unbound = FALSE
)
```

## Arguments

- FUN:

  [function](https://rdrr.io/r/base/function.html) The function to be
  called after the setup.

- options_file_name:

  *\[character\]* Name of the options file to use, including the suffix.

- options_dir:

  *\[character, optional\]* Path to the directory that contains user
  options. Defaults to the directory specified in PATHS.

- allow_unbound:

  *\[logical, optional\]* Whether an interactive call without an options
  file may proceed on the template defaults instead of prompting for a
  file. The session hub sets this: it offers the choice of options file
  as a menu item rather than as a gate in front of the menu. Defaults to
  FALSE, which keeps the prompt.
