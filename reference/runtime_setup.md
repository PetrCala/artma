# Runtime Setup

A function user as a wrapper for runtime functions invocation to ensure
crucial fucntionality, such as imports, etc., all work as expected.

## Usage

``` r
runtime_setup(FUN, options_file_name = NULL, options_dir = NULL)
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
