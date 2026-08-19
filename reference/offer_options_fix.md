# Offer to fix an outdated options file

Validate the given options file and, if it has problems, offer the
interactive user a chance to repair it via
[`options_fix()`](https://petrcala.github.io/artma/reference/options_fix.md)
before it is loaded. Best effort: any failure here is swallowed so that
loading (which is pure and applies defaults) can still proceed.

## Usage

``` r
offer_options_fix(options_file_name, options_dir = NULL)
```

## Arguments

- options_file_name:

  *\[character\]* Name of the options file, including the suffix.

- options_dir:

  *\[character, optional\]* Path to the directory that contains user
  options.
