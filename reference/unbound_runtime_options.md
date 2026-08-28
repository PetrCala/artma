# Session options for a session with no options file

The template defaults, prefixed for the
[`options()`](https://rdrr.io/r/base/options.html) namespace, plus the
temporary options that name the file behind the session: the directory,
and an explicitly empty file name. Every
[`getOption()`](https://rdrr.io/r/base/options.html) in the package
therefore reads the same value it would read under a freshly created
options file, and the empty file name is what marks the session as
unbound.

## Usage

``` r
unbound_runtime_options(options_dir = NULL, template_path = NULL)
```

## Arguments

- options_dir:

  *\[character, optional\]* Path to the directory that contains user
  options.

- template_path:

  *\[character, optional\]* Path to the options template.

## Value

*\[list\]* Options to apply for the session.
