# Options Help

Prints information for each requested option (or all options if
`options` is `NULL`).

## Usage

``` r
options_help(options = NULL, template_path = NULL)
```

## Arguments

- options:

  *\[character, optional\]* A single option name (dot-separated), a
  group name, or a character vector thereof. If `NULL`, prints **all**
  options from the template as a tree.

- template_path:

  *\[character, optional\]* Path to the template YAML file. Defaults to
  `PATHS$FILE_OPTIONS_TEMPLATE`.

## Value

Invisibly returns `NULL`, printing the requested information to the
console.

## Details

Called without arguments, the whole option tree is printed, grouped by
top-level section, one line per option carrying its name, type and
default. Named options are printed in full, with their help text. A name
that matches a group rather than a single option (e.g. `"methods.bma"`,
or `"methods"`) expands to every option underneath it.
