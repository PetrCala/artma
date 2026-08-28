# Restore the last used options file for a hub session

Entry step of an interactive
[`artma()`](https://petrcala.github.io/artma/reference/artma.md) call
that named no options file: read the last-used marker and, when it names
an options file that still exists, bind the session to it through the
same handler the hub's options-file menu uses. Any failure (no marker,
the file is gone, the load errors) reports one info line and returns
`NULL`, leaving the session to enter unbound exactly as it would without
the marker. A marker naming a file that no longer exists is cleared; a
load error keeps it, since the file may load fine next time.

## Usage

``` r
restore_last_options_file(bind_options, options_dir = NULL)
```

## Arguments

- bind_options:

  *\[function\]* Called with the remembered file name; must load that
  file's options for the session.

- options_dir:

  *\[character, optional\]* Directory containing the options files. If
  `NULL`, uses the default options directory.

## Value

*\[character, optional\]* The restored options file name, or `NULL` when
the session should enter unbound.
