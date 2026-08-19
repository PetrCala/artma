# Install the artma CLI launcher

Copy the launcher shipped inside the installed package to a directory on
your `PATH`, so `artma <command>` works from any shell. The launcher
relies solely on the installed package; it resolves nothing from its own
location.

The no-install alternative is to call the CLI through `Rscript`
directly:
`Rscript -e 'artma::cli_run()' run --options my_analysis.yaml`.

In interactive sessions the copy is confirmed via a menu. In
non-interactive sessions it requires `force = TRUE` and otherwise
aborts. It never writes inside
[`R.home()`](https://rdrr.io/r/base/Rhome.html) and never elevates
privileges.

## Usage

``` r
cli_install(dir = "~/.local/bin", force = FALSE)
```

## Arguments

- dir:

  *\[character, optional\]* Directory to install the launcher into.
  Defaults to `"~/.local/bin"`.

- force:

  *\[logical, optional\]* Required to install in a non-interactive
  session. Ignored (the menu governs) when interactive. Defaults to
  `FALSE`.

## Value

*\[character\]* Path to the installed launcher (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
artma::cli_install()
artma::cli_install(dir = "~/bin", force = TRUE)
} # }
```
