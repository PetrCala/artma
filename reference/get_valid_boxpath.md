# Get valid box path

Construct a box path that will allow box imports for the current
package. This is done by adding the package path to the box path option
if it is not already there.

## Usage

``` r
get_valid_boxpath(libname, pkgname)
```

## Arguments

- libname:

  The path to the library.

- pkgname:

  The name of the package.
