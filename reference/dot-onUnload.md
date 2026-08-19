# .onUnload hook for package detachment

Called when the package is detached.

## Usage

``` r
.onUnload(libpath)
```

## Value

`NULL` Cleans up the package on unload

## Note

The box imports no longer work after the package is detached.
