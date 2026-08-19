# Invoke methods

Pass a vector of runtime methods to invoke, together with a data frame
to invoke these methods on, and invoke them.

Methods run in an order derived from their declared `depends_on`
metadata (topologically sorted, discovery order preserved among
independent methods); each upstream result is passed to its dependents
as a `<dependency>_result` argument. Before a method runs, its declared
`required_columns` are checked against the data frame and its `suggests`
packages against the installed set; a method that fails either check is
skipped with an explanation rather than aborting the run.

Method failures are isolated: a method that throws an error is skipped
with a warning and the remaining methods still run. The run only aborts
for invalid input (for example, unknown method names), never because a
method failed or was skipped. Skipped and failed methods are attached as
attributes for the closing run summary to report, and a final warning is
emitted if every method failed.

## Usage

``` r
invoke_runtime_methods(methods, df, modules_dir = NULL, ...)
```

## Arguments

- methods:

  *\[character\]* A character vector of the methods to invoke.

- df:

  *\[data.frame\]* The data frame to invoke the methods on.

- modules_dir:

  *\[character, optional\]* Directory to discover runtime method modules
  in. Defaults to `NULL`, in which case the standard package methods
  directory is used. Used mainly for dependency injection in tests.

- ...:

  *\[any\]* Additional arguments to pass to the methods.

## Value

*\[list\]* Results of the invocations, indexed by method names. Failed
methods are omitted; their names and error messages are attached as the
`failed_methods` attribute. Methods skipped for missing columns or
packages are attached as the `skipped_methods` attribute (both named
character vectors).

Internal example: df \<- data.frame(...)
invoke_runtime_methods(c("funnel_plot", "bma", "fma"), df)
