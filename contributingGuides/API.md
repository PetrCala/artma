# Public API naming and deprecation

## Naming rule

Exported functions use snake_case with the noun group first: `options_create()`,
`config_set()`, `autonomy_get()`, `viz_themes()`. The group prefix keeps
related functions adjacent in autocomplete and in the pkgdown reference index.

Never mint a new dotted export (`group.action`). Dotted names are
syntactically ambiguous with S3 methods: the package registers real S3
methods (`print.artma_box_plot()`), and a dotted non-method such as
`options.list()` would collide with any future generic named `options` applied
to an object of class `list`. R tooling and readers assume the dot separates
generic from class.

Genuine S3 methods (`print.<class>`, `format.<class>`) keep their required
dotted form and are registered via roxygen's `@export` on the method, which
emits `S3method()` directives.

## Deprecated dotted aliases

The pre-0.4.0 API used dotted names (`options.create()`, `config.set()`).
Each was renamed to its snake_case twin in 0.4.0 and left behind as a thin
alias:

```r
#' @rdname artma-deprecated
#' @export
options.create <- function(...) {
  lifecycle::deprecate_warn("0.4.0", "options.create()", "options_create()")
  options_create(...)
}
```

Rules for these shims:

- Call `lifecycle::deprecate_warn()` first, fully qualified (no import), then
  forward all arguments with `...`.
- Document with `#' @rdname artma-deprecated` only; the shared topic lives in
  `R/deprecated.R`. No separate help pages, no examples.
- Tests exercise the snake_case names. One test per group asserts the alias
  warns with class `lifecycle_warning_deprecated` and returns the same result.
- Vignettes, README, and cli hint strings always show snake_case names.

## Removal schedule

Aliases stay through the 1.0 release: warn until 1.0, switch to
`lifecycle::deprecate_stop()` in the first post-1.0 minor release, delete one
release later.

Exception: `cli.run()` is a permanent alias. `cli_install()` copies a launcher
script to the user's `PATH`, and launchers installed before 0.4.0 call
`artma::cli.run()` from disk; removing the alias would break them on package
upgrade. It forwards without warning.
