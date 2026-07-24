#' @title Declarative option-spec resolver
#' @description Every runtime method used to read its options with the same
#'   four-touch pattern: read (`get_option_group` + `%||%` default), a
#'   `validate()` type block, an `assert()` range block, and a re-cast into the
#'   `resolved_options` list. This module replaces that with a single
#'   spec-driven resolver: a method declares a table of specs (name, default,
#'   type, constraint, cast) and `resolve_options()` returns the validated,
#'   cast, range-checked values.
#'
#'   Type coercion and validation are delegated to the shared option type
#'   registry (`options/type_registry.R`, item B4) via `validate_option_value`,
#'   so this module never re-implements the per-type `switch` logic that the
#'   registry already owns.

box::use(
  artma / libs / core / validation[assert],
  artma / options / utils[validate_option_value]
)

#' @title Build one option spec
#' @description Construct a single spec consumed by `resolve_options()`. Only
#'   `default` and `type` are required; the remaining fields are opt-in and
#'   default to "do nothing", so a spec with just a default and a type performs
#'   a read plus a registry type-check.
#' @param default *\[any\]* Value used when the option is unset. Must match the
#'   template default for the option.
#' @param type *\[character\]* Registry type key used for validation, e.g.
#'   `"numeric"`, `"integer"`, `"logical"`, `"character"`. Match the type the
#'   method historically validated (an integer template option validated with
#'   `is.numeric` uses `type = "numeric"` plus `cast = as.integer`).
#' @param allow_na *\[logical\]* Whether `NA` is a permitted value. When `TRUE`
#'   and the resolved value is a scalar `NA`, the cast and constraint are
#'   skipped, matching the historical `is.na(x) || ...` guards.
#' @param from *\[character, optional\]* Group key to read the raw value from
#'   when it differs from the output name (e.g. a nested `"simulate_cdfs.seed"`
#'   key resolving into a `simulate_cdfs_seed` output).
#' @param key *\[character, optional\]* Fully-qualified option name read via
#'   `getOption()` instead of from the passed group (for global options such as
#'   `"artma.output.number_of_decimals"`).
#' @param cast *\[function, optional\]* Applied to a non-`NA` value after
#'   validation, e.g. `as.integer`.
#' @param scalar *\[logical\]* When `TRUE`, assert the value is length one.
#' @param constraint *\[function, optional\]* Predicate applied to the non-`NA`
#'   value; when it does not return `TRUE`, the resolver aborts with
#'   `constraint_msg`.
#' @param constraint_msg *\[character, optional\]* Error message paired with
#'   `constraint`.
#' @return *\[list\]* A single option spec.
opt_spec <- function(default,
                     type,
                     allow_na = FALSE,
                     from = NULL,
                     key = NULL,
                     cast = NULL,
                     scalar = FALSE,
                     constraint = NULL,
                     constraint_msg = NULL) {
  list(
    default = default,
    type = type,
    allow_na = allow_na,
    from = from,
    key = key,
    cast = cast,
    scalar = scalar,
    constraint = constraint,
    constraint_msg = constraint_msg
  )
}

is_scalar_na <- function(x) length(x) == 1L && is.na(x)

#' @title Resolve a group of options from a spec table
#' @description Read, type-validate, cast, and range-check a set of options
#'   declared as specs. The output is a named list keyed by the spec names,
#'   suitable for use directly as a method's `resolved_options`.
#' @param group *\[list\]* The option group returned by `get_option_group()` for
#'   the method (e.g. `get_option_group("artma.methods.box_plot")`). Specs with
#'   a `key` read past this group via `getOption()`.
#' @param specs *\[list\]* Named list of specs built with `opt_spec()`.
#' @return *\[list\]* Named list of resolved option values.
resolve_options <- function(group, specs) {
  resolved <- vector("list", length(specs))
  names(resolved) <- names(specs)

  for (name in names(specs)) {
    spec <- specs[[name]]

    raw <- if (!is.null(spec$key)) {
      getOption(spec$key, spec$default)
    } else {
      read_key <- spec$from %||% name
      group[[read_key]] %||% spec$default
    }

    # Type coercion/validation is owned by the shared option type registry.
    err <- validate_option_value(raw, spec$type, name, allow_na = spec$allow_na)
    if (!is.null(err)) {
      cli::cli_abort(err, .subclass = "validation_error")
    }

    if (isTRUE(spec$scalar)) {
      assert(length(raw) == 1L, sprintf("Option '%s' must be a single value.", name))
    }

    val <- raw
    skip_transforms <- is_scalar_na(val)

    if (!is.null(spec$cast) && !skip_transforms) {
      val <- spec$cast(val)
    }

    if (!is.null(spec$constraint) && !skip_transforms) {
      assert(isTRUE(spec$constraint(val)), spec$constraint_msg)
    }

    resolved[[name]] <- val
  }

  resolved
}

box::export(opt_spec, resolve_options)
