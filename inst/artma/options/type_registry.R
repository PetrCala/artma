#' @title Option type registry
#' @description Single source of truth mapping each option type name to its
#'   coercion, validation, and NA capability. Both `coerce_option_value`
#'   (options/template.R) and `validate_option_value` (options/utils.R) resolve a
#'   type spec from here instead of carrying their own parallel `switch`, so the
#'   two paths cannot drift apart.
#'
#'   A spec is a list with three fields:
#'   \itemize{
#'     \item `coerce(val, opt)`: return the value cast to the type, aborting
#'       (fail-loud) when the raw value cannot be represented.
#'     \item `validate(val, opt_name, opt_type)`: return an error string when the
#'       value does not match the type, or `NULL` when it does.
#'     \item `allows_na`: whether the type can hold `NA` at all. Combined with the
#'       option-level `allow_na` flag by the callers, so NA is permitted only when
#'       both agree. Every current type allows NA, keeping the option-level flag
#'       the effective gate.
#'   }
#'
#'   The coerce and validate paths are intentionally asymmetric for some types
#'   (e.g. `logical` coerces strings via `as.logical` but validation demands an
#'   already-logical value); the registry captures those asymmetries per field
#'   rather than smoothing them over.

box::use(
  artma / const[CONST]
)

# Abort helper shared by the numeric-family coercers.
abort_option_coercion <- function(opt, expected_type, val) {
  cli::cli_abort(c(
    "Cannot coerce option {CONST$STYLES$OPTIONS$NAME(opt$name)} to {CONST$STYLES$OPTIONS$TYPE(expected_type)}.",
    "x" = "Raw value: {CONST$STYLES$OPTIONS$VALUE(val)}."
  ))
}

# Cast via `as_fun`, treating "manufactured NA out of a non-NA value" as a
# failure rather than a silent downgrade.
coerce_via_cast <- function(val, opt, expected_type, as_fun) {
  coerced <- suppressWarnings(as_fun(val))
  if (any(is.na(coerced) & !is.na(val))) {
    abort_option_coercion(opt, expected_type, val)
  }
  coerced
}

# Uniform validation-error formatter.
format_type_error <- function(opt_name, expected_type, val) {
  cli::format_inline("Option {CONST$STYLES$OPTIONS$NAME(opt_name)} must be {CONST$STYLES$OPTIONS$TYPE(expected_type)}, got: {CONST$STYLES$OPTIONS$VALUE(val)}")
}

option_type_registry <- list(
  character = list(
    allows_na = TRUE,
    coerce = function(val, opt) as.character(val),
    validate = function(val, opt_name, opt_type) {
      if (!is.character(val)) format_type_error(opt_name, "character", val)
    }
  ),
  integer = list(
    allows_na = TRUE,
    coerce = function(val, opt) {
      if (!is.numeric(val)) abort_option_coercion(opt, "integer", val)
      non_na <- val[!is.na(val)]
      if (any(non_na != as.integer(non_na))) abort_option_coercion(opt, "integer", val)
      as.integer(val)
    },
    validate = function(val, opt_name, opt_type) {
      if (!is.numeric(val) || any(val != as.integer(val), na.rm = TRUE)) {
        format_type_error(opt_name, "integer", val)
      }
    }
  ),
  logical = list(
    allows_na = TRUE,
    coerce = function(val, opt) coerce_via_cast(val, opt, "logical", as.logical),
    validate = function(val, opt_name, opt_type) {
      if (!is.logical(val)) format_type_error(opt_name, "logical", val)
    }
  ),
  numeric = list(
    allows_na = TRUE,
    coerce = function(val, opt) coerce_via_cast(val, opt, "numeric", as.numeric),
    validate = function(val, opt_name, opt_type) {
      if (!is.numeric(val)) format_type_error(opt_name, "numeric", val)
    }
  ),
  list = list(
    allows_na = TRUE,
    coerce = function(val, opt) as.list(val),
    validate = function(val, opt_name, opt_type) NULL
  ),
  enum = list(
    allows_na = TRUE,
    coerce = function(val, opt) {
      box::use(artma / options / utils[parse_template_enum_value])
      coerced_val <- as.character(val)
      valid_values <- parse_template_enum_value(opt$type)
      if (!all(coerced_val %in% valid_values)) {
        cli::cli_abort(c(
          "Option {CONST$STYLES$OPTIONS$NAME(opt$name)} must be one of {.emph {toString(valid_values)}}.",
          "x" = "Got: {CONST$STYLES$OPTIONS$VALUE(val)}."
        ))
      }
      coerced_val
    },
    validate = function(val, opt_name, opt_type) {
      box::use(artma / options / utils[parse_template_enum_value])
      valid_values <- parse_template_enum_value(opt_type)
      if (!val %in% valid_values) {
        cli::format_inline(
          "Option {CONST$STYLES$OPTIONS$NAME(opt_name)} must be one of {.emph {toString(valid_values)}}; got {CONST$STYLES$OPTIONS$VALUE(val)}."
        )
      }
    }
  )
)

# Spec used for any type name not in the registry: pass the value through
# unchanged on coerce and treat it as always valid, matching the historical
# `switch` default arms in both callers.
passthrough_type_spec <- list(
  allows_na = TRUE,
  coerce = function(val, opt) val,
  validate = function(val, opt_name, opt_type) NULL
)

#' @title Resolve the registry key for an option type
#' @description Normalize a template type string to a registry key. Every
#'   `enum: a|b|c` collapses to the single `enum` key; all other types map to
#'   themselves.
#' @param opt_type *\[character\]* The template-declared option type.
#' @return *\[character\]* The registry key.
resolve_option_type_key <- function(opt_type) {
  if (startsWith(opt_type, "enum:")) {
    return("enum")
  }
  opt_type
}

#' @title Get the type spec for an option type
#' @description Return the `{coerce, validate, allows_na}` spec for a template
#'   type string, falling back to a pass-through spec for unknown types.
#' @param opt_type *\[character\]* The template-declared option type.
#' @return *\[list\]* The type spec.
get_option_type_spec <- function(opt_type) {
  spec <- option_type_registry[[resolve_option_type_key(opt_type)]]
  if (is.null(spec)) {
    return(passthrough_type_spec)
  }
  spec
}

box::export(
  get_option_type_spec,
  resolve_option_type_key
)
