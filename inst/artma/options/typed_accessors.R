#' @title Typed option accessors with template-sourced defaults
#' @description Single home for the option defaults that runtime code used to
#'   re-hardcode inline. Two concerns live here:
#'
#'   1. `option_template_default()` reads the default straight from the parsed
#'      options template and coerces it through the B4 type registry, so a
#'      template edit cannot silently drift from the value runtime code falls back
#'      to. The template is parsed once (via the memoised `read_template`) and the
#'      flattened definition map is cached in a module-level environment, so no
#'      option access re-reads the YAML.
#'
#'   2. The typed accessors (`get_precision_type()`, `get_winsorization_level()`,
#'      `get_na_handling()`, `get_se_zero_handling()`) each mirror the exact
#'      `getOption`/`opt_or` call their former inline call sites used, so the
#'      migration is behavior-preserving.
#'
#'   `precision_type` sources its fallback from the template (its template default
#'   equals the value that was inlined). `na_handling` and `se_zero_handling`
#'   declare a `.na` template sentinel (`allow_na`): the template deliberately
#'   carries no concrete default, and the documented runtime strategy is resolved
#'   at read time. Those runtime defaults live in one place here so the compute
#'   readers and the configure-phase setters cannot drift. `winsorization_level`
#'   keeps the compute-layer "unset means disabled" default of `0`, which is a
#'   distinct contract from the template's load-time default of `0.01`; it is
#'   centralized here rather than duplicated across the two readers.

box::use(
  artma / options / template[flatten_template_options, read_template],
  artma / options / type_registry[get_option_type_spec],
  artma / libs / core / utils[opt_or],
  artma / paths[PATHS]
)

# Runtime resolution of the `.na` template sentinel. The template declares no
# concrete default for these options (they are `allow_na`), so the compute phase
# falls back to the documented strategy. Kept here so na_handling.R,
# preprocess.R, and configure.R all reference one value.
NA_HANDLING_DEFAULT <- "stop"
SE_ZERO_HANDLING_DEFAULT <- "remove"

# Compute-layer "winsorization disabled" default. Distinct from the template's
# load-time default (0.01): an option that was never loaded means "do not
# winsorize", which is what both compute readers assume.
WINSORIZATION_DISABLED_LEVEL <- 0

# Cache of option-name -> definition, keyed by template path and mtime so an
# on-disk template change invalidates it. Avoids re-flattening on every access;
# the YAML read itself is already memoised in `read_template`.
.defs_cache <- new.env(parent = emptyenv()) # nolint: object_name_linter.

.option_defs_by_name <- function(template_path) { # nolint: object_name_linter.
  mtime <- as.numeric(file.mtime(template_path))
  key <- paste0(template_path, "@", mtime)
  cached <- .defs_cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }
  defs <- flatten_template_options(read_template(template_path))
  by_name <- list()
  for (def in defs) {
    by_name[[def$name]] <- def
  }
  .defs_cache[[key]] <- by_name
  by_name
}

# Strip the `artma.` namespace prefix to reach the template leaf name.
.leaf_name <- function(name) sub("^artma\\.", "", name) # nolint: object_name_linter.

#' @title Read an option's template-declared default
#' @description Return the default declared for `name` in the options template,
#'   coerced through the B4 type registry. A `.na` sentinel default (`allow_na`)
#'   is returned as `NA`; an option with no declared default returns `NULL`.
#' @param name *\[character\]* Full option name, e.g. `"artma.calc.precision_type"`.
#' @param template_path *\[character, optional\]* Path to the template YAML file.
#'   Defaults to the packaged template.
#' @return The coerced template default, or `NA`/`NULL` for sentinel/undeclared
#'   defaults.
#' @keywords internal
option_template_default <- function(name, template_path = PATHS$FILE_OPTIONS_TEMPLATE) {
  defs <- .option_defs_by_name(template_path)
  opt <- defs[[.leaf_name(name)]]
  if (is.null(opt)) {
    cli::cli_abort("No template definition found for option {.field {name}}.")
  }

  raw_default <- opt$default
  if (is.null(raw_default)) {
    return(if (isTRUE(opt$allow_na)) NA else NULL)
  }

  spec <- get_option_type_spec(opt$type)
  na_allowed <- isTRUE(opt$allow_na) && isTRUE(spec$allows_na)
  if (length(raw_default) == 1L && is.na(raw_default) && na_allowed) {
    return(raw_default)
  }

  spec$coerce(raw_default, opt)
}

#' @title Precision type accessor
#' @description The precision definition used when deriving a precision column.
#'   Falls back to the template-declared default when unset.
#' @return *\[character\]* The precision type.
#' @keywords internal
get_precision_type <- function() {
  getOption("artma.calc.precision_type", option_template_default("artma.calc.precision_type"))
}

#' @title Winsorization level accessor
#' @description The winsorization level applied to effect and standard error
#'   columns. Returns the compute-layer disabled default (`0`) when unset.
#' @return *\[numeric\]* The winsorization level.
#' @keywords internal
get_winsorization_level <- function() {
  getOption("artma.data.winsorization_level", default = WINSORIZATION_DISABLED_LEVEL)
}

#' @title Missing-value handling strategy accessor
#' @description The strategy for handling missing values in the compute phase.
#'   Resolves the `.na` template sentinel to the documented `"stop"` default.
#' @return *\[character\]* The missing-value handling strategy.
#' @keywords internal
get_na_handling <- function() {
  opt_or("artma.data.na_handling", NA_HANDLING_DEFAULT)
}

#' @title Maximum imputation missingness accessor
#' @description The maximum share of missing values an optional numeric column
#'   may have and still be imputed. Falls back to the template-declared default
#'   when unset.
#' @return *\[numeric\]* The missingness-ratio threshold in \[0, 1\].
#' @keywords internal
get_max_imputation_missingness <- function() {
  getOption("artma.data.max_imputation_missingness", option_template_default("artma.data.max_imputation_missingness"))
}

#' @title Zero-standard-error handling strategy accessor
#' @description The strategy for handling zero standard errors in the compute
#'   phase. Resolves the `.na` template sentinel to the documented `"remove"`
#'   default.
#' @return *\[character\]* The zero-standard-error handling strategy.
#' @keywords internal
get_se_zero_handling <- function() {
  opt_or("artma.calc.se_zero_handling", SE_ZERO_HANDLING_DEFAULT)
}

box::export(
  option_template_default,
  get_precision_type,
  get_winsorization_level,
  get_na_handling,
  get_max_imputation_missingness,
  get_se_zero_handling,
  NA_HANDLING_DEFAULT,
  SE_ZERO_HANDLING_DEFAULT
)
