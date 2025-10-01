box::use(
  artma / libs / utils[get_verbosity],
  artma / libs / validation[validate],
  artma / options / index[get_option_group]
)

#' Resolve the add_significance_marks option across methods.
#'
#' This helper looks up the shared methods option for significance marks,
#' falling back to any legacy, method-specific option the user may have set.
#' It enforces type validation and gently warns when deprecated option names
#' are encountered.
#'
#' @param method_path *[character?]* The fully-qualified option prefix for the
#'   calling method (e.g. "artma.methods.linear_tests"). Provide this to allow
#'   backwards-compatible lookups of legacy option names.
#'
#' @return *[logical]* Whether significance marks should be appended.
resolve_add_significance_marks <- function(method_path = NULL) {
  validate(is.null(method_path) || (is.character(method_path) && length(method_path) <= 1))

  shared_options <- get_option_group("artma.methods")
  shared_value <- shared_options$add_significance_marks

  legacy_value <- NULL
  if (!is.null(method_path)) {
    legacy_value <- getOption(paste0(method_path, ".add_significance_marks"))
  }

  if (is.null(shared_value) && !is.null(legacy_value) && get_verbosity() >= 2) {
    cli::cli_alert_info(
      "Using legacy option name {.code {method_path}.add_significance_marks}. Please migrate to {.code artma.methods.add_significance_marks}."
    )
  }

  if (!is.null(shared_value) && !is.null(legacy_value) && !identical(shared_value, legacy_value) && get_verbosity() >= 1) {
    cli::cli_alert_warning(
      "Ignoring legacy option value {legacy_value} for {.code {method_path}.add_significance_marks} in favour of shared {.code artma.methods.add_significance_marks}."
    )
  }

  resolved <- shared_value
  if (is.null(resolved)) {
    resolved <- legacy_value
  }
  if (is.null(resolved)) {
    resolved <- TRUE
  }

  validate(is.logical(resolved), length(resolved) == 1)

  as.logical(resolved)
}

box::export(resolve_add_significance_marks)
