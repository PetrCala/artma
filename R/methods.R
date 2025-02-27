#' @title List methods
#' @description Print all runtime methods supported by ARTMA into the console.
#' @export
methods.list <- function() {
  static_setup() # nolint: box_usage_linter. # Imported on a package-level

  box::use(artma / const[CONST])

  cli::cli_text(paste("As of the current version,", CONST$PACKAGE_NAME, "supports the following runtime methods:"))
  cli::cli_ul(CONST$SUPPORTED_METHODS)
}


#' @keywords internal
get_method_mapping <- function() {
  static_setup() # nolint: box_usage_linter. # Imported on a package-level

  box::use(artma / const[CONST])

  default <- function() print("This method is not yet implemented.")

  mapping <- list(
    variable_summary_stats = default,
    effect_summary_stats = default,
    prima_facie_graphs = default,
    box_plot = default,
    funnel_plot = default,
    t_stat_histogram = default,
    linear_tests = default,
    nonlinear_tests = default,
    exo_tests = default,
    p_hacking_tests = default,
    bma = default,
    fma = default,
    ma_variables_description_table = default,
    bpe = default,
    bpe_graphs = default,
    robma = default
  )

  n_mapped_methods <- length(mapping)
  n_recognized_methods <- length(CONST$SUPPORTED_METHODS)

  if (n_mapped_methods != n_recognized_methods) {
    rlang::abort(glue::glue("Invalid method mapping. {n_mapped_methods} methods are mapped, while {n_recognized_methods} methods are recognized."))
  }

  uncrecognized_names_idxs <- !names(mapping) %in% CONST$SUPPORTED_METHODS
  if (any(uncrecognized_names_idxs)) {
    unrecognized_names <- glue::glue_collapse(names(mapping)[uncrecognized_names_idxs], sep = ", ")
    rlang::abort(paste("Unrecognized names in the method mapping:", unrecognized_names))
  }

  mapping
}

#' @keywords internal
METHOD_MAPPING <- get_method_mapping() # nolint: unused_declared_object_linter.
