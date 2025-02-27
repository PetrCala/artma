#' @keywords internal
get_method_mapping <- function() {
  static_setup() # nolint: box_usage_linter. # Imported on a package-level

  box::use(
    artma / const[CONST],
    artma / paths[PATHS],
    artma / libs / modules[crawl_and_import_modules]
  )

  modules <- crawl_and_import_modules(PATHS$DIR_METHODS)

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

  # Here, add validation in a way that imports each of the functions from the relevant module and informs if anything about the imported functions is off

  mapping
}

#' @keywords internal
METHOD_MAPPING <- get_method_mapping() # nolint: unused_declared_object_linter.

#' @title List methods
#' @description Print all runtime methods supported by ARTMA into the console.
#' @export
methods.list <- function() {
  static_setup() # nolint: box_usage_linter. # Imported on a package-level

  box::use(artma / const[CONST])

  cli::cli_text(paste("As of the current version,", CONST$PACKAGE_NAME, "supports the following runtime methods:"))
  cli::cli_ul(names(METHOD_MAPPING))
}
