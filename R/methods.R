#' @title List methods
#' @description
#' Print an overview of the runtime methods supported by artma, and return it
#' as a data frame. The table is built from the metadata each method registers
#' with `register_runtime_method()`: what it does, which data columns it needs,
#' which methods run before it, which optional packages it uses (and whether
#' they are installed), and whether it is opt-in (left out of
#' `methods = "all"`).
#'
#' Pass a data frame to `available_for` to turn the table into a pre-flight
#' check: each method's required columns are matched against that data, so a
#' method that would be skipped is visible before the run rather than after.
#'
#' @param available_for *\[data.frame, optional\]* Data to check each method's
#'   required columns against. When supplied, the returned frame gains a
#'   `missing_columns` column and an `available` flag. Defaults to `NULL`.
#' @return *\[data.frame\]* Invisibly, one row per method, with the `method`,
#'   `description`, `required_columns`, `depends_on`, `suggests`,
#'   `missing_packages`, `installed`, and `opt_in` columns (plus
#'   `missing_columns` and `available` when `available_for` is supplied).
#'   Multi-value columns are comma-separated strings.
#' @examples
#' \dontrun{
#' artma::methods.list()
#' artma::methods.list(available_for = my_data)
#' }
#' @export
methods.list <- function(available_for = NULL) {
  box::use(
    artma / const[CONST],
    artma / libs / core / utils[get_verbosity],
    artma / modules / methods_table[build_methods_table, print_methods_table]
  )

  methods <- build_methods_table(available_for = available_for)

  if (get_verbosity() >= 2) {
    cli::cli_h1("{.file {CONST$PACKAGE_NAME}} ({packageVersion(CONST$PACKAGE_NAME)}) runtime methods:")
    print_methods_table(methods)
  }

  invisible(methods)
}
