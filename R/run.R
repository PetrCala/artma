#' @title Invoke methods
#' @description Pass a vector of runtime methods to invoke, together with a data frame to invoke these methods on, and invoke them.
#' @param methods *\[character\]* A character vector of the methods to invoke.
#' @param df *\[data.frame\]* The data frame to invoke the methods on.
#' `list` Results of the invocations, indexed by method names.
#' @param ... *\[any\]* Additional arguments to pass to the methods.
#'
#' Internal example:
#' df <- data.frame(...)
#' invoke_runtime_methods(c("funnel_plot", "bma"), df)
#'
#' @keywords internal
invoke_runtime_methods <- function(methods, df, ...) {
  box::use(
    artma / const[CONST],
    artma / libs / string[pluralize]
  )

  RUNTIME_METHOD_MODULES <- get_runtime_method_modules() # nolint: box_usage_linter.
  supported_methods <- names(RUNTIME_METHOD_MODULES)

  if (is.null(methods)) {
    methods <- utils::select.list(
      title = "No runtime methods were provided. Please select the methods you would like to run: ",
      choices = supported_methods,
      multiple = TRUE
    )
    if (rlang::is_empty(methods)) {
      cli::cli_abort("No runtime methods were selected. Aborting…")
    }
  }

  if (methods == "all") {
    methods <- supported_methods
  } else if (!(is.character(methods) && all(methods %in% supported_methods))) {
    selected_methods <- glue::glue_collapse(as.character(methods), sep = ", ") # nolint: unused_declared_object_linter.
    cli::cli_abort(c(
      "x" = "Invalid runtime methods selected: {.val {selected_methods}}",
      "i" = "To see a list of available methods, run {.code artma::methods.list()}"
    ))
  }

  if (getOption("artma.verbose", 3) >= 3) {
    cli::cli_h3("Running the main {.emph {CONST$PACKAGE_NAME}} function")
    cli::cli_inform(c(
      "i" = "Invoking {length(methods)} {pluralize('method', length(methods))} in total."
    ))
  }

  results <- list()
  for (i in seq_along(supported_methods)) {
    method_name <- methods[i]
    if (method_name %in% methods) {
      if (getOption("artma.verbose", 3) >= 3) {
        cli::cli_inform("{cli::symbol$bullet} Running the {.code {method_name}} method…")
      }
      results[[method_name]] <- RUNTIME_METHOD_MODULES[[method_name]]$run(df = df, ...)
    }
  }
  results
}

#' @title Run artma
#' @description Run artma with the specified methods and options.
#' @param methods *\[character, optional\]* A character vector of the methods to invoke. Defaults to NULL.
#' @param options_file_name *\[character, optional\]* The name of the options file to use. Defaults to NULL.
#' @param options_dir *\[character, optional\]* The directory containing the options file. Defaults to NULL.
#' @return *\[list\]* Results of the invocations, indexed by method names.
#' @export
run <- function(
    methods = NULL,
    options_file_name = NULL,
    options_dir = NULL) {
  main <- function() {
    box::use(artma / data / index[prepare_data])

    df <- prepare_data()
    results <- invoke_runtime_methods(methods = methods, df = df)
    if (getOption("artma.verbose", 3) >= 3) {
      cli::cli_alert_success("Done.")
    }
  }

  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options_file_name,
    options_dir = options_dir,
    FUN = main
  )
}
