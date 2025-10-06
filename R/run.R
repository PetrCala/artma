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
    artma / const[CONST], # nolint: box_unused_att_mod_obj_linter.
    artma / libs / string[pluralize], # nolint: box_unused_att_mod_obj_linter.
    artma / libs / utils[get_verbosity],
    artma / modules / runtime_methods[get_runtime_method_modules]
  )

  arrange_methods <- function(method_names) {
    execution_order <- CONST$RUNTIME_METHODS$EXECUTION_ORDER

    if (is.null(execution_order)) {
      execution_order <- character()
    }

    execution_order <- unique(as.character(execution_order[!is.na(execution_order)]))

    if (!length(execution_order)) {
      return(method_names)
    }

    ordered <- execution_order[execution_order %in% method_names]
    remaining <- method_names[!method_names %in% ordered]

    c(ordered, remaining)
  }

  RUNTIME_METHOD_MODULES <- get_runtime_method_modules() # nolint: box_usage_linter.
  supported_methods <- arrange_methods(names(RUNTIME_METHOD_MODULES))

  if (is.null(methods)) {
    methods <- climenu::menu(
      choices = supported_methods,
      title = "No runtime methods were provided. Please select the methods you would like to run: ",
      multiple = TRUE
    )
    if (rlang::is_empty(methods)) {
      cli::cli_abort("No runtime methods were selected. Aborting...")
    }
  }

  resolve_methods <- function(methods_input) {
    if (rlang::is_string(methods_input) && methods_input == "all") {
      return(supported_methods)
    }

    if (is.factor(methods_input)) {
      methods_input <- as.character(methods_input)
    }

    if (!is.character(methods_input)) {
      cli::cli_abort(c(
        "x" = "Runtime methods must be supplied as a character vector.",
        "i" = "To see a list of available methods, run {.code artma::methods.list()}"
      ))
    }

    if (any(is.na(methods_input))) {
      cli::cli_abort("Runtime methods must not contain missing values.")
    }

    methods_input <- trimws(methods_input)

    if (length(methods_input) == 0L) {
      cli::cli_abort("At least one runtime method must be specified.")
    }

    deduped_methods <- unique(methods_input)
    invalid_methods <- setdiff(deduped_methods, supported_methods)

    if (length(invalid_methods) > 0L) {
      selected_methods <- glue::glue_collapse(as.character(invalid_methods), sep = ", ") # nolint: unused_declared_object_linter.
      cli::cli_abort(c(
        "x" = "Invalid runtime methods selected: {.val {selected_methods}}",
        "i" = "To see a list of available methods, run {.code artma::methods.list()}"
      ))
    }

    deduped_methods
  }

  methods <- resolve_methods(methods)

  methods <- supported_methods[supported_methods %in% methods]

  if (get_verbosity() >= 3) {
    cli::cli_h3("Running the main {.emph {CONST$PACKAGE_NAME}} function")
    cli::cli_inform(c(
      "i" = "Invoking {length(methods)} {pluralize('method', length(methods))} in total."
    ))
  }

  results <- list()
  for (method_name in methods) {
    if (get_verbosity() >= 3) {
      cli::cli_inform("{cli::symbol$bullet} Running the {.code {method_name}} method...")
    }
    results[[method_name]] <- RUNTIME_METHOD_MODULES[[method_name]]$run(df = df, ...)
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
    box::use(
      artma / data / index[prepare_data],
      artma / libs / utils[get_verbosity]
    )

    df <- prepare_data()
    results <- invoke_runtime_methods(methods = methods, df = df)
    if (get_verbosity() >= 3) {
      cli::cli_alert_success("Done.")
    }
  }

  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options_file_name,
    options_dir = options_dir,
    FUN = main
  )
}
