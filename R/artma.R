#' @title Run meta-analysis with artma
#' @description
#' Main entry point for the artma package. This function orchestrates the complete
#' meta-analysis workflow: loading options, preparing data, and running specified
#' analytical methods.
#'
#' @param data *\[data.frame, optional\]* Data frame to analyze. If `NULL`, data will
#'   be loaded from the options file (see `options` parameter). When provided, this
#'   data will be used directly, bypassing the data reading step.
#' @param methods *\[character, optional\]* A character vector of method names to run.
#'   Use `"all"` to run all available methods. If `NULL`, an interactive menu will
#'   prompt you to select methods. See `artma::methods.list()` for available methods.
#' @param options *\[character, optional\]* Name of the options file (with or without
#'   `.yaml` extension) to use. If `NULL` and running interactively, you will be
#'   prompted to create or select an options file.
#' @param options_dir *\[character, optional\]* Directory containing the options file.
#'   If `NULL`, uses the default options directory.
#' @param ... Additional arguments passed to the runtime methods.
#'
#' @return *\[list\]* A named list containing results from each method, indexed by
#'   method name. The structure of each result depends on the specific method.
#'
#' @details
#' The `artma()` function is the primary way to interact with the artma package.
#' It handles the complete workflow:
#'
#' 1. **Options Loading**: Loads configuration from an options file (or prompts for
#'    creation in interactive mode)
#' 2. **Data Preparation**: Reads and prepares your data (unless `data` is provided)
#' 3. **Method Execution**: Runs the specified analytical methods on your data
#' 4. **Results**: Returns a structured list of results
#'
#' ## Options Files
#'
#' Options files are YAML configuration files that store all settings for your analysis,
#' including data paths, column mappings, method parameters, and output preferences.
#' They ensure reproducibility and make it easy to manage multiple analysis configurations.
#'
#' ## Methods
#'
#' Methods are analytical functions that perform specific meta-analysis tasks (e.g.,
#' funnel plots, Bayesian Model Averaging, effect size calculations). You can run
#' multiple methods in a single call, and they will execute in a predefined order.
#'
#' ## Data Parameter
#'
#' When `data` is provided, it bypasses the data reading step and uses your data frame
#' directly. The data will still be preprocessed and validated according to your
#' options configuration. This is useful when you already have data loaded in R or
#' want to analyze data programmatically.
#'
#' @examples
#' \dontrun{
#' # Interactive mode - will prompt for options and methods
#' results <- artma()
#'
#' # Run specific methods with an options file
#' results <- artma(
#'   methods = c("funnel_plot", "bma", "fma"),
#'   options = "my_analysis.yaml"
#' )
#'
#' # Run all methods
#' results <- artma(methods = "all", options = "my_analysis.yaml")
#'
#' # Use data directly (bypasses file reading)
#' my_data <- data.frame(
#'   effect = c(0.5, 0.3, 0.7),
#'   se = c(0.1, 0.15, 0.12),
#'   study_id = c("Study A", "Study B", "Study C")
#' )
#' results <- artma(data = my_data, methods = "funnel_plot")
#'
#' # Access results
#' funnel_result <- results$funnel_plot
#' }
#'
#' @seealso
#' - `artma::methods.list()` - List available methods
#' - `artma::options.create()` - Create a new options file
#' - `artma::prepare_data()` - Prepare data manually
#'
#' @export
artma <- function(
    data = NULL,
    methods = NULL,
    options = NULL,
    options_dir = NULL,
    ...) {
  box::use(
    artma / interactive / welcome[
      show_welcome_message,
      is_first_time_user,
      mark_welcome_as_shown
    ]
  )

  # Check and show welcome message if first-time user (only in interactive mode)
  if (interactive() && is_first_time_user(options_dir)) {
    show_welcome_message()
    mark_welcome_as_shown(options_dir)
  }

  main <- function() {
    box::use(
      artma / data / index[prepare_data],
      artma / libs / core / utils[get_verbosity],
      artma / output / export[
        resolve_output_dir, ensure_output_dirs, export_results
      ],
      artma / interactive / welcome[
        show_welcome_message,
        is_first_time_user,
        mark_welcome_as_shown
      ]
    )

    # Ensure output directories exist before methods run (graphics export
    # happens during method execution and needs the directories in place)
    save_results <- getOption("artma.output.save_results", TRUE)
    output_dir <- NULL
    if (isTRUE(save_results)) {
      output_dir <- resolve_output_dir()
      ensure_output_dirs(output_dir)
    }

    # Prepare data: use provided data or load from options
    if (is.null(data)) {
      df <- prepare_data()
    } else {
      # User provided data directly - still need to preprocess and compute
      # nolint start: object_usage_linter.
      box::use(
        artma / data / preprocess[preprocess_data],
        artma / data / compute[compute_optional_columns]
      )
      # nolint end

      if (get_verbosity() >= 3) {
        cli::cli_inform("Using provided data frame (skipping file read step).")
      }

      df <- preprocess_data(data)
      df <- compute_optional_columns(df)
    }

    # Invoke methods
    results <- invoke_runtime_methods(methods = methods, df = df, ...)

    # Export tabular results
    if (isTRUE(save_results)) {
      export_results(results, output_dir)
    }

    if (get_verbosity() >= 3) {
      cli::cli_alert_success("Analysis complete.")
      if (isTRUE(save_results)) {
        cli::cli_alert_info("Results saved to {.path {output_dir}}")
      }
    }

    invisible(results)
  }

  runtime_setup( # nolint: box_usage_linter. # Imported on a package-level
    options_file_name = options,
    options_dir = options_dir,
    FUN = main
  )
}

#' @title Invoke methods
#' @description Pass a vector of runtime methods to invoke, together with a data frame to invoke these methods on, and invoke them.
#' @param methods *\[character\]* A character vector of the methods to invoke.
#' @param df *\[data.frame\]* The data frame to invoke the methods on.
#' @param ... *\[any\]* Additional arguments to pass to the methods.
#' @return *\[list\]* Results of the invocations, indexed by method names.
#'
#' Internal example:
#' df <- data.frame(...)
#' invoke_runtime_methods(c("funnel_plot", "bma", "fma"), df)
#'
#' @keywords internal
invoke_runtime_methods <- function(methods, df, ...) {
  box::use(
    artma / const[CONST],
    artma / libs / core / string[pluralize],
    artma / libs / core / utils[get_verbosity],
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
    methods <- climenu::checkbox(
      choices = supported_methods,
      prompt = "No runtime methods were provided. Please select the methods you would like to run: ",
      allow_select_all = TRUE
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
      # nolint start: object_usage_linter.
      selected_methods <- paste(as.character(invalid_methods), collapse = ", ")
      # nolint end
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
    cli::cli_h3("Running {.emph {CONST$PACKAGE_NAME}} methods")
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
