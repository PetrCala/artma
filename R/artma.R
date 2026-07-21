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
#' @param open_results *\[logical, optional\]* Whether to open the results directory
#'   after exporting results. Defaults to `FALSE`.
#' @param ... Additional arguments passed to the runtime methods.
#'
#' @return *\[list\]* A named list containing results from each method, indexed by
#'   method name. The structure of each result depends on the specific method.
#'   Methods that fail are omitted from the list; their names and error messages
#'   are attached as the `failed_methods` attribute.
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
#' ## Method Failures
#'
#' A method that throws an error does not abort the run. The failing method is
#' skipped with a warning, the remaining methods still execute, and results from
#' the methods that succeeded are exported as usual. A summary of successes and
#' failures is printed at the end of the run. The run itself never signals an
#' error because of a method failure; when every requested method fails, a final
#' warning is emitted instead. Failed method names and their error messages are
#' available in the `failed_methods` attribute of the returned list.
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
  open_results = FALSE,
  ...
) {
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
      # User provided data directly - still need to preprocess and compute.
      # Mirror prepare_data's phases: decide the NA strategy (configure), run
      # the pure preprocess + compute, then persist the computed columns.
      # nolint start: object_usage_linter.
      box::use(
        artma / data / preprocess[clean_data, preprocess_data, resolve_na_handling],
        artma / data / compute[compute_optional_columns, update_config_with_computed_columns]
      )
      # nolint end

      if (get_verbosity() >= 3) {
        cli::cli_inform("Using provided data frame (skipping file read step).")
      }

      resolve_na_handling(clean_data(data))
      df <- preprocess_data(data)
      df <- compute_optional_columns(df)
      update_config_with_computed_columns(df)
    }

    # Invoke methods
    results <- invoke_runtime_methods(methods = methods, df = df, ...)

    # Export tabular results
    if (isTRUE(save_results)) {
      export_results(results, output_dir)
    }

    if (isTRUE(save_results) && isTRUE(open_results) && interactive()) {
      tryCatch(
        results.open(),
        error = function(e) {
          if (get_verbosity() >= 2) {
            cli::cli_alert_warning(
              "Unable to open results directory: {e$message}"
            )
          }
        }
      )
    }

    if (get_verbosity() >= 3) {
      cli::cli_alert_success("Analysis complete.")
      if (isTRUE(save_results)) {
        cli::cli_alert_info("Results saved to {.path {output_dir}}")
        if (!isTRUE(open_results)) {
          cli::cli_alert_info(
            "Run {.code artma::results.open()} to open the results directory."
          )
        }
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
#'
#' Methods run in an order derived from their declared `depends_on` metadata
#' (topologically sorted, discovery order preserved among independent methods);
#' each upstream result is passed to its dependents as a `<dependency>_result`
#' argument. Before a method runs, its declared `required_columns` are checked
#' against the data frame and its `suggests` packages against the installed
#' set; a method that fails either check is skipped with an explanation rather
#' than aborting the run.
#'
#' Method failures are isolated: a method that throws an error is skipped with
#' a warning and the remaining methods still run. The run only aborts for
#' invalid input (for example, unknown method names), never because a method
#' failed or was skipped. When at least one method fails or is skipped, a
#' summary is printed at the end, and a final warning is emitted if every method
#' failed.
#' @param methods *\[character\]* A character vector of the methods to invoke.
#' @param df *\[data.frame\]* The data frame to invoke the methods on.
#' @param modules_dir *\[character, optional\]* Directory to discover runtime method modules in. Defaults to `NULL`, in which case the standard package methods directory is used. Used mainly for dependency injection in tests.
#' @param ... *\[any\]* Additional arguments to pass to the methods.
#' @return *\[list\]* Results of the invocations, indexed by method names.
#'   Failed methods are omitted; their names and error messages are attached
#'   as the `failed_methods` attribute. Methods skipped for missing columns or
#'   packages are attached as the `skipped_methods` attribute (both named
#'   character vectors).
#'
#' Internal example:
#' df <- data.frame(...)
#' invoke_runtime_methods(c("funnel_plot", "bma", "fma"), df)
#'
#' @keywords internal
invoke_runtime_methods <- function(methods, df, modules_dir = NULL, ...) {
  box::use(
    artma / const[CONST],
    artma / libs / core / string[pluralize],
    artma / libs / core / utils[get_verbosity],
    artma / modules / runtime_methods[
      get_method_metadata,
      get_runtime_method_modules,
      missing_required_columns,
      missing_suggested_packages,
      topo_sort_methods
    ]
  )

  RUNTIME_METHOD_MODULES <- if (is.null(modules_dir)) { # nolint: box_usage_linter.
    get_runtime_method_modules()
  } else {
    get_runtime_method_modules(modules_dir = modules_dir)
  }
  supported_methods <- names(RUNTIME_METHOD_MODULES)

  method_meta <- stats::setNames(
    lapply(supported_methods, function(name) {
      get_method_metadata(RUNTIME_METHOD_MODULES[[name]]$run, name = name)
    }),
    supported_methods
  )

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
      # Opt-in methods (typically too expensive to run by default) are excluded
      # from "all" and must be requested by name.
      is_opt_in <- vapply(
        method_meta[supported_methods],
        function(meta) isTRUE(meta$opt_in),
        logical(1)
      )
      return(supported_methods[!is_opt_in])
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

  # Order by declared dependencies (topological sort), keeping the discovery
  # order for methods that are independent of one another. A dependent whose
  # dependency is not requested runs standalone; the method itself falls back to
  # recomputing what it needs.
  requested_in_order <- supported_methods[supported_methods %in% methods]
  deps_map <- lapply(method_meta[requested_in_order], function(meta) meta$depends_on)
  methods <- topo_sort_methods(requested_in_order, deps_map)

  if (get_verbosity() >= 3) {
    cli::cli_h3("Running {.emph {CONST$PACKAGE_NAME}} methods")
    cli::cli_inform(c(
      "i" = "Invoking {length(methods)} {pluralize('method', length(methods))} in total."
    ))
  }

  results <- list()
  succeeded_methods <- character()
  failed_methods <- character()
  skipped_methods <- character()

  extra_args <- list(...)

  for (method_name in methods) {
    meta <- method_meta[[method_name]]

    # Gate on declared required columns: skip (do not abort) when any are absent.
    missing_cols <- missing_required_columns(df, meta$required_columns)
    if (length(missing_cols) > 0L) {
      reason <- sprintf(
        "missing required %s: %s",
        pluralize("column", length(missing_cols)),
        paste(missing_cols, collapse = ", ")
      )
      skipped_methods[[method_name]] <- reason
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("Skipping {.code {method_name}}: {reason}")
      }
      next
    }

    # Gate on declared optional-package dependencies. Soft-skip everywhere, with
    # an interactive install offer. The one hard-abort case: a non-interactive
    # run that requested exactly this single method, so a script gets a clear
    # signal rather than silently producing nothing.
    missing_pkgs <- missing_suggested_packages(meta$suggests)
    if (length(missing_pkgs) > 0L && interactive()) {
      missing_pkgs <- prompt_install_missing_packages(missing_pkgs, method_name)
    }
    if (length(missing_pkgs) > 0L) {
      reason <- sprintf(
        "missing suggested %s: %s",
        pluralize("package", length(missing_pkgs)),
        paste(missing_pkgs, collapse = ", ")
      )
      if (!interactive() && length(methods) == 1L) {
        cli::cli_abort(c(
          "x" = "Method {.code {method_name}} cannot run: {reason}.",
          "i" = "Install with: {.code install.packages({deparse(missing_pkgs)})}"
        ))
      }
      skipped_methods[[method_name]] <- reason
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("Skipping {.code {method_name}}: {reason}")
      }
      next
    }

    if (get_verbosity() >= 3) {
      cli::cli_inform("{cli::symbol$bullet} Running the {.code {method_name}} method...")
    }

    method_args <- c(list(df = df), extra_args)

    # Pass each upstream dependency's result to the dependent as
    # `<dependency>_result`, unless the caller already supplied it.
    for (dep in meta$depends_on) {
      dep_arg <- paste0(dep, "_result")
      if (!(dep_arg %in% names(extra_args)) && !is.null(results[[dep]])) {
        method_args[[dep_arg]] <- results[[dep]]
      }
    }

    method_failed <- FALSE
    method_result <- tryCatch(
      do.call(RUNTIME_METHOD_MODULES[[method_name]]$run, method_args),
      error = function(e) {
        method_failed <<- TRUE
        failed_methods[[method_name]] <<- conditionMessage(e)
        if (get_verbosity() >= 2) {
          cli::cli_warn(
            "Method {.code {method_name}} failed and was skipped: {conditionMessage(e)}"
          )
        }
        NULL
      }
    )

    if (!method_failed) {
      succeeded_methods <- c(succeeded_methods, method_name)
      results[[method_name]] <- method_result
    }
  }

  # Build unified MA table when BMA and/or FMA have produced results. Both
  # methods return the standard contract, so the coefficient frame lives in
  # `tables$coefficients` and skip reasons under `meta$skipped`.
  extract_ma_coefficients <- function(result) {
    if (is.null(result) || !is.list(result) || !is.null(result$meta$skipped)) {
      return(NULL)
    }
    coefs <- result$tables$coefficients
    if (is.null(coefs) || !is.data.frame(coefs) || nrow(coefs) == 0) {
      return(NULL)
    }
    coefs
  }

  bma_coefs <- extract_ma_coefficients(results[["bma"]])
  fma_coefs <- extract_ma_coefficients(results[["fma"]])

  if (!is.null(bma_coefs) || !is.null(fma_coefs)) {
    box::use(artma / output / ma_table[build_ma_table, display_ma_table])

    round_to <- as.integer(getOption("artma.output.number_of_decimals", 3))
    ma_table <- build_ma_table(
      bma_coefficients = bma_coefs,
      fma_coefficients = fma_coefs,
      round_to = round_to
    )

    if (!is.null(ma_table)) {
      display_ma_table(ma_table, verbosity = get_verbosity())
      results[["ma_table"]] <- list(
        tables = list(summary = ma_table),
        plots = list(),
        meta = list()
      )
    }
  }

  if (length(failed_methods) > 0L) {
    attr(results, "failed_methods") <- failed_methods
  }
  if (length(skipped_methods) > 0L) {
    attr(results, "skipped_methods") <- skipped_methods
  }

  if ((length(failed_methods) > 0L || length(skipped_methods) > 0L) && get_verbosity() >= 3) {
    cli::cli_h3("Method run summary")
    if (length(succeeded_methods) > 0L) {
      cli::cli_inform(c(
        "v" = "Succeeded: {.code {succeeded_methods}}"
      ))
    }
    for (skipped_name in names(skipped_methods)) {
      cli::cli_inform(c(
        "i" = "Skipped: {.code {skipped_name}} ({skipped_methods[[skipped_name]]})"
      ))
    }
    for (failed_name in names(failed_methods)) {
      cli::cli_inform(c(
        "x" = "Failed: {.code {failed_name}} ({failed_methods[[failed_name]]})"
      ))
    }
  }

  if (length(failed_methods) > 0L && length(succeeded_methods) == 0L && get_verbosity() >= 2) {
    cli::cli_warn(
      "All {length(methods)} requested {pluralize('method', length(methods))} failed. No method results were produced."
    )
  }

  results
}

#' @title Offer to install missing suggested packages
#' @description
#' In interactive sessions, offer to install the optional packages a method
#' needs before it runs. Returns the packages still missing afterwards (the
#' whole set unchanged when the user declines or the session is
#' non-interactive), so the caller can decide to skip the method.
#' @param pkgs *\[character\]* Packages the method suggests but that are absent.
#' @param method_name *\[character\]* Method the packages are needed for.
#' @param is_installed *\[function, optional\]* Predicate testing package
#'   availability. Injectable for testing; defaults to `requireNamespace`.
#' @param install_packages *\[function, optional\]* Installer. Injectable for
#'   testing; defaults to `utils::install.packages`.
#' @return *\[character\]* The packages that remain missing.
#' @keywords internal
prompt_install_missing_packages <- function(pkgs, method_name,
                                            is_installed = NULL,
                                            install_packages = NULL) {
  box::use(
    artma / libs / core / autonomy[should_prompt_user]
  )

  if (length(pkgs) == 0L) {
    return(character())
  }
  if (is.null(is_installed)) {
    is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)
  }
  if (!interactive() || !should_prompt_user(required_level = "balanced")) {
    return(pkgs)
  }

  choice <- climenu::select(
    choices = c("Yes", "No"),
    prompt = cli::format_inline(
      "Method {.code {method_name}} needs {pkgs}, which {?is/are} not installed. Install now?"
    )
  )
  if (!identical(choice, "Yes")) {
    return(pkgs)
  }

  if (is.null(install_packages)) {
    install_packages <- function(p) utils::install.packages(p)
  }
  tryCatch(install_packages(pkgs), error = function(e) NULL)

  pkgs[!vapply(pkgs, function(pkg) isTRUE(is_installed(pkg)), logical(1))]
}
