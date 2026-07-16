box::use(
  artma / modules / utils[crawl_and_import_modules, validate_runtime_method_modules],
  artma / libs / core / utils[get_verbosity],
  artma / paths[PATHS]
)

# Modules can opt out of runtime registration by setting this flag to FALSE.
RUNTIME_METHOD_MARKER <- ".__runtime_method__"

#' @title Standard runtime method return contract
#' @description
#' Build the value every runtime method returns. The contract has three slots:
#'
#' * `tables`: a named list of `data.frame`s destined for CSV export. The
#'   exporter walks this list; keys become filename suffixes (see
#'   `export_method_result`).
#' * `plots`: a named list of plot objects (for example `ggplot`s) for
#'   programmatic access and printing. Graphics files are written by the method
#'   during execution, not by this contract.
#' * `meta`: a named list of anything else a downstream consumer needs (the BMA
#'   model, fit parameters, skip reasons, auxiliary frames, and so on).
#'
#' @param tables *\[list\]* Named list of `data.frame`s to export.
#' @param plots *\[list\]* Named list of plot objects.
#' @param meta *\[list\]* Named list of method-specific extras.
#' @param class *\[character, optional\]* Extra S3 class(es) to prepend, used by
#'   methods that ship a bespoke print method.
#' @return *\[list\]* A list with `tables`, `plots`, and `meta` elements.
new_method_result <- function(tables = list(), plots = list(), meta = list(), class = NULL) {
  if (!is.list(tables) || !is.list(plots) || !is.list(meta)) {
    cli::cli_abort("`tables`, `plots`, and `meta` must all be lists.")
  }

  result <- list(tables = tables, plots = plots, meta = meta)

  if (!is.null(class)) {
    class(result) <- c(class, class(result))
  }

  result
}

#' @title Runtime method metadata attribute
#' @description Name of the attribute that carries a method's declarative
#'   metadata on its exported `run` wrapper.
METHOD_META_ATTR <- "artma_method_meta"

#' @title Register a runtime method
#' @description
#' Wrap a method implementation in the shared caching layer, attach the method's
#' declarative metadata, and return the `run` function every method file
#' exports. This replaces the per-file registration trailer that previously
#' duplicated the `cache_cli_runner` wiring across all method modules.
#'
#' The metadata drives the orchestrator in `invoke_runtime_methods()`:
#'
#' * `depends_on`: names of methods that must run before this one. The
#'   orchestrator topologically sorts by these edges and passes each upstream
#'   result to the dependent as a `<dependency>_result` argument (for example
#'   a `depends_on = "bma"` method receives `bma_result`).
#' * `required_columns`: columns that must be present in the data frame. A
#'   method whose columns are missing is skipped with an explanation instead of
#'   aborting the run.
#' * `suggests`: optional packages the method needs. A method whose suggested
#'   packages are not installed is skipped (with an interactive install offer);
#'   this is the single, declarative gate for optional-package dependencies.
#'
#' @param impl *\[function\]* The method implementation (its `run` worker).
#' @param stage *\[character\]* Stage label used for cache keys and the cache
#'   hit notice. Conventionally matches the implementation's name.
#' @param depends_on *\[character, optional\]* Names of methods this one depends
#'   on. Defaults to no dependencies.
#' @param required_columns *\[character, optional\]* Columns the method needs in
#'   the data frame. Defaults to none.
#' @param suggests *\[character, optional\]* Optional packages the method needs.
#'   Defaults to none.
#' @param label *\[character, optional\]* Human-readable display label. Defaults
#'   to `stage`.
#' @param key_builder *\[function, optional\]* Cache signature builder. Defaults
#'   to the standard data cache signature.
#' @param ... *\[any\]* Extra arguments forwarded to `cache_cli_runner`.
#' @return *\[function\]* The cached `run` wrapper, carrying the method metadata
#'   in its `artma_method_meta` attribute.
register_runtime_method <- function(impl, stage,
                                    depends_on = character(),
                                    required_columns = character(),
                                    suggests = character(),
                                    label = NULL,
                                    key_builder = NULL, ...) {
  box::use(
    artma / libs / infrastructure / cache[cache_cli_runner],
    artma / data / cache_signatures[build_data_cache_signature]
  )

  if (is.null(key_builder)) {
    key_builder <- function(...) build_data_cache_signature()
  }

  run <- cache_cli_runner(impl, stage = stage, key_builder = key_builder, ...)

  attr(run, METHOD_META_ATTR) <- list(
    stage = stage,
    label = if (is.null(label)) stage else label,
    depends_on = as.character(depends_on),
    required_columns = as.character(required_columns),
    suggests = as.character(suggests)
  )

  run
}

#' @title Read a runtime method's declarative metadata
#' @description Return the metadata attached to a method's `run` wrapper,
#'   filling in defaults for methods that predate declarative registration.
#' @param run_fn *\[function\]* A method's exported `run` wrapper.
#' @param name *\[character, optional\]* Method name, used as the default stage
#'   and label when the wrapper carries no metadata.
#' @return *\[list\]* A list with `stage`, `label`, `depends_on`,
#'   `required_columns`, and `suggests`.
get_method_metadata <- function(run_fn, name = NULL) {
  meta <- attr(run_fn, METHOD_META_ATTR, exact = TRUE)
  if (is.null(meta)) {
    meta <- list()
  }

  defaults <- list(
    stage = name %||% NA_character_,
    label = name %||% NA_character_,
    depends_on = character(),
    required_columns = character(),
    suggests = character()
  )

  utils::modifyList(defaults, meta)
}

#' @title Topologically sort methods by their declared dependencies
#' @description
#' Order `method_names` so that every method runs after the methods it depends
#' on. Dependencies pointing outside `method_names` are ignored (the dependent
#' runs standalone). Ties preserve the input order, keeping the sort stable.
#' Aborts when the dependency graph contains a cycle.
#' @param method_names *\[character\]* Methods to order (in their base order).
#' @param deps *\[list, optional\]* Named list mapping a method name to the
#'   character vector of methods it depends on.
#' @return *\[character\]* `method_names` in dependency-respecting order.
topo_sort_methods <- function(method_names, deps = list()) {
  ordered <- character()
  remaining <- method_names

  while (length(remaining) > 0L) {
    progressed <- FALSE
    for (name in remaining) {
      prereqs <- intersect(deps[[name]] %||% character(), method_names)
      if (all(prereqs %in% ordered)) {
        ordered <- c(ordered, name)
        remaining <- remaining[remaining != name]
        progressed <- TRUE
        break
      }
    }
    if (!progressed) {
      cli::cli_abort(c(
        "x" = "Cyclic method dependencies detected.",
        "i" = "Methods involved in the cycle: {.val {remaining}}"
      ))
    }
  }

  ordered
}

#' @title Columns a method requires but the data frame lacks
#' @param df *\[data.frame\]* The prepared data frame.
#' @param required_columns *\[character\]* Columns the method declares it needs.
#' @return *\[character\]* The missing column names (empty when all are present).
missing_required_columns <- function(df, required_columns) {
  if (length(required_columns) == 0L) {
    return(character())
  }
  setdiff(as.character(required_columns), colnames(df))
}

#' @title Suggested packages a method needs but are not installed
#' @param suggests *\[character\]* Optional packages the method declares.
#' @param is_installed *\[function, optional\]* Predicate testing whether a
#'   package is installed. Injectable for testing; defaults to
#'   `requireNamespace`.
#' @return *\[character\]* The missing package names (empty when all present).
missing_suggested_packages <- function(suggests, is_installed = NULL) {
  if (length(suggests) == 0L) {
    return(character())
  }
  if (is.null(is_installed)) {
    is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)
  }
  suggests <- as.character(suggests)
  suggests[!vapply(suggests, function(pkg) isTRUE(is_installed(pkg)), logical(1))]
}

module_has_run <- function(module) {
  is.function(module[["run"]])
}

resolve_runtime_marker <- function(module, marker) {
  flag <- module[[marker]]
  if (!is.null(flag)) {
    return(flag)
  }

  ns <- attr(module, "namespace")
  if (is.environment(ns) && exists(marker, envir = ns, inherits = FALSE)) {
    return(get(marker, envir = ns, inherits = FALSE))
  }

  NULL
}

module_should_be_runtime_method <- function(module, module_name = NULL, marker = RUNTIME_METHOD_MARKER) {
  runtime_flag <- resolve_runtime_marker(module, marker)
  if (!is.null(runtime_flag)) {
    if (!is.logical(runtime_flag) || length(runtime_flag) != 1L || is.na(runtime_flag)) {
      if (is.null(module_name)) {
        module_name <- "<unnamed>"
      }
      cli::cli_abort(cli::format_inline(
        "Invalid value supplied for the runtime method marker in module {.code {module_name}}. Expected a single logical scalar."
      ))
    }
    if (isFALSE(runtime_flag)) {
      return(FALSE)
    }
  }

  module_has_run(module)
}

gather_runtime_modules <- function(
    modules,
    include_predicate,
    excluded_modules = NULL) {
  if (length(modules) == 0) {
    return(list(runtime = modules, skipped = character()))
  }

  module_names <- names(modules)
  include_flags <- vapply(
    module_names,
    function(name) {
      if (!is.null(excluded_modules) && name %in% excluded_modules) {
        return(FALSE)
      }
      res <- include_predicate(modules[[name]], name)
      if (is.null(res) || is.na(res)) {
        return(FALSE)
      }
      isTRUE(res)
    },
    logical(1)
  )

  runtime_modules <- modules[include_flags]
  skipped_modules <- module_names[!include_flags]
  list(runtime = runtime_modules, skipped = skipped_modules)
}

get_runtime_method_modules <- function(
    modules_dir = PATHS$DIR_METHODS,
    include_predicate = module_should_be_runtime_method,
    excluded_modules = NULL) {
  modules <- crawl_and_import_modules(modules_dir)
  split_modules <- gather_runtime_modules(modules, include_predicate, excluded_modules)
  runtime_modules <- split_modules$runtime

  if (length(split_modules$skipped) > 0L && get_verbosity() >= 3) {
    skipped_label <- paste(split_modules$skipped, collapse = ", ")
    cli::cli_inform(c(
      "i" = "Skipping non-runtime method modules: {skipped_label}"
    ))
  }

  validate_runtime_method_modules(runtime_modules)
  runtime_modules
}

box::export(
  get_method_metadata,
  get_runtime_method_modules,
  METHOD_META_ATTR,
  missing_required_columns,
  missing_suggested_packages,
  module_should_be_runtime_method,
  new_method_result,
  register_runtime_method,
  RUNTIME_METHOD_MARKER,
  topo_sort_methods
)
