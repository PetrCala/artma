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

#' @title Register a runtime method
#' @description
#' Wrap a method implementation in the shared caching layer and return the
#' `run` function every method file exports. This replaces the per-file
#' registration trailer that previously duplicated the `cache_cli_runner`
#' wiring across all method modules.
#' @param impl *\[function\]* The method implementation (its `run` worker).
#' @param stage *\[character\]* Stage label used for cache keys and the cache
#'   hit notice. Conventionally matches the implementation's name.
#' @param key_builder *\[function, optional\]* Cache signature builder. Defaults
#'   to the standard data cache signature.
#' @param ... *\[any\]* Extra arguments forwarded to `cache_cli_runner`.
#' @return *\[function\]* The cached `run` wrapper.
register_runtime_method <- function(impl, stage, key_builder = NULL, ...) {
  box::use(
    artma / libs / infrastructure / cache[cache_cli_runner],
    artma / data / cache_signatures[build_data_cache_signature]
  )

  if (is.null(key_builder)) {
    key_builder <- function(...) build_data_cache_signature()
  }

  cache_cli_runner(impl, stage = stage, key_builder = key_builder, ...)
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
  get_runtime_method_modules,
  module_should_be_runtime_method,
  new_method_result,
  register_runtime_method,
  RUNTIME_METHOD_MARKER
)
