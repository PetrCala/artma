box::use(
  artma / modules / utils[crawl_and_import_modules, validate_runtime_method_modules],
  artma / libs / utils[get_verbosity],
  artma / paths[PATHS]
)

# Modules can opt out of runtime registration by setting this flag to FALSE.
RUNTIME_METHOD_MARKER <- ".__runtime_method__"

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
  RUNTIME_METHOD_MARKER
)
