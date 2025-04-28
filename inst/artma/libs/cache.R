#' @title Create a new artifact
#' @description Create a new artifact
#' @param value *\[any\]* The value to cache
#' @param log *\[list\]* The log to cache
#' @param meta *\[list\]* The meta data to cache
#' @return `NULL`
#' @export
new_artifact <- function(value, log, meta) {
  base::structure(list(value = value, log = log, meta = meta), # nolint: undesirable_function_linter.
    class = "cached_artifact"
  )
}

#' @title Print cached artifact
#' @description Print cached artifact
#' @param x *\[cached_artifact\]* The cached artifact to print
#' @param ... *\[any\]* Additional arguments
#' @return `NULL`
#' @export
print.cached_artifact <- function(x, ...) {
  header <- "Artifact"
  cat(header, "\n", sep = "")

  cat("Value:\n")
  print(x$value)

  cat("\nLog (", length(x$log), " entr", if (length(x$log) == 1) "y" else "ies",
    ")\n",
    sep = ""
  )
  if (length(x$log)) {
    cat("  • ", vapply(x$log, `[[`, "", "message"), sep = "\n  • ")
  }

  cat("\n\nMeta:\n")
  str(x$meta, give.attr = FALSE, comp.str = "")

  invisible(x)
}

# -------------------------------------------------------------------------
# explicitly register the S3 method (needed when pkg not attached)
# -------------------------------------------------------------------------
#' @export
#' @method print cached_artifact
NULL

#' @title Capture cli
#' @description Capture cli
#' @param expr *\[expression\]* The expression to capture
#' @return `NULL`
#' @export
capture_cli <- function(expr) {
  log <- list()

  value <- withCallingHandlers(
    expr,
    cli_message = function(c) {
      log <<- append(log, list(
        list(
          message = conditionMessage(c), # always a plain string
          classes = class(c)
        )
      ))
      ## DO NOT muffle – we still want the message to appear live
    }
  )

  list(value = value, log = log)
}

#' @title Replay log
#' @description Replay log
#' @param log *\[list\]* The log to replay
#' @return `NULL`
#' @examples
#' \dontrun{
#' # Run this in a regular R session _after_ you've used any cache_cli wrapper
#' cache <- memoise::cache_filesystem(rappdirs::user_cache_dir("artma"))
#' keys <- cache$keys() # hashes of all artifacts
#' art <- cache$get(keys[[1]]) # read the first one
#'
#' art$value # <- model, plot, etc.
#' art$log # <- list of stored cli conditions
#'
#' # To watch the console story again
#' replay_log(art$log)
#' }
#' @export
replay_log <- function(log) {
  for (entry in log) {
    cli::cli_inform(entry$message, .envir = parent.frame())
  }
  invisible(NULL)
}

#' @title Cache cli
#' @description Cache cli
#' @param fun *\[function\]* The function to cache
#' @param extra_keys *\[list\]* Additional keys
#' @param invalidate_fun *\[function\]* Optional custom invalidator
#' @param cache *\[memoise::cache_filesystem\]* The cache to use
#' @return `NULL`
#' @examples
#' \dontrun{
#' # real work here — bookended by cli alerts but not memoised itself
#' .run_models_impl <- function(df, formula, seed = 123) {
#'   cli::cli_alert("Starting model fit…")
#'
#'   set.seed(seed)
#'   mod <- stats::lm(formula, data = df)
#'
#'   cli::cli_alert_success("Done.")
#'   list(
#'     model = mod,
#'     tidy = broom::tidy(mod),
#'     glance = broom::glance(mod)
#'   )
#' }
#'
#' # memoised, log-aware version exported to users
#' run_models <- cache_cli(
#'   .run_models_impl,
#'   extra_keys = list(pkg_ver = utils::packageVersion("yourpkg"))
#' )
#' }
#' @export
cache_cli <- function(fun,
                      extra_keys = list(),
                      invalidate_fun = NULL, # optional custom invalidator
                      cache = NULL) {
  base::force(fun) # lock the original function inside the closure

  # — pick / create the cache ------------------------------------------------
  if (is.null(cache)) {
    box::use(artma / paths[PATHS])
    cache_dir <- PATHS$DIR_USR_CACHE
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    cache <- memoise::cache_filesystem(cache_dir)
  }

  ## The worker that actually does the heavy lifting --------------------------
  worker <- function(...) {
    res <- capture_cli(fun(...))
    meta <- list(
      timestamp = Sys.time(),
      extra     = extra_keys,
      session   = utils::sessionInfo()
    )
    new_artifact(res$value, res$log, meta)
  }

  ## Memoise that worker ------------------------------------------------------
  worker_memoised <- memoise::memoise(worker, cache = cache)

  ## The *public* wrapper that callers will see ------------------------------
  function(...) {
    # Allow user-supplied invalidator
    if (!is.null(invalidate_fun) && isTRUE(invalidate_fun(...))) {
      worker_memoised$forget(worker_memoised, ...)
    }

    art <- worker_memoised(...)

    # --- on cache hit you still want to replay! ---
    replay_log(art$log)
    art$value
  }
}

box::export(
  cache_cli,
  capture_cli,
  new_artifact,
  print.cached_artifact,
  replay_log
)
