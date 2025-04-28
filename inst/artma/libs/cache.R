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
  cli::cli_h1("Artifact")
  cli::cli_text("Value: {x$value}")
  cli::cli_text("Log: {x$log}")
  cli::cli_text("Meta: {x$meta}")
}

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
      log <<- append(log, list(c))
      # Let it print now, or muffle if you prefer
    }
  )
  list(value = value, log = log)
}

#' @title Replay log
#' @description Replay log
#' @param log *\[list\]* The log to replay
#' @return `NULL`
#' @export
replay_log <- function(log) {
  for (cond in log) {
    # replay as-is
    cli::cli_inform(cond$message, .envir = parent.frame())
    # or, if you muffled earlier, you can show a subtle “(cached)” tag:
    # cli::cli_alert_info("{cond$message} {.dim (cached)}")
  }
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
  force(fun) # lock the original function inside the closure

  box::use(artma / paths[PATHS])

  .cache_dir <- PATHS$DIR_USR_CACHE
  if (!dir.exists(.cache_dir)) dir.create(.cache_dir, recursive = TRUE)

  .cli_cache <- memoise::cache_filesystem(.cache_dir)

  cache <- if (is.null(cache)) .cli_cache else cache

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
      worker_memoised$forget(...)
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
