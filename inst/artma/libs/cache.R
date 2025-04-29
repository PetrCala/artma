#' @title Create a new artifact
#' @description Create a new artifact
#' @param value *\[any\]* The value to cache
#' @param log *\[list\]* The log to cache
#' @param meta *\[list\]* The meta data to cache
#' @return `NULL`
new_artifact <- function(value, log, meta) {
  base::structure( # nolint: undesirable_function_linter.
    list(value = value, log = log, meta = meta),
    class = "cached_artifact"
  )
}

#' @title Print cached artifact
#' @description Print cached artifact
#' @param x *\[cached_artifact\]* The cached artifact to print
#' @param ... *\[any\]* Additional arguments
#' @return `NULL`
print.cached_artifact <- function(x, ...) {
  cli::cli_h3("Artifact")

  cli::cli_text("{.bold Value:} {.val {x$value}}")
  cli::cli_text("{.bold Log:} ({length(x$log)} entries)")

  if (length(x$log) > 0) {
    msgs <- vapply(x$log, `[[`, "", "message")
    cli::cli_bullets(msgs)
  }

  cli::cli_text("{.bold Meta:}")
  for (line in utils::capture.output(utils::str(x$meta, give.attr = FALSE, comp.str = ""))) {
    cli::cli_text(line)
  }

  invisible(x)
}

# -------------------------------------------------------------------------
# explicitly register the S3 method (needed when pkg not attached)
# -------------------------------------------------------------------------
#' @export
#' @method print cached_artifact
NULL

#' @title Find the most-recent console-printing call from the cli package
#' @description Find the most-recent console-printing call from the cli package
#' @param calls *\[list\]* The calls to inspect
#' @param add_pkg_prefix *\[logical\]* Whether to add the package prefix
#' @return *\[character\]* A character scalar like "cli::inform", or NA_character_ if no such call is on the stack.
last_cli_print <- function(calls = sys.calls(), add_pkg_prefix = FALSE) {
  box::use(artma / libs / modules[get_pkg_exports])

  funs <- get_pkg_exports("cli")

  ## Examine the stack from the innermost frame outward
  for (call in rev(calls)) {
    head <- call[[1L]] # the function part of the call

    ## Detect namespaced calls of the form cli::something()
    if (is.call(head) && identical(head[[1L]], quote(`::`))) {
      pkg <- as.character(head[[2L]])
      fun <- as.character(head[[3L]])

      if (identical(pkg, "cli") && fun %in% funs) {
        out <- if (add_pkg_prefix) paste0(pkg, "::", fun) else fun
        return(out)
      }
    }
  }

  NA_character_
}


#' @title Evaluate an expression, trapping *every* cli call it makes
#' @description Evaluate an expression, trapping *every* cli call it makes
#' @param expr *\[expression\]* The expression to evaluate
#' @return *\[list\]* A list with the following elements:
#'   * **value** – the value of the expression
#'   * **log** – a list of the cli calls
#'   * **fun**     – the original helper (e.g. "cli_inform")
#'   * **message** – fully rendered, ready-to-print text
#'
#' The expression's own value is returned unchanged in `value`.
#'
capture_cli <- function(expr) {
  expr <- substitute(expr) # preserve NSE
  logs <- list()

  ## -----------------------------------------------------------
  ##  Condition handler: record + silence the message
  ## -----------------------------------------------------------
  handler <- function(cnd) {
    str_msg <- cnd$args$text$str # For 'alert' based messages
    logs <<- append(logs, list(list(
      fun     = last_cli_print(), # e.g. "cli_inform"
      message = if (is.null(str_msg)) conditionMessage(cnd) else str_msg
    )))
    # Muffle the message
    if (!is.null(r <- findRestart("cli_message_handled"))) invokeRestart(r)
    if (inherits(cnd, "message") && !is.null(r <- findRestart("muffleMessage"))) invokeRestart(r)
    if (inherits(cnd, "warning") && !is.null(r <- findRestart("muffleWarning"))) invokeRestart(r)
    cli::cli_abort("invalid muffle restart")
  }

  value <- withCallingHandlers(
    eval(expr, parent.frame()),
    cli_message = handler,
    message = handler,
    warning = handler
  )

  list(value = value, log = logs)
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
replay_log <- function(log, ..., .envir = parent.frame()) {
  for (entry in log) {
    fn <- get(entry$fun, envir = asNamespace("cli"))
    fn(entry$message, ...)
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
#'
#' # get the artifact
#' art <- get_artifact(cache, key)
#' art$value # <- model, plot, etc.
#' art$log # <- list of stored cli conditions
#'
#' # To watch the console story again
#' replay_log(art$log)
#' }
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
      memoise::forget(worker_memoised)
    }

    art <- worker_memoised(...)

    # --- on cache hit you still want to replay! ---
    replay_log(art$log)
    art$value
  }
}

#' @title Get artifact
#' @description Get artifact
#' @param cache *\[memoise::cache_filesystem\]* The cache to use
#' @param key *\[character\]* The key to get
#' @return *\[cached_artifact\]* The artifact
#' @examples
#' \dontrun{
#' cache <- memoise::cache_filesystem(rappdirs::user_cache_dir("artma"))
#' keys <- cache$keys() # hashes of all artifacts
#' art <- get_artifact(cache, keys[[1]]) # read the first one
#'
#' art$value # <- model, plot, etc.
#' art$log # <- list of stored cli conditions
#' }
get_artifact <- function(cache, key) {
  cache$get(key)$value
}

box::export(
  cache_cli,
  capture_cli,
  get_artifact,
  last_cli_print,
  new_artifact,
  print.cached_artifact,
  replay_log
)
