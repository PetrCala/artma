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


call_cli_default <- function(msg) {
  fallback <- base::get0("cli_server_default", envir = asNamespace("cli"), inherits = FALSE)

  if (is.function(fallback)) {
    fallback(msg)
    return(invisible(NULL))
  }

  message_text <- NULL

  if (is.list(msg) && !is.null(msg$message)) {
    message_text <- msg$message
  } else {
    message_text <- tryCatch(conditionMessage(msg), error = function(err) NULL)
  }

  if (!is.null(message_text) && length(message_text) > 0L) {
    cli::cli_inform(paste(message_text, collapse = "\n"))
  }

  invisible(NULL)
}


#' @title Evaluate an expression, trapping *every* cli call it makes
#' @description Evaluate an expression, trapping *every* cli call it makes
#' @param expr *\[expression\]* The expression to evaluate
#' @param emit *\[logical\]* Should captured CLI output be emitted to the
#'   console while recording? Defaults to `TRUE`. When `FALSE`, messages are
#'   logged silently for later replay.
#' @return *\[list\]* A list with the following elements:
#'   * **value** – the value of the evaluated expression
#'   * **log** – a list of replayable entries. Each entry contains a
#'     `kind` describing how it should be replayed (currently either the
#'     string "condition" for `cli_message` signals or "call" for direct
#'     `cli::cat_*()` helpers) alongside the metadata required to replay it.
#'
#' The expression's own value is returned unchanged in `value`, while CLI
#' output continues to be emitted to the console during the initial run.
#'
capture_cli <- function(expr, emit = TRUE) {
  box::use(artma / libs / modules[get_pkg_exports])

  expr <- substitute(expr) # preserve NSE
  emit <- isTRUE(emit)
  logs <- list()

  pkg_funs <- get_pkg_exports("cli")
  cat_helpers <- pkg_funs[grepl("^cat_", pkg_funs)]
  ns_cli <- asNamespace("cli") # cli’s namespace environment
  originals <- list() # to keep the real helpers

  inside_default_handler <- FALSE
  cat_depth <- 0L

  ## ------------------------------------------------------------------
  ## 1. Monkey-patch every requested cat_*() helper
  ## ------------------------------------------------------------------
  make_wrapper <- function(orig_fun, fun_name) {
    force(orig_fun)
    force(fun_name)

    function(...) {
      args <- rlang::list2(...)

      if (inside_default_handler || cat_depth > 0L) {
        return(rlang::exec(orig_fun, !!!args))
      }

      cat_depth <<- cat_depth + 1L
      on.exit(cat_depth <<- cat_depth - 1L, add = TRUE)

      res <- if (emit) {
        rlang::exec(orig_fun, !!!args)
      } else {
        utils::capture.output(res <- rlang::exec(orig_fun, !!!args))
        res
      }

      logs <<- append(logs, list(list(
        kind = "call",
        namespace = "cli",
        fun = fun_name,
        args = args
      )))

      res
    }
  }

  for (fn in cat_helpers) {
    if (exists(fn, envir = ns_cli, inherits = FALSE)) {
      originals[[fn]] <- get(fn, envir = ns_cli)
      unlockBinding(fn, ns_cli)
      assign(fn, make_wrapper(originals[[fn]], fn), envir = ns_cli)
      lockBinding(fn, ns_cli)
    }
  }

  ## Always restore originals
  on.exit(
    {
      for (fn in names(originals)) {
        unlockBinding(fn, ns_cli)
        assign(fn, originals[[fn]], envir = ns_cli)
        lockBinding(fn, ns_cli)
      }
    },
    add = TRUE
  )

  ## -----------------------------------------------------------
  ##  2. Custom default handler: record + emit the message
  ## -----------------------------------------------------------
  previous_handler <- getOption("cli.default_handler")
  options(cli.default_handler = function(msg) {
    old_flag <- inside_default_handler
    inside_default_handler <<- TRUE
    on.exit(inside_default_handler <<- old_flag, add = TRUE)

    rendered <- tryCatch(
      cli:::cli__fmt(list(msg), collapse = TRUE, strip_newline = TRUE),
      error = function(err) NULL
    )
    fallback_message <- tryCatch(conditionMessage(msg), error = function(err) "")

    logs <<- append(logs, list(list(
      kind = "condition",
      namespace = "cli",
      cli_type = as.character(msg$type)[1],
      args = rlang::duplicate(msg$args, shallow = FALSE),
      message = if (is.null(rendered)) fallback_message else rendered
    )))

    if (emit) {
      if (is.function(previous_handler)) {
        previous_handler(msg)
      } else {
        call_cli_default(msg)
      }
    }
  })

  on.exit(options(cli.default_handler = previous_handler), add = TRUE)

  value <- eval(expr, parent.frame())

  list(value = value, log = logs)
}

#' @title Replay log
#' @description Replay log
#' @param log *\[list\]* The log produced by [capture_cli()].
#' @param ... Additional arguments forwarded to replayed CLI helper calls
#'   (i.e. entries created from `cli::cat_*()` helpers).
#' @return `NULL`
#' @examples
#' \dontrun{
#' # Run this in a regular R session _after_ you've used any cache_cli wrapper
#' cache <- memoise::cache_filesystem(rappdirs::user_cache_dir("artma"))
#' keys <- cache$keys() # hashes of all artifacts
#' art <- cache$get(keys[[1]]) # read the first one
#'
#' art$value # <- model, plot, etc.
#' art$log # <- list of stored cli messages and helper calls
#'
#' # To watch the console story again
#' replay_log(art$log)
#' }
replay_log <- function(log, ..., .envir = parent.frame()) {
  ns_cli <- asNamespace("cli")
  extra_args <- rlang::list2(...)

  for (entry in log) {
    if (identical(entry$kind, "condition")) {
      msg <- list(type = entry$cli_type, args = entry$args, message = entry$message)
      tryCatch(
        call_cli_default(msg),
        error = function(err) {
          kind <- entry$cli_type
          if (is.null(kind) || length(kind) == 0L) kind <- "unknown"
          cli::cli_warn(
            sprintf(
              "Failed to replay CLI condition of type '%s': %s",
              kind,
              conditionMessage(err)
            )
          )
          cli::cli_inform(paste(entry$message, collapse = "\n"))
        }
      )
      next
    }

    if (identical(entry$kind, "call")) {
      if (!exists(entry$fun, envir = ns_cli, inherits = FALSE)) {
        cli::cli_warn(
          sprintf("Cannot replay CLI helper '%s' because it is not available.", entry$fun),
        )
        next
      }

      fn <- get(entry$fun, envir = ns_cli)
      args <- c(entry$args, extra_args)
      rlang::exec(fn, !!!args)
      next
    }

    cli::cli_warn("Unrecognised cache log entry; skipping.")
  }

  invisible(NULL)
}

#' @title Cache cli
#' @description Wrap a function so that its results – including the CLI story it
#'   produces – are cached and replayed on subsequent calls.
#' @param fun *\[function\]* The function to cache.
#' @param extra_keys *\[list\]* Additional key material appended to the memoise
#'   cache key.
#' @param cache *\[memoise::cache_filesystem\]* The cache to use. Defaults to
#'   the user cache directory.
#' @param invalidate_fun *\[function\]* Optional predicate evaluated on each
#'   call to decide whether the cached value should be bypassed and recomputed.
#' @param max_age *\[numeric\]* Maximum age of cached artifacts in seconds.
#'   Use `Inf` to disable time-based invalidation. When `NULL` (the default) the
#'   value is sourced from the `artma.cache.max_age` option, falling back to
#'   `Inf`.
#' @return The wrapped function.
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
                      cache = NULL,
                      invalidate_fun = NULL,
                      max_age = NULL) {
  base::force(fun) # lock the original function inside the closure

  if (is.null(max_age)) {
    max_age <- getOption("artma.cache.max_age", Inf)
  }

  if (!is.numeric(max_age) || length(max_age) != 1L || is.na(max_age)) {
    max_age <- Inf
  }

  if (max_age < 0) {
    max_age <- 0
  }

  if (!getOption("artma.cache.use_cache", TRUE)) {
    return(fun)
  }

  # — pick / create the cache ------------------------------------------------
  if (is.null(cache)) {
    box::use(artma / paths[PATHS])
    cache_dir <- PATHS$DIR_USR_CACHE
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    cache <- memoise::cache_filesystem(cache_dir)
  }

  worker_state <- new.env(parent = emptyenv())

  ## The worker that actually does the heavy lifting --------------------------
  worker <- function(...) {
    worker_state$executed <- TRUE
    on.exit(worker_state$quiet <- FALSE, add = TRUE)

    emit_cli <- !isTRUE(worker_state$quiet)
    res <- capture_cli(fun(...), emit = emit_cli)
    meta <- list(
      timestamp = Sys.time(),
      extra     = extra_keys,
      session   = utils::sessionInfo(),
      cache     = list(max_age = max_age)
    )
    new_artifact(res$value, res$log, meta)
  }

  ## Memoise that worker ------------------------------------------------------
  worker_memoised <- memoise::memoise(worker, cache = cache)
  drop_worker_cache <- memoise::drop_cache(worker_memoised)

  ## The *public* wrapper that callers will see ------------------------------
  function(...) {
    dots <- rlang::list2(...)
    worker_state$executed <- FALSE

    if (!is.null(invalidate_fun)) {
      should_invalidate <- FALSE
      if (is.function(invalidate_fun)) {
        should_invalidate <- tryCatch(
          isTRUE(rlang::exec(invalidate_fun, !!!dots)),
          error = function(err) {
            cli::cli_warn(
              sprintf(
                "cache invalidator failed and was ignored: %s",
                conditionMessage(err)
              )
            )
            FALSE
          }
        )
      }

      if (isTRUE(should_invalidate)) {
        ## Ensure the predicate truly bypasses the cache by clearing all
        ## memoised entries; callers can re-populate whatever keys they need.
        tryCatch(
          memoise::forget(worker_memoised),
          error = function(err) {
            cli::cli_warn(
              sprintf(
                "cache invalidator failed to reset memoised state: %s",
                conditionMessage(err)
              )
            )
          }
        )
      }
    }

    log_to_replay <- NULL

    art <- rlang::exec(worker_memoised, !!!dots)
    cache_hit <- !isTRUE(worker_state$executed)

    if (cache_hit) {
      log_to_replay <- art$log
    }

    if (cache_hit && is.finite(max_age)) {
      art_ts <- art$meta$timestamp
      age <- tryCatch(
        as.numeric(difftime(Sys.time(), art_ts, units = "secs")),
        error = function(err) NA_real_
      )

      if (!is.na(age) && age > max_age) {
        old_art <- art
        log_to_replay <- old_art$log
        tryCatch(
          rlang::exec(drop_worker_cache, !!!dots),
          error = function(err) {
            cli::cli_warn(
              sprintf(
                "Failed to evict stale cache entry: %s",
                conditionMessage(err)
              )
            )
            FALSE
          }
        )

        tryCatch(
          {
            worker_state$executed <- FALSE
            worker_state$quiet <- TRUE
            art <- rlang::exec(worker_memoised, !!!dots)
          },
          error = function(err) {
            cli::cli_warn(
              sprintf(
                "Failed to refresh stale cache entry: %s",
                conditionMessage(err)
              )
            )
            art <- old_art
          },
          finally = {
            worker_state$quiet <- FALSE
          }
        )

        cache_hit <- TRUE
      }
    }

    if (cache_hit && length(log_to_replay) > 0L) {
      tryCatch(
        replay_log(log_to_replay),
        error = function(err) {
          cli::cli_warn(
            sprintf(
              "Failed to replay cached CLI output: %s",
              conditionMessage(err)
            )
          )
        }
      )
    }

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
