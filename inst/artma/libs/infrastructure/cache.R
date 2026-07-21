# Filesystem-backed memoisation for expensive, data-dependent workflows.
#
# `cache_cli()` wraps a function so its result is memoised on disk via
# `memoise`. `cache_cli_runner()` layers stage naming and cache-signature
# injection on top so callers stay free of caching boilerplate. On a cache
# hit a single notice is printed; the wrapped function's own CLI output is
# not replayed.
#
# What ends up in a cache key
# ---------------------------
# `memoise` hashes the arguments of the memoised call, so the key covers:
#
# * every argument the caller passes, including the data frame and any
#   upstream `<dependency>_result` the orchestrator injects;
# * the `cache_signature` that `cache_cli_runner()` builds via `key_builder`,
#   which for runtime methods carries the stage label, the user-authored
#   `artma.*` options, the data source path and mtime, the package version,
#   the package source fingerprint, and the method's own source hash.
#
# `memoise` also folds in the memoised function's body, but that body is the
# fixed `worker` closure below and is identical for every stage, so stage
# isolation comes from the signature rather than from `memoise`.
#
# Beyond the key, a cached artifact is rejected when it has outlived `max_age`
# or when the output files it recorded on the cold run are no longer on disk.

#' @title Construct a cached artifact
#' @description Bundle a computed value with the metadata required to reason
#'   about cache freshness. The artifact is what `memoise` stores; callers only
#'   ever see `artifact$value`.
#' @param value *\[any\]* The value to cache.
#' @param meta *\[list\]* Metadata describing the cached value (timestamp,
#'   extra key material, and the TTL used when it was produced).
#' @return *\[cached_artifact\]* An S3 object of class `cached_artifact`.
new_artifact <- function(value, meta = list()) {
  base::structure( # nolint: undesirable_function_linter.
    list(value = value, meta = meta),
    class = "cached_artifact"
  )
}

#' @title Print a cached artifact
#' @description Human-readable summary of a cached artifact, useful when
#'   inspecting the on-disk cache during debugging.
#' @param x *\[cached_artifact\]* The cached artifact to print.
#' @param ... *\[any\]* Ignored.
#' @return The artifact, invisibly.
print.cached_artifact <- function(x, ...) {
  cli::cli_h3("Artifact")
  cli::cli_text("{.bold Value:} {.val {x$value}}")
  cli::cli_text("{.bold Meta:}")
  for (line in utils::capture.output(utils::str(x$meta, give.attr = FALSE, comp.str = ""))) {
    cli::cli_text(line)
  }
  invisible(x)
}

#' @title Resolve the effective cache TTL
#' @description Normalise the `max_age` argument, sourcing the default from the
#'   `artma.cache.max_age` option when it is `NULL`. Invalid values fall back to
#'   `Inf` (no time-based invalidation); negative values collapse to `0`.
#' @param max_age *\[numeric|NULL\]* The requested maximum age in seconds.
#' @return *\[numeric\]* A single, sanitised TTL in seconds.
#' @keywords internal
resolve_max_age <- function(max_age) {
  if (is.null(max_age)) {
    max_age <- getOption("artma.cache.max_age", 3600)
  }
  if (!is.numeric(max_age) || length(max_age) != 1L || is.na(max_age)) {
    return(Inf)
  }
  if (max_age < 0) {
    return(0)
  }
  max_age
}

#' @title Create the default on-disk cache
#' @description Build a filesystem cache rooted at `PATHS$DIR_USR_CACHE`,
#'   creating the directory if necessary.
#' @return *\[memoise::cache_filesystem\]* A filesystem-backed cache.
#' @keywords internal
default_cache <- function() {
  box::use(artma / paths[PATHS])
  cache_dir <- PATHS$DIR_USR_CACHE
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  memoise::cache_filesystem(cache_dir)
}

#' @title Evaluate a cache invalidation predicate
#' @description Run `invalidate_fun` against the call arguments, treating any
#'   error as "do not invalidate" so a faulty predicate can never crash a call.
#' @param invalidate_fun *\[function\]* The predicate to evaluate.
#' @param dots *\[list\]* The arguments captured from the current call.
#' @return *\[logical\]* `TRUE` when the cache should be bypassed.
#' @keywords internal
should_invalidate <- function(invalidate_fun, dots) {
  tryCatch(
    isTRUE(rlang::exec(invalidate_fun, !!!dots)),
    error = function(err) {
      cli::cli_warn(
        sprintf("cache invalidator failed and was ignored: %s", conditionMessage(err))
      )
      FALSE
    }
  )
}

#' @title Test whether a cached artifact has expired
#' @description Compare an artifact's timestamp against a TTL.
#' @param art *\[cached_artifact\]* The artifact to test.
#' @param max_age *\[numeric\]* The TTL in seconds.
#' @return *\[logical\]* `TRUE` when the artifact is older than `max_age`.
#' @keywords internal
artifact_expired <- function(art, max_age) {
  age <- tryCatch(
    as.numeric(difftime(Sys.time(), art$meta$timestamp, units = "secs")),
    error = function(err) NA_real_
  )
  !is.na(age) && age > max_age
}

#' @title Test whether a cached artifact's output files are gone
#' @description Report whether any file the artifact recorded when it was
#'   produced has since disappeared. Runtime methods write graphics during
#'   execution, so replaying only the returned value would leave a run claiming
#'   success with its plots missing.
#' @param art *\[cached_artifact\]* The artifact to test.
#' @return *\[logical\]* `TRUE` when a recorded output file is missing.
#' @keywords internal
artifact_files_missing <- function(art) {
  box::use(
    artma / libs / infrastructure / output_files[recorded_output_files_missing]
  )
  recorded_output_files_missing(art$meta$output_files)
}

#' @title Announce a cache hit
#' @description Print a single short notice when cached results are reused,
#'   respecting the configured verbosity level.
#' @param extra_keys *\[list\]* Key material; a `stage` entry names the workflow
#'   in the notice.
#' @return `NULL`, invisibly.
#' @keywords internal
notify_cache_hit <- function(extra_keys) {
  box::use(artma / libs / core / utils[get_verbosity])
  if (get_verbosity() < 3) {
    return(invisible(NULL))
  }
  stage <- if (is.list(extra_keys)) extra_keys$stage else NULL
  if (is.character(stage) && length(stage) == 1L && nzchar(stage)) {
    cli::cli_alert_info("Using cached results for {stage}")
  } else {
    cli::cli_alert_info("Using cached results")
  }
  invisible(NULL)
}

#' @title Cache a function on disk
#' @description Wrap a function so its results are memoised via `memoise`. On a
#'   cache hit a short notice is printed instead of rerunning the function.
#' @param fun *\[function\]* The function to cache.
#' @param extra_keys *\[list\]* Additional key material stored in the artifact
#'   metadata. A `stage` entry, when present, names the workflow in the cache
#'   hit notice.
#' @param cache *\[memoise::cache_filesystem\]* The cache to use. Defaults to a
#'   filesystem cache under `PATHS$DIR_USR_CACHE`.
#' @param invalidate_fun *\[function\]* Optional predicate evaluated on each
#'   call to decide whether the cached value should be dropped and recomputed.
#' @param max_age *\[numeric\]* Maximum age of cached artifacts in seconds. Use
#'   `Inf` to disable time-based invalidation. When `NULL` (the default) the
#'   value is sourced from the `artma.cache.max_age` option, falling back to
#'   3600 seconds (1 hour). Time-based invalidation is a backstop rather than
#'   the primary guard: cache keys cover the inputs, the options, and the
#'   package source, so the TTL only catches inputs that changed without the
#'   signature noticing (an edited data file whose mtime was preserved, say).
#' @return *\[function\]* The wrapped function. It consults
#'   `artma.cache.use_cache` on every call, so disabling caching mid-session
#'   takes effect immediately and calls through to `fun` unchanged.
#' @examples
#' \dontrun{
#' .run_models_impl <- function(df, formula, seed = 123) {
#'   set.seed(seed)
#'   stats::lm(formula, data = df)
#' }
#'
#' run_models <- cache_cli(
#'   .run_models_impl,
#'   extra_keys = list(pkg_ver = utils::packageVersion("artma"))
#' )
#' }
cache_cli <- function(fun,
                      extra_keys = list(),
                      cache = NULL,
                      invalidate_fun = NULL,
                      max_age = NULL) {
  box::use(
    artma / libs / infrastructure / output_files[
      begin_output_file_capture, end_output_file_capture
    ]
  )

  base::force(fun) # lock the original function inside the closure

  requested_max_age <- max_age
  supplied_cache <- cache

  # Tracks whether the worker actually ran, so the wrapper can tell a genuine
  # cache hit (worker skipped by memoise) from a cold computation. It also
  # carries the TTL in force for the current call into the worker.
  worker_state <- new.env(parent = emptyenv())
  worker_state$max_age <- resolve_max_age(requested_max_age)

  worker <- function(...) {
    worker_state$executed <- TRUE

    # Bracket the run so any file the implementation writes (graphics, in
    # practice) is recorded on the artifact and can be re-checked on a hit.
    capture_id <- begin_output_file_capture()
    on.exit(end_output_file_capture(capture_id), add = TRUE)
    value <- fun(...)
    output_files <- end_output_file_capture(capture_id)

    meta <- list(
      timestamp = Sys.time(),
      extra = extra_keys,
      output_files = output_files,
      cache = list(max_age = worker_state$max_age)
    )
    new_artifact(value, meta)
  }

  # The memoised layer is built on first use rather than at wrap time: runtime
  # methods are wrapped when their module loads, which is well before the
  # options file has settled, and building it lazily also keeps the cache
  # directory from being created for sessions that never cache anything.
  memo_state <- new.env(parent = emptyenv())

  resolve_memoised <- function() {
    if (is.null(memo_state$memoised)) {
      resolved_cache <- if (is.null(supplied_cache)) default_cache() else supplied_cache
      memo_state$memoised <- memoise::memoise(worker, cache = resolved_cache)
      memo_state$drop <- memoise::drop_cache(memo_state$memoised)
    }
    memo_state
  }

  function(...) {
    dots <- rlang::list2(...)

    # Read at call time so toggling `artma.cache.use_cache` takes effect
    # immediately, rather than being frozen at module load.
    if (!getOption("artma.cache.use_cache", TRUE)) {
      return(rlang::exec(fun, !!!dots))
    }

    max_age <- resolve_max_age(requested_max_age)
    worker_state$max_age <- max_age
    worker_state$executed <- FALSE

    memo <- resolve_memoised()

    if (is.function(invalidate_fun) && should_invalidate(invalidate_fun, dots)) {
      tryCatch(
        memoise::forget(memo$memoised),
        error = function(err) {
          cli::cli_warn(
            sprintf("cache invalidator failed to reset memoised state: %s", conditionMessage(err))
          )
        }
      )
    }

    art <- rlang::exec(memo$memoised, !!!dots)
    cache_hit <- !isTRUE(worker_state$executed)

    stale <- cache_hit &&
      ((is.finite(max_age) && artifact_expired(art, max_age)) || artifact_files_missing(art))

    if (stale) {
      tryCatch(
        rlang::exec(memo$drop, !!!dots),
        error = function(err) {
          cli::cli_warn(
            sprintf("Failed to evict stale cache entry: %s", conditionMessage(err))
          )
        }
      )
      worker_state$executed <- FALSE
      art <- rlang::exec(memo$memoised, !!!dots)
      cache_hit <- !isTRUE(worker_state$executed)
    }

    if (cache_hit) {
      notify_cache_hit(extra_keys)
    }

    art$value
  }
}

#' @title Create a reusable cache_cli-backed runner
#' @description
#' Build a memoised wrapper around an implementation function while keeping the
#' calling surface free from cache-related boilerplate. The helper introduces a
#' `cache_signature` argument in the memoised layer so additional cache key
#' components can be injected via `key_builder()` without requiring the
#' underlying implementation to manage that parameter.
#' @param fun *\[function\]* The function to wrap. It may optionally accept a
#'   `cache_signature` argument.
#' @param stage *\[character\]* Optional stage label appended to the cached
#'   artifact metadata and used in the cache hit notice.
#' @param key_builder *\[function\]* Optional function invoked on every call
#'   with the same arguments passed to the runner. Its return value is provided
#'   to `cache_cli()` as the `cache_signature` argument.
#' @param extra_keys *\[list\]* Additional metadata entries merged with the
#'   stage identifier.
#' @inheritParams cache_cli
#' @return *\[function\]* A callable wrapper that proxies to `fun()` and
#'   memoises results through `cache_cli()`.
#' @examples
#' build_signature <- function(data) list(rows = nrow(data))
#' slow_identity <- function(data) data
#'
#' cached <- cache_cli_runner(
#'   slow_identity,
#'   stage = "demo",
#'   key_builder = build_signature,
#'   cache = memoise::cache_memory()
#' )
#'
#' cached(iris) # <- first call computes
#' cached(iris) # <- subsequent call reuses the cached result
cache_cli_runner <- function(fun,
                             stage = NULL,
                             key_builder = NULL,
                             extra_keys = list(),
                             cache = NULL,
                             invalidate_fun = NULL,
                             max_age = NULL) {
  if (!is.function(fun)) {
    cli::cli_abort("`fun` must be a function.")
  }

  if (!is.null(key_builder) && !is.function(key_builder)) {
    cli::cli_abort("`key_builder` must be a function when supplied.")
  }

  fun_formals <- tryCatch(formals(fun), error = function(err) NULL)
  accepts_signature <- isTRUE("cache_signature" %in% names(fun_formals)) ||
    isTRUE("..." %in% names(fun_formals))

  extra_keys <- if (is.null(extra_keys)) list() else as.list(extra_keys)
  if (!is.null(stage)) {
    extra_keys <- c(list(stage = stage), extra_keys)
  }

  base_impl <- function(cache_signature = NULL, ...) {
    inner_dots <- rlang::list2(...)

    if (accepts_signature) {
      return(rlang::exec(fun, cache_signature = cache_signature, !!!inner_dots))
    }

    rlang::exec(fun, !!!inner_dots)
  }

  cached_impl <- cache_cli(
    base_impl,
    extra_keys = extra_keys,
    cache = cache,
    invalidate_fun = invalidate_fun,
    max_age = max_age
  )

  function(...) {
    dots <- rlang::list2(...)
    cache_signature <- NULL

    if (!is.null(key_builder)) {
      cache_signature <- tryCatch(
        rlang::exec(key_builder, !!!dots),
        error = function(err) {
          cli::cli_abort(
            sprintf("cache signature builder failed: %s", conditionMessage(err))
          )
        }
      )
    }

    if (!is.null(stage)) {
      stage_signature <- list(stage = stage)

      if (is.null(cache_signature)) {
        cache_signature <- stage_signature
      } else {
        if (!is.list(cache_signature)) {
          cache_signature <- list(value = cache_signature)
        }
        cache_signature <- c(stage_signature, cache_signature)
      }
    }

    rlang::exec(cached_impl, cache_signature = cache_signature, !!!dots)
  }
}

#' @title Get artifact
#' @description Read the value stored under a cache key. Intended for
#'   inspecting the on-disk cache during debugging.
#' @param cache *\[memoise::cache_filesystem\]* The cache to read from.
#' @param key *\[character\]* The key to read.
#' @return *\[any\]* The cached value.
#' @examples
#' \dontrun{
#' cache <- memoise::cache_filesystem(rappdirs::user_cache_dir("artma"))
#' keys <- cache$keys()
#' get_artifact(cache, keys[[1]])
#' }
get_artifact <- function(cache, key) {
  cache$get(key)$value
}

box::export(
  cache_cli,
  cache_cli_runner,
  get_artifact,
  new_artifact,
  print.cached_artifact
)
