#' @title Disk cache for the Elliott LCM critical-value simulation
#' @description
#' The LCM p-hacking test compares its statistic against quantiles of a
#' simulated null distribution produced by `simulate_cdfs_parallel()`. That
#' table depends only on the iteration count, the grid size, the seed, the
#' RNG kind, and the simulation implementation itself, never on the analysed
#' data. The method-level cache from `register_runtime_method()` keys on the
#' data source and the whole user option group, so any data or option change
#' would re-run the simulation. This module caches the table under its own
#' narrow key instead.
NULL

box::use(
  artma / calc / methods / elliott[simulate_cdfs_block_cpp, simulate_cdfs_parallel],
  artma / libs / infrastructure / cache[cache_cli],
  artma / libs / infrastructure / source_fingerprint[hash_source_files],
  artma / paths[PATHS]
)

STAGE <- "lcm_critical_values"

# Resolved at module load time: the directory holding this module and the
# simulation implementation it fingerprints.
MODULE_DIR <- box::file()

#' @title Decide whether the compiled simulation backend will run
#' @description Mirror the backend selection inside
#'   `simulate_cdfs_parallel()`, without its fallback warning: the option gate
#'   first, then a probe of the compiled routine. The C++ path and the R
#'   fallback can differ per draw, so the resolved backend has to be part of
#'   the cache key.
#' @return *\[logical\]* `TRUE` when the C++ implementation will run.
#' @keywords internal
uses_cpp_backend <- function() {
  if (!isTRUE(getOption("artma.methods.p_hacking_tests.simulate_cdfs.use_cpp", TRUE))) {
    return(FALSE)
  }
  tryCatch(
    {
      simulate_cdfs_block_cpp(matrix(0, nrow = 1, ncol = 1))
      TRUE
    },
    error = function(e) FALSE
  )
}

#' @title Fingerprint the simulation implementation
#' @description Hash the two source files that define the simulation (the R
#'   module and the C++ kernel) together with the package version, so a fix to
#'   the simulation invalidates previously cached tables. Each file is hashed
#'   against its own directory, keeping the fingerprint independent of the
#'   checkout location. The C++ source does not ship with installed packages;
#'   the package version stands in for it there.
#' @return *\[list\]* Named hash components for the cache key.
#' @keywords internal
implementation_fingerprint <- function() {
  r_source <- file.path(MODULE_DIR, "elliott.R")
  cpp_source <- file.path(dirname(PATHS$PACKAGE_PATH), "src", "elliott_cdfs.cpp")
  list(
    r = hash_source_files(r_source, root = dirname(r_source)),
    cpp = if (file.exists(cpp_source)) {
      hash_source_files(cpp_source, root = dirname(cpp_source))
    } else {
      NA_character_
    },
    package_version = as.character(utils::packageVersion("artma"))
  )
}

#' @title Read the session RNG state
#' @return *\[integer|NULL\]* The current `.Random.seed`, or `NULL` when the
#'   session has not used the RNG yet.
#' @keywords internal
current_rng_state <- function() {
  if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    return(NULL)
  }
  get(".Random.seed", envir = globalenv(), inherits = FALSE)
}

#' @title Restore a previously captured session RNG state
#' @param state *\[integer|NULL\]* A `.Random.seed` vector, or `NULL` to leave
#'   the session state untouched.
#' @return `NULL`, invisibly.
#' @keywords internal
restore_rng_state <- function(state) {
  if (!is.null(state)) {
    assign(".Random.seed", state, envir = globalenv()) # nolint: object_name_linter.
  }
  invisible(NULL)
}

#' @title Build a cached variant of `simulate_cdfs_parallel()`
#' @description
#' Wrap `sim_fun` in a disk cache keyed narrowly on the inputs that determine
#' the simulated table: iteration count, grid size, seed, RNG kind, the
#' resolved backend, and the implementation fingerprint. Scheduling arguments
#' (workers, block size, progress display) cannot change the result, so they
#' stay out of the key and are forwarded out of band.
#'
#' A `NULL` seed deliberately consumes the caller's RNG state and is never
#' cached. `options(artma.cache.use_cache = FALSE)` bypasses the cache like
#' everywhere else in the cache layer; `cache_cli()` reads it on every call.
#'
#' A cold run stores, next to the simulated values, the RNG state the
#' simulation left behind; a cache hit restores that state. Downstream draws
#' therefore do not depend on whether the cache was warm.
#' @param cache *\[cachem cache, optional\]* Cache to store results in.
#'   Defaults to the shared filesystem cache under `PATHS$DIR_USR_CACHE`.
#' @param sim_fun *\[function, optional\]* The simulation implementation,
#'   injectable for tests. Defaults to `simulate_cdfs_parallel`.
#' @return *\[function\]* A drop-in replacement for
#'   `simulate_cdfs_parallel()`.
make_simulate_cdfs_cached <- function(cache = NULL, sim_fun = simulate_cdfs_parallel) {
  # Scheduling arguments travel to the worker through this environment rather
  # than as arguments: `memoise` hashes every argument of the memoised call,
  # and these must not fragment the cache key.
  scheduling <- new.env(parent = emptyenv())

  simulate_for_key <- function(key) {
    values <- sim_fun(
      iterations = key$iterations,
      grid_points = key$grid_points,
      workers = scheduling$workers,
      block_size = scheduling$block_size,
      show_progress = scheduling$show_progress,
      seed = key$seed
    )
    list(values = values, rng_state = current_rng_state())
  }

  cached_simulate <- cache_cli(
    simulate_for_key,
    extra_keys = list(stage = STAGE),
    cache = cache,
    # The key covers every input the result depends on, so the TTL backstop
    # for silently edited data files does not apply here.
    max_age = Inf
  )

  function(iterations = 10000,
           grid_points = 10000,
           workers = NULL,
           block_size = 256L,
           show_progress = TRUE,
           seed = NULL) {
    if (is.null(seed)) {
      # A NULL seed consumes the caller's RNG state; serving those draws from
      # cache would silently repeat them.
      return(sim_fun(
        iterations = iterations,
        grid_points = grid_points,
        workers = workers,
        block_size = block_size,
        show_progress = show_progress,
        seed = NULL
      ))
    }

    scheduling$workers <- workers
    scheduling$block_size <- block_size
    scheduling$show_progress <- show_progress

    key <- list(
      stage = STAGE,
      iterations = as.integer(iterations),
      grid_points = as.integer(grid_points),
      seed = as.integer(seed),
      rng_kind = RNGkind(),
      use_cpp = uses_cpp_backend(),
      implementation = implementation_fingerprint()
    )

    result <- cached_simulate(key)
    restore_rng_state(result$rng_state)
    result$values
  }
}

# The production instance backing `run_p_hacking_tests()`, built once at
# module load so the memoised layer persists for the session.
simulate_cdfs_cached <- make_simulate_cdfs_cached()

box::export(
  make_simulate_cdfs_cached,
  simulate_cdfs_cached
)
