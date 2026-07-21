# Layered, optionally parallel execution of runtime methods.
#
# The orchestrator in `R/artma.R` topologically sorts methods by their declared
# `depends_on` metadata. This module turns that order into dependency *layers*
# (every method in a layer has all of its in-graph dependencies satisfied by
# earlier layers) and runs each layer either sequentially or concurrently via
# `parallel::mclapply`.
#
# Two properties are preserved regardless of how many workers are used:
#
#   * CLI output is captured per method and replayed in discovery order after
#     the layer joins, so forked output never interleaves.
#   * Each method runs on a pre-assigned L'Ecuyer-CMRG stream, so a run is
#     reproducible given the seed whether it executed in parallel or not.

box::use(
  artma / libs / core / autonomy[get_autonomy_level, get_default_autonomy_level],
  artma / libs / core / validation[assert]
)

#' @title Group methods into dependency layers
#' @description
#' Split `method_names` into successive layers such that every method in a
#' layer depends only on methods placed in earlier layers. Dependencies
#' pointing outside `method_names` are ignored (the dependent runs standalone),
#' matching `topo_sort_methods()`. Within a layer the input order is preserved.
#' Aborts when the dependency graph contains a cycle.
#' @param method_names *\[character\]* Methods to group, in their base order.
#' @param deps *\[list, optional\]* Named list mapping a method name to the
#'   character vector of methods it depends on.
#' @return *\[list\]* A list of character vectors, one per layer.
group_methods_into_layers <- function(method_names, deps = list()) {
  method_names <- as.character(method_names)
  if (length(method_names) == 0L) {
    return(list())
  }

  layers <- list()
  placed <- character()
  remaining <- method_names

  while (length(remaining) > 0L) {
    ready <- vapply(
      remaining,
      function(name) {
        prereqs <- intersect(deps[[name]] %||% character(), method_names)
        all(prereqs %in% placed)
      },
      logical(1)
    )

    if (!any(ready)) {
      cli::cli_abort(c(
        "x" = "Cyclic method dependencies detected.",
        "i" = "Methods involved in the cycle: {.val {remaining}}"
      ))
    }

    layers[[length(layers) + 1L]] <- remaining[ready]
    placed <- c(placed, remaining[ready])
    remaining <- remaining[!ready]
  }

  layers
}

#' @title Hard ceiling on simultaneous forked workers
#' @description
#' `R CMD check` refuses a run that spawns more than two simultaneous processes
#' (it sets `_R_CHECK_LIMIT_CORES_`), and `mc.cores` is the conventional way for
#' a user to cap concurrency. Both are respected here so a layer never spawns
#' more workers than the environment allows.
#' @return *\[numeric\]* The ceiling, or `Inf` when unconstrained.
#' @keywords internal
max_parallel_workers <- function() {
  check_limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(check_limit) && !identical(tolower(check_limit), "false")) {
    return(2L)
  }

  mc_cores <- getOption("mc.cores", NULL)
  if (is.numeric(mc_cores) && length(mc_cores) == 1L && !is.na(mc_cores)) {
    return(max(1L, as.integer(mc_cores)))
  }

  Inf
}

#' @title Number of workers to use for a layer
#' @description
#' Decide how many forked workers a layer of `n_tasks` methods may use. Returns
#' `1` (sequential execution) whenever forking is unavailable or unsafe:
#' on Windows, on single-core machines, when the `artma.general.parallel`
#' option is `FALSE`, or in an interactive session whose autonomy level still
#' allows methods to prompt (forked children cannot prompt).
#' @param n_tasks *\[integer\]* Number of methods in the layer.
#' @param is_interactive *\[logical, optional\]* Whether the session is
#'   interactive. Injectable for testing; defaults to `interactive()`.
#' @param os_type *\[character, optional\]* Platform OS type. Injectable for
#'   testing; defaults to `.Platform$OS.type`.
#' @param n_cores *\[integer, optional\]* Detected core count. Injectable for
#'   testing; defaults to `parallel::detectCores()`.
#' @param max_workers *\[numeric, optional\]* Hard ceiling on the worker count.
#'   Injectable for testing; defaults to `max_parallel_workers()`.
#' @return *\[integer\]* The number of workers, at least `1`.
resolve_worker_count <- function(n_tasks,
                                 is_interactive = NULL,
                                 os_type = NULL,
                                 n_cores = NULL,
                                 max_workers = NULL) {
  n_tasks <- as.integer(n_tasks)
  if (is.na(n_tasks) || n_tasks < 2L) {
    return(1L)
  }

  if (!isTRUE(getOption("artma.general.parallel", TRUE))) {
    return(1L)
  }

  os_type <- os_type %||% .Platform$OS.type
  if (identical(os_type, "windows")) {
    return(1L)
  }

  is_interactive <- is_interactive %||% interactive()
  autonomy_level <- get_autonomy_level() %||% get_default_autonomy_level()
  if (isTRUE(is_interactive) && !identical(autonomy_level, "autonomous")) {
    # Methods may still prompt at these autonomy levels, and a forked child
    # cannot read from the console.
    return(1L)
  }

  if (is.null(n_cores)) {
    n_cores <- tryCatch(parallel::detectCores(), error = function(err) NA_integer_)
  }
  if (!is.numeric(n_cores) || length(n_cores) != 1L || is.na(n_cores) || n_cores < 2L) {
    return(1L)
  }

  max_workers <- max_workers %||% max_parallel_workers()

  as.integer(min(n_tasks, max(1L, as.integer(n_cores) - 1L), max_workers))
}

#' @title Pre-assign an RNG stream to every method
#' @description
#' Derive one independent L'Ecuyer-CMRG stream per method from `seed`, so a
#' stochastic method draws the same numbers whether it ran in a fork or in the
#' parent process. The caller's RNG kind and seed are restored before
#' returning.
#' @param method_names *\[character\]* Methods to assign streams to.
#' @param seed *\[integer\]* The run seed.
#' @return *\[list\]* A named list of `.Random.seed` vectors.
build_rng_streams <- function(method_names, seed) {
  method_names <- as.character(method_names)
  if (length(method_names) == 0L) {
    return(list())
  }

  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
  old_kind <- RNGkind()

  on.exit(
    {
      RNGkind(old_kind[[1L]], old_kind[[2L]], old_kind[[3L]])
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = globalenv()) # nolint: object_name_linter.
      } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      }
    },
    add = TRUE
  )

  RNGkind("L'Ecuyer-CMRG")
  set.seed(as.integer(seed))

  stream <- get(".Random.seed", envir = globalenv())
  streams <- vector("list", length(method_names))
  for (i in seq_along(method_names)) {
    streams[[i]] <- stream
    stream <- parallel::nextRNGStream(stream)
  }

  stats::setNames(streams, method_names)
}

#' @title Evaluate an expression with all of its CLI output captured
#' @description
#' Route everything `expr` prints into a single text connection, so nothing
#' reaches the console until the caller replays it: printed output via a sink,
#' and messages and warnings (which is how `cli` emits almost everything) via
#' calling handlers, keeping the two streams in the order they were produced.
#' `cli` normally strips colour when it detects a sink; the console's colour
#' count and width are pinned for the duration so the captured text keeps its
#' formatting when replayed.
#'
#' Warnings raised by `expr` are recorded as text rather than re-signalled: a
#' forked worker's warnings would otherwise surface at an arbitrary point in
#' the parent's output.
#' @param expr *\[any\]* The expression to evaluate. Errors are caught, not
#'   propagated.
#' @return *\[list\]* A list with `value`, `error` (a message or `NULL`), and
#'   `output` (a character vector of captured lines).
with_captured_output <- function(expr) {
  captured <- NULL
  con <- textConnection("captured", open = "w", local = TRUE)

  old_opts <- options(
    cli.num_colors = cli::num_ansi_colors(),
    cli.width = cli::console_width(),
    cli.dynamic = FALSE
  )

  base_out <- sink.number()
  on.exit(
    {
      while (sink.number() > base_out) sink() # nolint: undesirable_function_linter.
      options(old_opts)
    },
    add = TRUE
  )

  sink(con, type = "output") # nolint: undesirable_function_linter.

  result <- tryCatch(
    withCallingHandlers(
      list(value = expr, error = NULL),
      message = function(cond) {
        cat(conditionMessage(cond), file = con, sep = "") # nolint: undesirable_function_linter.
        invokeRestart("muffleMessage")
      },
      warning = function(cond) {
        cat(sprintf("Warning: %s\n", conditionMessage(cond)), file = con, sep = "") # nolint: undesirable_function_linter.
        invokeRestart("muffleWarning")
      }
    ),
    error = function(err) list(value = NULL, error = conditionMessage(err))
  )

  sink(type = "output") # nolint: undesirable_function_linter.
  close(con)

  list(value = result$value, error = result$error, output = captured %||% character())
}

#' @title Replay captured CLI output
#' @description Write previously captured lines to the console verbatim,
#'   preserving any ANSI formatting they carry.
#' @param output *\[character\]* Lines captured by `with_captured_output()`.
#' @return `NULL`, invisibly.
replay_captured_output <- function(output) {
  if (length(output) == 0L) {
    return(invisible(NULL))
  }
  cat(output, sep = "\n") # nolint: undesirable_function_linter.
  cat("\n") # nolint: undesirable_function_linter.
  invisible(NULL)
}

#' @title Run one layer of methods
#' @description
#' Execute every method in `method_names` through `run_one`, capturing output
#' and errors per method. With more than one worker the methods run
#' concurrently via `parallel::mclapply`; results come back in the input order
#' either way, so the caller can replay output and record outcomes in discovery
#' order.
#' @param method_names *\[character\]* Methods in this layer, in discovery order.
#' @param run_one *\[function\]* Called with a single method name; returns that
#'   method's result or throws.
#' @param streams *\[list, optional\]* Named list of RNG streams keyed by method
#'   name, as built by `build_rng_streams()`.
#' @param workers *\[integer, optional\]* Number of forked workers. `1` runs
#'   sequentially in the current process.
#' @return *\[list\]* One `list(value, error, output)` per method, in input
#'   order.
execute_method_layer <- function(method_names, run_one, streams = list(), workers = 1L) {
  assert(is.function(run_one), "`run_one` must be a function.")
  method_names <- as.character(method_names)
  if (length(method_names) == 0L) {
    return(list())
  }

  run_task <- function(method_name) {
    stream <- streams[[method_name]]
    if (!is.null(stream)) {
      assign(".Random.seed", stream, envir = globalenv()) # nolint: object_name_linter.
    }
    with_captured_output(run_one(method_name))
  }

  if (workers > 1L) {
    outcomes <- parallel::mclapply(
      method_names,
      run_task,
      mc.cores = workers,
      mc.preschedule = FALSE,
      mc.set.seed = TRUE
    )
    # A worker that dies outright (rather than signalling an R error) comes
    # back as a try-error; surface it through the same bookkeeping.
    outcomes <- lapply(outcomes, function(outcome) {
      if (inherits(outcome, "try-error") || !is.list(outcome)) {
        return(list(
          value = NULL,
          error = trimws(paste(as.character(outcome), collapse = " ")),
          output = character()
        ))
      }
      outcome
    })
    return(outcomes)
  }

  lapply(method_names, run_task)
}

box::export(
  build_rng_streams,
  execute_method_layer,
  group_methods_into_layers,
  max_parallel_workers,
  replay_captured_output,
  resolve_worker_count,
  with_captured_output
)
