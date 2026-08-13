#' @title Per-run manifest
#' @description
#' A run leaves a `run.json` in its output directory describing exactly what it
#' did: when it ran, which options file and data source it used, which methods
#' were requested, ran, were skipped or failed, the effective seed, and the
#' files it wrote.
#'
#' The file list comes from the output-file recorder
#' (`artma/libs/infrastructure/output_files`), not from listing the directory,
#' so a manifest never claims files that an earlier run left behind. That is
#' also what makes it usable as the report's plot index: `method_files` maps
#' each method to the paths it actually wrote, so the report no longer has to
#' guess a method's graphics from their file names.
#'
#' ## Multi-run policy
#'
#' `run.json` is overwritten on every run: it always describes the latest run
#' into that output directory, never a history. Runs driven by different
#' options files already resolve to different output directories (see
#' `auto_output_subdir()` in `artma/output/export`), so the common case of
#' several analyses side by side keeps one manifest each. Keep a run by copying
#' its output directory, or point `output.dir` somewhere per-run.

box::use(
  artma / libs / core / utils[get_verbosity]
)

RUN_MANIFEST_FILENAME <- "run.json"

#' @title Path of the manifest inside an output directory
#' @param output_dir *\[character\]* The run's output directory.
#' @return *\[character\]* Path of the `run.json` file.
run_manifest_path <- function(output_dir) {
  file.path(output_dir, RUN_MANIFEST_FILENAME)
}

#' @title Format a timestamp for the manifest
#' @param when *\[POSIXct\]* The time to format.
#' @return *\[character\]* An ISO-8601 timestamp, or `NULL` when unusable.
#' @keywords internal
manifest_timestamp <- function(when) {
  if (is.null(when) || length(when) != 1L || is.na(when)) {
    return(NULL)
  }
  tryCatch(
    format(when, "%Y-%m-%dT%H:%M:%S%z"),
    error = function(e) NULL
  )
}

#' @title Normalise a path for the manifest
#' @description Manifest paths are absolute, so a consumer (the report, the
#'   CLI) can use them without knowing the working directory the run had.
#'   `normalizePath()` leaves a path that does not exist untouched, which on a
#'   platform whose temporary or home directory is a symlink would make two
#'   spellings of the same location look unrelated. The longest existing
#'   ancestor is resolved instead, and the rest appended, so a file and the
#'   directory it sits in always agree.
#' @param path *\[character\]* Paths to normalise.
#' @return *\[character\]* Absolute paths.
#' @keywords internal
manifest_path <- function(path) {
  if (length(path) == 0L) {
    return(character())
  }

  resolve_one <- function(p) {
    if (is.na(p) || !nzchar(p)) {
      return(p)
    }
    p <- path.expand(p)
    tail_parts <- character()
    current <- p

    repeat {
      if (file.exists(current)) {
        resolved <- tryCatch(
          normalizePath(current, mustWork = FALSE),
          error = function(e) current
        )
        if (length(tail_parts) == 0L) {
          return(resolved)
        }
        return(paste(c(resolved, rev(tail_parts)), collapse = "/"))
      }
      parent <- dirname(current)
      if (identical(parent, current)) {
        return(p)
      }
      tail_parts <- c(tail_parts, basename(current))
      current <- parent
    }
  }

  vapply(as.character(path), resolve_one, character(1), USE.NAMES = FALSE)
}

#' @title Test whether a path sits inside a directory
#' @param paths *\[character\]* Normalised paths to test.
#' @param dir *\[character\]* The directory (normalised).
#' @return *\[logical\]* One flag per path.
#' @keywords internal
is_inside_dir <- function(paths, dir) {
  if (length(paths) == 0L) {
    return(logical())
  }
  if (is.null(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    return(rep(FALSE, length(paths)))
  }
  prefix <- paste0(sub("/+$", "", dir), "/")
  startsWith(paths, prefix)
}

#' @title Split a run's files into tables, graphics and everything else
#' @description Classification is by location: the `tables` subdirectory of the
#'   output directory, the resolved graphics directory, and the remainder (the
#'   HTML report, interactive plot widgets written elsewhere).
#' @param files *\[character\]* Paths recorded during the run.
#' @param output_dir *\[character\]* The run's output directory.
#' @return *\[list\]* Named list with `tables`, `graphics` and `other`.
#' @keywords internal
classify_run_files <- function(files, output_dir) {
  box::use(artma / output / export[resolve_graphics_dir])

  files <- unique(manifest_path(files))
  empty <- list(tables = character(), graphics = character(), other = character())
  if (length(files) == 0L) {
    return(empty)
  }

  tables_dir <- manifest_path(file.path(output_dir, "tables"))
  graphics_dir <- manifest_path(resolve_graphics_dir(output_dir))

  in_tables <- is_inside_dir(files, tables_dir)
  in_graphics <- !in_tables & is_inside_dir(files, graphics_dir)

  # `I()` keeps a one-element group an array in the written JSON, so a consumer
  # never has to handle both a string and a list of strings.
  list(
    tables = I(sort(files[in_tables])),
    graphics = I(sort(files[in_graphics])),
    other = I(sort(files[!in_tables & !in_graphics]))
  )
}

#' @title Coerce a named character vector to a JSON-friendly named list
#' @param x *\[character or list\]* Values keyed by method name.
#' @return *\[list\]* A named list of single strings (empty list when absent).
#' @keywords internal
as_reason_map <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(stats::setNames(list(), character(0)))
  }
  lapply(as.list(x), function(value) as.character(value)[[1L]])
}

#' @title Build a run manifest
#' @description Assemble the record of a single run from its results and the
#'   files it wrote. Everything here is read from the run itself (the results
#'   object and the loaded options); nothing is inferred from the output
#'   directory's contents.
#' @param results *\[list\]* The value returned by `invoke_runtime_methods()`,
#'   carrying the `run_info` attribute (requested methods, effective seed,
#'   files written per method) alongside `skipped_methods` and
#'   `failed_methods`.
#' @param output_dir *\[character\]* The run's output directory.
#' @param run_files *\[character, optional\]* Paths recorded outside the
#'   methods themselves (exported tables, the HTML report).
#' @param timestamp *\[POSIXct, optional\]* The run's end time.
#' @return *\[list\]* The manifest, ready for `jsonlite::toJSON()`.
build_run_manifest <- function(results,
                               output_dir,
                               run_files = character(),
                               timestamp = Sys.time()) {
  run_info <- attr(results, "run_info")
  if (!is.list(run_info)) {
    run_info <- list()
  }

  method_files <- run_info$output_files
  if (!is.list(method_files)) {
    method_files <- list()
  }
  method_files <- lapply(method_files, function(paths) unique(manifest_path(paths)))
  method_files <- method_files[vapply(method_files, length, integer(1)) > 0L]

  all_files <- c(as.character(run_files), unlist(method_files, use.names = FALSE))
  method_files <- lapply(method_files, I)

  source_path <- getOption("artma.data.source_path", NULL)
  source_path <- if (is.null(source_path)) NULL else as.character(source_path)[[1L]]
  source_mtime <- NULL
  if (!is.null(source_path) && file.exists(source_path)) {
    source_mtime <- manifest_timestamp(file.info(source_path)$mtime)
  }

  requested <- run_info$methods_requested
  seed <- run_info$seed

  list(
    timestamp = manifest_timestamp(timestamp),
    artma_version = tryCatch(
      as.character(utils::packageVersion("artma")),
      error = function(e) NULL
    ),
    options_file = getOption("artma.temp.file_name", NULL),
    options_dir = getOption("artma.temp.dir_name", NULL),
    data_source_path = source_path,
    data_source_mtime = source_mtime,
    seed = if (is.null(seed)) NULL else as.numeric(seed)[[1L]],
    output_dir = manifest_path(output_dir),
    methods_requested = I(as.character(requested %||% character())),
    methods_run = I(as.character(setdiff(names(results), "ma_table"))),
    methods_skipped = as_reason_map(attr(results, "skipped_methods")),
    methods_failed = as_reason_map(attr(results, "failed_methods")),
    files = classify_run_files(all_files, output_dir),
    method_files = method_files
  )
}

#' @title Write the run manifest into an output directory
#' @description Serialise the manifest as `run.json`, overwriting any manifest
#'   an earlier run left there. Writing is best-effort: a run that produced
#'   results must not fail because its record could not be written, so problems
#'   are reported at verbosity >= 2 and swallowed.
#' @inheritParams build_run_manifest
#' @return *\[character or NULL\]* The path written, invisibly, or `NULL` when
#'   the manifest could not be written.
write_run_manifest <- function(results,
                               output_dir,
                               run_files = character(),
                               timestamp = Sys.time()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning(
        "Skipping the run manifest: install {.pkg jsonlite} to write {.file run.json}."
      )
    }
    return(invisible(NULL))
  }

  path <- run_manifest_path(output_dir)

  tryCatch(
    {
      manifest <- build_run_manifest(
        results,
        output_dir = output_dir,
        run_files = run_files,
        timestamp = timestamp
      )
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      writeLines(
        jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null"),
        path,
        useBytes = TRUE
      )
      if (get_verbosity() >= 4) {
        cli::cli_alert_success("Run manifest written to {.path {path}}")
      }
      invisible(path)
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("Failed to write the run manifest: {e$message}")
      }
      invisible(NULL)
    }
  )
}

#' @title Read the run manifest from an output directory
#' @param output_dir *\[character or NULL\]* The output directory to read from.
#' @return *\[list or NULL\]* The parsed manifest, or `NULL` when there is none
#'   (or it cannot be parsed).
read_run_manifest <- function(output_dir) {
  if (is.null(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
    return(NULL)
  }
  path <- run_manifest_path(output_dir)
  if (!file.exists(path) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(NULL)
  }
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = TRUE),
    error = function(e) NULL
  )
}

#' @title Flatten a manifest's file list
#' @param manifest *\[list or NULL\]* A manifest from `read_run_manifest()`.
#' @return *\[character\]* Every file the run recorded, tables first.
manifest_files <- function(manifest) {
  files <- manifest$files
  if (!is.list(files)) {
    return(character())
  }
  unique(as.character(unlist(files[c("tables", "graphics", "other")], use.names = FALSE)))
}

#' @title Build the report's plot index from recorded files
#' @description Map each method to the PNG files it wrote, dropping any that
#'   have since disappeared. Accepts either a manifest read back from disk or
#'   the in-memory `output_files` attribute of a live run, so the report can be
#'   rendered during a run and after it from the same code path.
#' @param x *\[list or NULL\]* A manifest, or a named list of paths per method.
#' @return *\[list\]* Named list of existing PNG paths, one entry per method.
manifest_plot_index <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  per_method <- if (is.list(x) && !is.null(x$method_files)) x$method_files else x
  if (!is.list(per_method) || length(per_method) == 0L) {
    return(list())
  }

  index <- lapply(per_method, function(paths) {
    paths <- as.character(unlist(paths, use.names = FALSE))
    paths <- paths[grepl("\\.png$", paths, ignore.case = TRUE)]
    paths <- paths[file.exists(paths)]
    sort(unique(paths))
  })

  index[vapply(index, length, integer(1)) > 0L]
}

box::export(
  RUN_MANIFEST_FILENAME,
  build_run_manifest,
  classify_run_files,
  manifest_files,
  manifest_plot_index,
  read_run_manifest,
  run_manifest_path,
  write_run_manifest
)
