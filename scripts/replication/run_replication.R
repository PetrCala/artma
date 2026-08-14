#!/usr/bin/env Rscript
# Replicate IES meta-analysis theses with artma and compare against the
# numbers their authors report.
#
# For each manifest this script:
#   1. resolves the dataset (downloading it once if a URL is given),
#   2. renames the thesis's columns onto artma's canonical names and writes a
#      normalised CSV -- this is what lets the run be fully non-interactive,
#      since artma never has to ask how to map columns,
#   3. generates a dedicated options file,
#   4. runs the manifest's methods,
#   5. joins artma's `estimates` frame against the transcribed author numbers,
#   6. writes a per-thesis CSV + markdown table, and a combined table.
#
# Usage:
#   Rscript scripts/replication/run_replication.R                 # every manifest
#   Rscript scripts/replication/run_replication.R --ids a,b       # a subset
#   Rscript scripts/replication/run_replication.R --refresh-data  # re-download
#
# Requires R >= 4.4 and an installed artma. The manifest and comparison layers
# have no such requirement -- see test_harness.R.

#' Directory of this script, whether it was run via Rscript or `source()`d.
script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  for (i in seq_len(sys.nframe())) {
    ofile <- get0("ofile", envir = sys.frame(i), inherits = FALSE)
    if (!is.null(ofile)) {
      return(dirname(normalizePath(ofile)))
    }
  }
  getwd()
}

here <- script_dir()
source(file.path(here, "lib", "manifest.R"))
source(file.path(here, "lib", "compare.R"))

MANIFEST_DIR <- file.path(here, "manifests")
DATA_DIR <- file.path(here, "data")
WORK_DIR <- file.path(here, "work")
OUT_DIR <- file.path(here, "output")

# --- Argument parsing ---------------------------------------------------------

parse_args <- function(argv) {
  args <- list(ids = NULL, refresh_data = FALSE)

  i <- 1L
  while (i <= length(argv)) {
    switch(argv[[i]],
      "--ids" = {
        i <- i + 1L
        args$ids <- strsplit(argv[[i]], ",", fixed = TRUE)[[1]]
      },
      "--refresh-data" = {
        args$refresh_data <- TRUE
      },
      stop(sprintf("Unknown argument: %s", argv[[i]]), call. = FALSE)
    )
    i <- i + 1L
  }

  args
}

# --- Data resolution ----------------------------------------------------------

#' Ensure the manifest's dataset is on disk, downloading it when needed.
resolve_data_file <- function(manifest, refresh) {
  dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(DATA_DIR, manifest$data$file)

  if (file.exists(destination) && !refresh) {
    verify_checksum(destination, manifest$data$sha256)
    return(destination)
  }

  url <- manifest$data$url
  if (is.null(url) || !nzchar(url)) {
    stop(sprintf(
      "[%s] no local data file at %s and no `data.url` to fetch it from.",
      manifest$id, destination
    ), call. = FALSE)
  }

  message(sprintf("  downloading %s", url))
  utils::download.file(url, destination, mode = "wb", quiet = TRUE)
  verify_checksum(destination, manifest$data$sha256)
  destination
}

#' Compare a file's SHA-256 against the manifest, when one is pinned.
verify_checksum <- function(path, expected) {
  if (is.null(expected) || is.na(expected) || !nzchar(expected)) {
    return(invisible(NULL))
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    warning("digest not installed; skipping checksum verification.", call. = FALSE)
    return(invisible(NULL))
  }
  actual <- digest::digest(path, algo = "sha256", file = TRUE)
  if (!identical(actual, expected)) {
    stop(sprintf(
      "Checksum mismatch for %s\n  expected: %s\n  actual:   %s",
      basename(path), expected, actual
    ), call. = FALSE)
  }
  invisible(NULL)
}

#' Read a dataset in whichever format the thesis published it.
read_dataset <- function(path, sheet = NULL) {
  extension <- tolower(tools::file_ext(path))

  reader <- switch(extension,
    csv = function() utils::read.csv(path, stringsAsFactors = FALSE),
    tsv = function() utils::read.delim(path, stringsAsFactors = FALSE),
    txt = function() utils::read.delim(path, stringsAsFactors = FALSE),
    rds = function() readRDS(path),
    xlsx = ,
    xls = function() {
      require_package("readxl", extension)
      as.data.frame(readxl::read_excel(path, sheet = sheet %||% 1))
    },
    dta = function() {
      require_package("haven", extension)
      as.data.frame(haven::read_dta(path))
    },
    stop(sprintf("Unsupported data format: .%s", extension), call. = FALSE)
  )

  reader()
}

require_package <- function(package, extension) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("Reading .%s files needs the %s package.", extension, package), call. = FALSE)
  }
}

#' Rename the thesis's columns onto artma's canonical names.
#'
#' Only the mapped columns are kept, so an unrelated column in the source file
#' cannot collide with a canonical name and silently shadow the mapping.
apply_column_mapping <- function(df, mapping) {
  missing <- setdiff(unlist(mapping, use.names = FALSE), names(df))
  if (length(missing) > 0L) {
    stop(sprintf(
      "Column(s) named in the manifest but absent from the data: %s\n  data has: %s",
      paste(missing, collapse = ", "), paste(names(df), collapse = ", ")
    ), call. = FALSE)
  }

  out <- df[, unlist(mapping, use.names = FALSE), drop = FALSE]
  names(out) <- names(mapping)
  out
}

# --- artma invocation ---------------------------------------------------------

#' Build a throwaway options file pointing artma at the normalised dataset.
prepare_options <- function(manifest, data_path) {
  options_name <- sprintf("replication_%s.yaml", manifest$id)

  artma::options_create(
    options_file_name = options_name,
    options_dir = WORK_DIR,
    should_overwrite = TRUE,
    user_input = list(
      "data.source_path" = data_path,
      "data.na_handling" = "stop",
      "output.dir" = file.path(OUT_DIR, manifest$id),
      "general.seed" = 20250101L,
      "autonomy.level" = "autonomous",
      "verbose" = 2L
    )
  )

  options_name
}

#' Run one manifest end to end, returning its comparison table.
run_one <- function(manifest, refresh_data) {
  message(sprintf("\n[%s] %s", manifest$id, manifest$title))

  source_path <- resolve_data_file(manifest, refresh_data)
  raw <- read_dataset(source_path, manifest$data$sheet)
  mapped <- apply_column_mapping(raw, manifest$columns)

  message(sprintf("  %d rows, %d mapped column(s)", nrow(mapped), ncol(mapped)))

  dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)
  normalised <- file.path(WORK_DIR, sprintf("%s.csv", manifest$id))
  utils::write.csv(mapped, normalised, row.names = FALSE)

  options_name <- prepare_options(manifest, normalised)

  results <- artma::artma(
    methods = unlist(manifest$methods, use.names = FALSE),
    options = options_name,
    options_dir = WORK_DIR
  )

  report_failures(manifest, results)

  estimates <- bind_estimates(results)
  write_estimates(manifest, estimates)

  compare_manifest(manifest, estimates)
}

#' Surface any method artma skipped or errored on -- a missing method silently
#' becomes a `not_produced` row otherwise, which reads as a failed replication
#' rather than a failed run.
report_failures <- function(manifest, results) {
  failed <- attr(results, "failed_methods")
  if (is.null(failed) || length(failed) == 0L) {
    return(invisible(NULL))
  }
  for (name in names(failed)) {
    message(sprintf("  ! method '%s' failed: %s", name, failed[[name]]))
  }
  invisible(NULL)
}

write_estimates <- function(manifest, estimates) {
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    estimates,
    file.path(OUT_DIR, sprintf("%s_artma_estimates.csv", manifest$id)),
    row.names = FALSE
  )
}

# --- Main ---------------------------------------------------------------------

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))

  if (!requireNamespace("artma", quietly = TRUE)) {
    stop(
      "artma is not installed. Install it (needs R >= 4.4 and CRAN access), ",
      "or run test_harness.R to exercise the artma-free layers.",
      call. = FALSE
    )
  }

  manifests <- read_manifests(MANIFEST_DIR, args$ids)
  message(sprintf("Replicating %d thesis/theses.", length(manifests)))

  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

  comparisons <- list()
  for (id in names(manifests)) {
    manifest <- manifests[[id]]
    comparison <- tryCatch(
      run_one(manifest, args$refresh_data),
      error = function(e) {
        message(sprintf("  ! %s", conditionMessage(e)))
        NULL
      }
    )

    if (is.null(comparison)) next

    comparisons[[id]] <- comparison
    write_thesis_report(manifest, comparison)
  }

  if (length(comparisons) == 0L) {
    message("\nNo thesis produced a comparison.")
    return(invisible(NULL))
  }

  combined <- do.call(rbind, c(unname(comparisons), list(make.row.names = FALSE)))
  utils::write.csv(combined, file.path(OUT_DIR, "comparison_all.csv"), row.names = FALSE)
  writeLines(summary_markdown(combined), file.path(OUT_DIR, "SUMMARY.md"))

  message(sprintf(
    "\nWrote %d comparison row(s) across %d thesis/theses to %s",
    nrow(combined), length(comparisons), OUT_DIR
  ))
  print(table(combined$verdict))
}

write_thesis_report <- function(manifest, comparison) {
  lines <- c(
    sprintf("# %s", manifest$title),
    "",
    sprintf("- **Author**: %s", manifest$author),
    sprintf("- **Supervisor**: %s", manifest$supervisor),
    sprintf("- **Year**: %s (%s)", manifest$year, manifest$degree %||% "thesis"),
    if (!is.null(manifest$thesis_url)) sprintf("- **Thesis**: %s", manifest$thesis_url),
    "",
    comparison_markdown(comparison)
  )
  writeLines(lines, file.path(OUT_DIR, sprintf("%s.md", manifest$id)))
}

summary_markdown <- function(combined) {
  counts <- table(combined$verdict)
  c(
    "# artma replication summary",
    "",
    sprintf(
      "%d quantities compared across %d thesis/theses.",
      nrow(combined), length(unique(combined$thesis_id))
    ),
    "",
    "| Verdict | Count |",
    "| --- | ---: |",
    sprintf("| %s | %d |", names(counts), as.integer(counts)),
    "",
    "## All comparisons",
    "",
    comparison_markdown(combined)
  )
}

# Run only when invoked as a script, so tests can source this file for its
# data-pipeline helpers without triggering a full run.
if (sys.nframe() == 0L) {
  main()
}
