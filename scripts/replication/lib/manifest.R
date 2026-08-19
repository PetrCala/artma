# Manifest reading and validation for the replication harness.
#
# Deliberately free of any artma dependency: only base R and `yaml`, so this
# layer can be validated on machines that cannot install the package
# (see test_harness.R).

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Read and validate one replication manifest.
#'
#' @param path Path to a manifest YAML file.
#' @return The manifest as a named list, with `reported` normalised so every
#'   entry carries `artma_model` (possibly `NULL`), a numeric `estimate`, and a
#'   numeric `std_error` (`NA` when the source reports only a point estimate).
read_manifest <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The yaml package is required to read manifests.", call. = FALSE)
  }

  manifest <- yaml::read_yaml(path)

  required <- c("id", "title", "author", "year", "data", "columns", "methods")
  missing <- required[vapply(required, function(f) is.null(manifest[[f]]), logical(1))]
  if (length(missing) > 0L) {
    stop(sprintf(
      "Manifest %s is missing required field(s): %s",
      basename(path), paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  if (is.null(manifest$data$file) || !nzchar(manifest$data$file)) {
    stop(sprintf("Manifest %s: `data.file` is required.", basename(path)), call. = FALSE)
  }

  for (column in c("effect", "se")) {
    if (is.null(manifest$columns[[column]])) {
      stop(sprintf(
        "Manifest %s: `columns.%s` is required -- artma cannot run without it.",
        basename(path), column
      ), call. = FALSE)
    }
  }

  if (length(manifest$methods) == 0L) {
    stop(sprintf("Manifest %s: `methods` is empty.", basename(path)), call. = FALSE)
  }

  manifest$reported <- lapply(
    manifest$reported %||% list(),
    normalize_reported_entry,
    manifest_name = basename(path)
  )

  manifest
}

#' Validate and normalise one `reported` entry.
#' @keywords internal
normalize_reported_entry <- function(entry, manifest_name) {
  required <- c("label", "artma_method", "artma_term", "estimate")
  missing <- required[vapply(required, function(f) is.null(entry[[f]]), logical(1))]
  if (length(missing) > 0L) {
    stop(sprintf(
      "Manifest %s: reported entry %s is missing: %s",
      manifest_name,
      if (is.null(entry$label)) "(unlabelled)" else sprintf("'%s'", entry$label),
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  entry$estimate <- as.numeric(entry$estimate)
  if (is.na(entry$estimate)) {
    stop(sprintf(
      "Manifest %s: reported entry '%s' has a non-numeric estimate.",
      manifest_name, entry$label
    ), call. = FALSE)
  }

  entry$std_error <- if (is.null(entry$std_error)) {
    NA_real_
  } else {
    as.numeric(entry$std_error)
  }

  entry
}

#' Read every manifest in a directory, optionally restricted to a set of ids.
#'
#' Files whose name starts with an underscore (the template) are skipped.
#'
#' @param dir Directory holding `*.yaml` manifests.
#' @param ids Optional character vector of manifest ids to keep, in order.
#' @return A named list of manifests, keyed by id.
read_manifests <- function(dir, ids = NULL) {
  paths <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)
  paths <- paths[!startsWith(basename(paths), "_")]

  if (length(paths) == 0L) {
    stop(sprintf("No manifests found in %s.", dir), call. = FALSE)
  }

  manifests <- lapply(paths, read_manifest)
  names(manifests) <- vapply(manifests, function(m) m$id, character(1))

  duplicated_ids <- unique(names(manifests)[duplicated(names(manifests))])
  if (length(duplicated_ids) > 0L) {
    stop(sprintf(
      "Duplicate manifest id(s): %s", paste(duplicated_ids, collapse = ", ")
    ), call. = FALSE)
  }

  if (!is.null(ids)) {
    unknown <- setdiff(ids, names(manifests))
    if (length(unknown) > 0L) {
      stop(sprintf(
        "Unknown manifest id(s): %s\n  available: %s",
        paste(unknown, collapse = ", "), paste(names(manifests), collapse = ", ")
      ), call. = FALSE)
    }
    manifests <- manifests[ids]
  }

  manifests
}
