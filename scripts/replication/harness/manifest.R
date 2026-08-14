# Manifest loading and validation for the replication harness.
#
# A manifest is a YAML file describing one thesis: where its dataset lives, how
# to map that dataset onto artma's canonical columns, and which numbers the
# author reported. Everything here is a pure function of its inputs so that
# test_harness.R can exercise it without touching the network.

REPLICATION_DEGREES <- c("bachelor", "master")

# artma's own required set (CONST$DATA$REQUIRED_COLNAMES): the pipeline refuses
# to run without these four, so a manifest must map all of them. Mapping them to
# artma's canonical spellings also means the column auto-detection accepts them
# outright instead of falling through to an interactive prompt.
REPLICATION_REQUIRED_COLUMNS <- c("effect", "se", "study_id", "n_obs")

# Default agreement thresholds. A claim matches when the absolute difference is
# within `abs`, or within `rel` of the reported magnitude, whichever is looser.
# Theses round their published tables, so an exact match is not the bar.
REPLICATION_DEFAULT_TOLERANCE <- list(abs = 0.005, rel = 0.05)

`%||%` <- function(x, y) if (is.null(x)) y else x

manifest_stop <- function(id, ...) {
  stop(sprintf("[manifest %s] %s", id %||% "<unknown>", paste0(...)), call. = FALSE)
}

#' Validate one already-parsed manifest.
#'
#' Returns the manifest unchanged (with defaults filled in) or throws with a
#' message naming the offending field. Kept separate from reading so tests can
#' feed it plain lists.
validate_manifest <- function(m) {
  id <- m$id
  if (!is.character(id) || length(id) != 1L || !nzchar(id)) {
    manifest_stop(NULL, "`id` must be a non-empty string.")
  }

  th <- m$thesis
  if (!is.list(th)) manifest_stop(id, "`thesis` must be a mapping.")
  for (f in c("author", "title", "degree", "year", "advisor", "handle")) {
    v <- th[[f]]
    if (is.null(v) || (is.character(v) && !nzchar(v))) {
      manifest_stop(id, sprintf("`thesis.%s` is required.", f))
    }
  }
  if (!th$degree %in% REPLICATION_DEGREES) {
    manifest_stop(id, sprintf(
      "`thesis.degree` must be one of %s, got '%s'.",
      paste(REPLICATION_DEGREES, collapse = "/"), th$degree
    ))
  }
  if (!is.numeric(th$year) || th$year < 2000 || th$year > 2100) {
    manifest_stop(id, "`thesis.year` must be a plausible year.")
  }

  ds <- m$dataset
  if (!is.list(ds)) manifest_stop(id, "`dataset` must be a mapping.")
  if (!is.character(ds$url) || !nzchar(ds$url)) {
    manifest_stop(id, "`dataset.url` is required.")
  }
  if (!is.logical(ds$code_published) || length(ds$code_published) != 1L) {
    manifest_stop(id, "`dataset.code_published` must be TRUE or FALSE.")
  }

  cols <- m$columns
  if (!is.list(cols)) manifest_stop(id, "`columns` must be a mapping.")
  missing <- setdiff(REPLICATION_REQUIRED_COLUMNS, names(cols))
  if (length(missing)) {
    manifest_stop(id, sprintf(
      "`columns` is missing required mapping(s): %s.", paste(missing, collapse = ", ")
    ))
  }

  if (!is.null(m$artma_options) && !is.list(m$artma_options)) {
    manifest_stop(id, "`artma_options` must be a mapping of option paths to values.")
  }

  claims <- m$claims
  if (!is.list(claims) || length(claims) == 0L) {
    manifest_stop(id, "`claims` must be a non-empty list.")
  }
  seen <- character(0)
  m$claims <- lapply(seq_along(claims), function(i) {
    validate_claim(id, claims[[i]], i, seen_ids = seen) -> c_ok
    seen <<- c(seen, c_ok$id)
    c_ok
  })

  m
}

#' Validate a single claim and fill in its defaults.
validate_claim <- function(manifest_id, cl, index, seen_ids = character(0)) {
  where <- sprintf("claims[[%d]]", index)
  if (!is.list(cl)) manifest_stop(manifest_id, where, " must be a mapping.")

  if (!is.character(cl$id) || !nzchar(cl$id)) {
    manifest_stop(manifest_id, where, "$id is required.")
  }
  if (cl$id %in% seen_ids) {
    manifest_stop(manifest_id, sprintf("duplicate claim id '%s'.", cl$id))
  }
  for (f in c("label", "source", "method", "artma_model", "artma_term")) {
    if (!is.character(cl[[f]]) || !nzchar(cl[[f]])) {
      manifest_stop(manifest_id, where, sprintf("$%s is required (claim '%s').", f, cl$id))
    }
  }
  # `source` is the audit trail back to the PDF: without a table/page pointer a
  # reported number cannot be checked by a reader, which defeats the exercise.
  if (!grepl("p\\.?\\s*[0-9]", cl$source, ignore.case = TRUE)) {
    manifest_stop(manifest_id, sprintf(
      "claim '%s': `source` must cite a page, e.g. 'Table 5.1, p. 35' (got '%s').",
      cl$id, cl$source
    ))
  }
  if (!is.numeric(cl$reported) || length(cl$reported) != 1L || !is.finite(cl$reported)) {
    manifest_stop(manifest_id, sprintf("claim '%s': `reported` must be a finite number.", cl$id))
  }
  for (f in c("reported_se", "reported_ci_low", "reported_ci_high")) {
    if (!is.null(cl[[f]]) && (!is.numeric(cl[[f]]) || !is.finite(cl[[f]]))) {
      manifest_stop(manifest_id, sprintf("claim '%s': `%s` must be numeric when present.", cl$id, f))
    }
  }
  for (f in c("artma_model", "artma_term")) {
    tryCatch(grepl(cl[[f]], "probe"), error = function(e) {
      manifest_stop(manifest_id, sprintf("claim '%s': `%s` is not a valid regex: %s", cl$id, f, conditionMessage(e)))
    })
  }

  cl$tolerance <- resolve_tolerance(cl$tolerance)
  cl
}

#' Merge a claim-level tolerance over the harness default.
resolve_tolerance <- function(tol) {
  if (is.null(tol)) return(REPLICATION_DEFAULT_TOLERANCE)
  if (!is.list(tol)) stop("`tolerance` must be a mapping with `abs` and/or `rel`.", call. = FALSE)
  out <- REPLICATION_DEFAULT_TOLERANCE
  for (k in c("abs", "rel")) {
    if (!is.null(tol[[k]])) {
      if (!is.numeric(tol[[k]]) || tol[[k]] < 0) {
        stop(sprintf("`tolerance$%s` must be a non-negative number.", k), call. = FALSE)
      }
      out[[k]] <- tol[[k]]
    }
  }
  out
}

#' Read one manifest, forcing a UTF-8 interpretation.
#'
#' Manifests carry Czech names, and these scripts routinely run under a C/POSIX
#' locale where `yaml::read_yaml()` would mangle or reject them. Reading the
#' bytes ourselves and marking the encoding keeps the harness locale-independent.
read_manifest <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  yaml::yaml.load(txt)
}

#' Read and validate every manifest in a directory.
load_manifests <- function(dir) {
  files <- sort(list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE))
  lapply(files, function(p) {
    m <- validate_manifest(read_manifest(p))
    m$.path <- p
    m
  })
}

#' Every artma method a manifest's claims refer to, in a stable order.
manifest_methods <- function(m) {
  unique(vapply(m$claims, function(cl) cl$method, character(1)))
}
