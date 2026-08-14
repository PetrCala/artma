# Fetching a thesis dataset and shaping it into artma's canonical columns.
#
# The published files are real-world messy: zip archives with Czech filenames,
# Excel workbooks, and CSVs exported from a Czech-locale spreadsheet where the
# decimal separator is a comma inside quoted fields. The manifest's `dataset`
# block says how to read each one; this file does the reading.

#' Download a URL to `dest` unless it is already cached there.
fetch_file <- function(url, dest, quiet = FALSE) {
  if (file.exists(dest) && file.size(dest) > 0L) return(invisible(dest))
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!quiet) message("  downloading ", basename(dest))
  utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  invisible(dest)
}

#' Verify a file against an expected sha256, when one is recorded.
#'
#' A published dataset is a moving target only in the sense that the repository
#' could re-issue it; pinning the hash makes a silent change loud.
verify_checksum <- function(path, expected) {
  if (is.null(expected) || !nzchar(expected)) return(invisible(NA_character_))
  if (!requireNamespace("digest", quietly = TRUE)) return(invisible(NA_character_))
  actual <- digest::digest(path, algo = "sha256", file = TRUE)
  if (!identical(actual, expected)) {
    stop(sprintf("checksum mismatch for %s\n  expected %s\n  actual   %s",
                 basename(path), expected, actual), call. = FALSE)
  }
  invisible(actual)
}

#' Pull one member out of a zip archive, tolerating non-UTF-8 entry names.
#'
#' Entry names in these archives are frequently CP852/CP1250 Czech, so matching
#' is done on the basename and, failing that, on a case-insensitive suffix.
extract_member <- function(archive, member, exdir) {
  entries <- utils::unzip(archive, list = TRUE)$Name
  hit <- entries[basename(entries) == member]
  if (length(hit) == 0L) {
    hit <- entries[grepl(paste0(member, "$"), entries, fixed = FALSE, ignore.case = TRUE)]
  }
  if (length(hit) == 0L) {
    stop(sprintf("archive member '%s' not found in %s\n  available: %s",
                 member, basename(archive), paste(utils::head(entries, 20), collapse = ", ")),
         call. = FALSE)
  }
  utils::unzip(archive, files = hit[1], exdir = exdir, junkpaths = TRUE)
  file.path(exdir, basename(hit[1]))
}

#' Read a delimited or Excel file into a data frame.
#'
#' `decimal_mark = ","` handles the Czech-locale CSV exports, where numbers
#' arrive as quoted strings like "0,2065" that R would otherwise read as text.
read_dataset_file <- function(path, delim = NULL, decimal_mark = ".",
                              encoding = "UTF-8", sheet = NULL) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls", "xlsm")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("reading '", basename(path), "' needs the readxl package", call. = FALSE)
    }
    df <- as.data.frame(readxl::read_excel(path, sheet = sheet %||% 1L), stringsAsFactors = FALSE)
    return(df)
  }
  # Several of these files are saved from Excel with a UTF-8 BOM, which would
  # otherwise ride along inside the first column's name ("﻿bs") and make the
  # manifest's column mapping fail on a name that looks correct in the error.
  if (identical(encoding, "UTF-8")) path <- strip_bom(path)
  if (is.null(delim)) delim <- sniff_delim(path, encoding)
  df <- utils::read.table(
    path, header = TRUE, sep = delim, quote = "\"", comment.char = "",
    stringsAsFactors = FALSE, check.names = FALSE, fill = TRUE,
    fileEncoding = if (identical(encoding, "UTF-8")) "" else encoding,
    dec = if (identical(decimal_mark, ",")) "," else "."
  )
  # A comma-decimal export read with sep="," leaves numbers as quoted strings;
  # dec= cannot help there because the separator already consumed the comma.
  if (identical(decimal_mark, ",")) df <- repair_comma_decimals(df)
  dedupe_names(df)
}

#' Make duplicate column names addressable.
#'
#' Wide "one column block per outcome" layouts repeat headers -- Horák's file
#' has four separate `se` columns, one after each outcome. `check.names = FALSE`
#' keeps the duplicates but then `df[["se"]]` silently resolves to whichever
#' came first, so a manifest could not name the others and would not be told.
#' The first occurrence keeps the bare name; later ones get `__2`, `__3`, ...
dedupe_names <- function(df) {
  nms <- names(df)
  dup <- duplicated(nms)
  if (!any(dup)) return(df)
  seen <- list()
  names(df) <- vapply(nms, function(n) {
    k <- (seen[[n]] %||% 0L) + 1L
    seen[[n]] <<- k
    if (k == 1L) n else paste0(n, "__", k)
  }, character(1), USE.NAMES = FALSE)
  df
}

#' Return a path whose content has no leading UTF-8 byte-order mark.
#'
#' Copies to a temp file only when a BOM is actually present, so the common case
#' costs one 3-byte read.
strip_bom <- function(path) {
  con <- file(path, "rb")
  head3 <- readBin(con, "raw", 3L)
  close(con)
  if (length(head3) < 3L || !identical(head3, as.raw(c(0xEF, 0xBB, 0xBF)))) return(path)
  raw <- readBin(path, "raw", file.size(path))
  out <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  writeBin(raw[-seq_len(3)], out)
  out
}

sniff_delim <- function(path, encoding = "UTF-8") {
  con <- file(path, "r", encoding = if (identical(encoding, "UTF-8")) "" else encoding)
  on.exit(close(con))
  line <- readLines(con, n = 1L, warn = FALSE)
  if (length(line) == 0L) return(",")
  counts <- c("," = lengths(regmatches(line, gregexpr(",", line))),
              ";" = lengths(regmatches(line, gregexpr(";", line))),
              "\t" = lengths(regmatches(line, gregexpr("\t", line))))
  names(counts)[which.max(counts)]
}

#' Convert character columns of the form "0,2065" into numerics.
repair_comma_decimals <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (!is.character(col)) next
    trimmed <- trimws(col)
    looks_numeric <- grepl("^-?[0-9]+(,[0-9]+)?$", trimmed) | !nzchar(trimmed) | trimmed %in% c("NA", "N/A")
    if (all(looks_numeric) && any(grepl(",", trimmed))) {
      df[[nm]] <- suppressWarnings(as.numeric(gsub(",", ".", trimmed, fixed = TRUE)))
    }
  }
  df
}

#' Resolve a manifest's dataset block to a local, parsed data frame.
resolve_dataset <- function(m, cache_dir) {
  ds <- m$dataset
  raw <- file.path(cache_dir, m$id, basename(sub("\\?.*$", "", ds$url)))
  fetch_file(ds$url, raw)
  verify_checksum(raw, ds$sha256)

  path <- raw
  if (!is.null(ds$archive_member) && nzchar(ds$archive_member)) {
    exdir <- file.path(cache_dir, m$id, "extracted")
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
    path <- extract_member(raw, ds$archive_member, exdir)
  }
  df <- read_dataset_file(
    path,
    delim = ds$delim, decimal_mark = ds$decimal_mark %||% ".",
    encoding = ds$encoding %||% "UTF-8", sheet = ds$sheet
  )
  apply_row_filter(df, ds$row_filter)
}

#' Restrict a dataset to the subsample a thesis's table actually reports.
#'
#' Several theses publish one pooled file but report results per subgroup (a
#' milk-type dummy, a country group). `row_filter` is an R expression evaluated
#' with the raw columns in scope, e.g. `"Milk == 1"`.
apply_row_filter <- function(df, expr) {
  if (is.null(expr) || !nzchar(expr)) return(df)
  keep <- tryCatch(
    eval(parse(text = expr), envir = df, enclos = baseenv()),
    error = function(e) stop(sprintf("row_filter '%s' failed: %s", expr, conditionMessage(e)), call. = FALSE)
  )
  if (!is.logical(keep) || length(keep) != nrow(df)) {
    stop(sprintf("row_filter '%s' must yield one logical per row (got %s of length %d)",
                 expr, class(keep)[1], length(keep)), call. = FALSE)
  }
  keep[is.na(keep)] <- FALSE
  out <- df[keep, , drop = FALSE]
  if (nrow(out) == 0L) stop(sprintf("row_filter '%s' selected no rows", expr), call. = FALSE)
  out
}

#' Rename source columns onto artma's canonical names and drop unusable rows.
#'
#' Returns the frame plus a record of how many rows were dropped, so the summary
#' can report the sample artma actually fitted against the thesis's stated N.
prepare_for_artma <- function(df, columns) {
  out <- list()
  for (canonical in names(columns)) {
    src <- columns[[canonical]]
    if (!src %in% names(df)) {
      stop(sprintf("column '%s' (mapped to '%s') not found. Available: %s",
                   src, canonical, paste(utils::head(names(df), 40), collapse = ", ")),
           call. = FALSE)
    }
    out[[canonical]] <- df[[src]]
  }
  out <- as.data.frame(out, stringsAsFactors = FALSE)

  for (nm in intersect(c("effect", "se", "n_obs", "t_stat"), names(out))) {
    out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
  }
  n_raw <- nrow(out)
  keep <- is.finite(out$effect) & is.finite(out$se) & out$se > 0
  out <- out[keep, , drop = FALSE]
  if (nrow(out) == 0L) {
    stop("no usable rows after dropping non-finite effect/se; check `columns` and `decimal_mark`",
         call. = FALSE)
  }
  # artma clusters on study_id; a character key is fine but it must be stable.
  if ("study_id" %in% names(out)) out$study_id <- as.character(out$study_id)
  attr(out, "n_raw") <- n_raw
  attr(out, "n_dropped") <- n_raw - nrow(out)
  out
}
