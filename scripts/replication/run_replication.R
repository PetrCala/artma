#!/usr/bin/env Rscript
# Replicate IES meta-analysis theses with artma and compare against the numbers
# their authors reported.
#
#   Rscript scripts/replication/run_replication.R              # all manifests
#   Rscript scripts/replication/run_replication.R --only=<id>  # one manifest
#   Rscript scripts/replication/run_replication.R --list       # list manifests
#
# Writes SUMMARY.md next to the manifests, and per-thesis artefacts under out/:
# `estimates.csv` (everything artma emitted) and `claims.csv` (the comparison).
# The estimates dump is the tool for fixing a claim whose regexes did not match:
# it is the authoritative list of the model/term pairs actually available.

suppressWarnings(suppressMessages({
  ok <- requireNamespace("yaml", quietly = TRUE) && requireNamespace("artma", quietly = TRUE)
}))
if (!ok) stop("run_replication.R needs the `yaml` and `artma` packages installed.", call. = FALSE)

HERE <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(f)) dirname(normalizePath(f)) else file.path(getwd(), "scripts", "replication")
})

source(file.path(HERE, "harness", "manifest.R"))
source(file.path(HERE, "harness", "dataset.R"))
source(file.path(HERE, "harness", "compare.R"))
source(file.path(HERE, "harness", "summary.R"))

MANIFEST_DIR <- file.path(HERE, "manifests")
CACHE_DIR <- file.path(HERE, "data")
OUT_DIR <- file.path(HERE, "out")

args <- commandArgs(trailingOnly = TRUE)
only <- sub("^--only=", "", grep("^--only=", args, value = TRUE))
do_list <- any(args == "--list")

manifests <- load_manifests(MANIFEST_DIR)
if (length(manifests) == 0L) stop("no manifests found in ", MANIFEST_DIR, call. = FALSE)
if (length(only)) {
  manifests <- Filter(function(m) m$id %in% only, manifests)
  if (length(manifests) == 0L) stop("no manifest matched --only=", paste(only, collapse = ","), call. = FALSE)
}

if (do_list) {
  for (m in manifests) {
    cat(sprintf("%-38s %s (%d), %d claims\n", m$id, m$thesis$author, m$thesis$year, length(m$claims)))
  }
  quit(status = 0)
}

dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

#' Run artma over one prepared data frame and return the bound estimates frame.
run_artma <- function(m, df, out_dir) {
  csv <- file.path(out_dir, "prepared.csv")
  utils::write.csv(df, csv, row.names = FALSE, na = "")

  # options_create() merges into an existing file rather than regenerating it,
  # so a column mapping guessed during an earlier (failed) run would survive and
  # poison this one. Each run starts from a clean options directory.
  opts_dir <- file.path(out_dir, "options")
  unlink(opts_dir, recursive = TRUE)
  dir.create(opts_dir, recursive = TRUE, showWarnings = FALSE)
  artma::options_create(
    options_file_name = paste0(m$id, ".yaml"),
    options_dir = opts_dir,
    user_input = c(
      list("data.source_path" = normalizePath(csv), "general.seed" = 20240101L),
      m$artma_options %||% list()
    ),
    should_overwrite = TRUE
  )

  res <- artma::artma(
    methods = manifest_methods(m),
    options = paste0(m$id, ".yaml"),
    options_dir = opts_dir
  )
  failed <- attr(res, "failed_methods")
  if (length(failed)) {
    message("  note: artma methods failed: ", paste(names(failed), collapse = ", "))
  }
  collect_estimates(res)
}

replicate_one <- function(m) {
  cat(sprintf("\n== %s  (%s, %d)\n", m$id, m$thesis$author, m$thesis$year))
  out_dir <- file.path(OUT_DIR, m$id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  res <- list(manifest = m, n_used = NA_integer_, n_dropped = NA_integer_,
              claims = NULL, error = NULL)
  tryCatch({
    raw <- resolve_dataset(m, CACHE_DIR)
    df <- prepare_for_artma(raw, m$columns)
    res$n_used <- nrow(df)
    res$n_dropped <- attr(df, "n_dropped")
    cat(sprintf("  %d estimates (%d dropped)\n", res$n_used, res$n_dropped))

    estimates <- run_artma(m, df, out_dir)
    utils::write.csv(estimates, file.path(out_dir, "estimates.csv"), row.names = FALSE)

    res$claims <- evaluate_claims(m, estimates)
    utils::write.csv(res$claims, file.path(out_dir, "claims.csv"), row.names = FALSE)

    tally <- table(factor(res$claims$verdict, levels = REPLICATION_VERDICTS))
    cat("  ", paste(sprintf("%s=%d", names(tally), as.integer(tally))[tally > 0], collapse = "  "), "\n", sep = "")
  }, error = function(e) {
    res$error <<- conditionMessage(e)
    cat("  FAILED: ", conditionMessage(e), "\n", sep = "")
  })
  res
}

results <- lapply(manifests, replicate_one)

summary_path <- file.path(HERE, "SUMMARY.md")
write_summary(results, summary_path, generated_at = format(Sys.Date()))
cat("\nWrote ", summary_path, "\n", sep = "")

tally <- verdict_tally(Filter(function(r) is.null(r$error), results))
cat("Totals: ", paste(sprintf("%s=%d", names(tally), tally)[tally > 0], collapse = "  "), "\n", sep = "")

# A manifest whose regexes do not resolve is a harness/manifest defect rather
# than a finding, so surface it with a non-zero status for CI.
unresolved <- sum(tally[c("unmatched", "ambiguous", "error")], na.rm = TRUE)
failed <- sum(vapply(results, function(r) !is.null(r$error), logical(1)))
if (unresolved > 0 || failed > 0) {
  cat("Unresolved claims: ", unresolved, "; failed theses: ", failed, "\n", sep = "")
}
