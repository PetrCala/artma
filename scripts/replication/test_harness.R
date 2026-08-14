#!/usr/bin/env Rscript
# Self-contained checks for the manifest and comparison layers.
#
# These deliberately avoid artma entirely: they exercise the parts of the
# harness that are pure data manipulation, so the pipeline can be validated
# before any thesis data or R >= 4.4 environment is available.
#
# Run with:  Rscript scripts/replication/test_harness.R

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

failures <- 0L
check <- function(label, expr) {
  ok <- isTRUE(tryCatch(expr, error = function(e) structure(FALSE, msg = conditionMessage(e))))
  if (!ok) {
    failures <<- failures + 1L
    cat(sprintf("  FAIL  %s\n", label))
  } else {
    cat(sprintf("  ok    %s\n", label))
  }
}

fixture_estimates <- function() {
  data.frame(
    method = c("linear_tests", "linear_tests", "linear_tests", "effect_summary_stats"),
    model = c("OLS", "OLS", "WLS", "all"),
    term = c("(Intercept)", "se", "se", "mean"),
    estimate = c(0.1200, -0.8500, -0.7900, 0.2100),
    std_error = c(0.0300, 0.2000, 0.1800, 0.0250),
    statistic = NA_real_, p_value = NA_real_,
    conf_low = NA_real_, conf_high = NA_real_,
    n_obs = 500L, n_clusters = 40L, note = NA_character_,
    stringsAsFactors = FALSE
  )
}

cat("\nmatching\n")
est <- fixture_estimates()

check("matches a unique (method, model, term) triple", {
  m <- match_estimate(est, list(artma_method = "linear_tests", artma_model = "OLS", artma_term = "se"))
  identical(m$reason, "matched") && nrow(m$hit) == 1L && identical(m$hit$estimate, -0.85)
})

check("reports not_produced when nothing matches", {
  m <- match_estimate(est, list(artma_method = "maive", artma_model = NULL, artma_term = "se"))
  is.null(m$hit) && identical(m$reason, "not_produced")
})

check("reports ambiguous when the model is left unpinned", {
  m <- match_estimate(est, list(artma_method = "linear_tests", artma_model = NULL, artma_term = "se"))
  is.null(m$hit) && identical(m$reason, "ambiguous")
})

check("treats artma_model as an anchored regex", {
  m <- match_estimate(est, list(artma_method = "linear_tests", artma_model = "^W.S$", artma_term = "se"))
  identical(m$reason, "matched") && identical(m$hit$estimate, -0.79)
})

check("anchoring stops a substring from matching", {
  m <- match_estimate(est, list(artma_method = "linear", artma_model = "OLS", artma_term = "se"))
  is.null(m$hit) && identical(m$reason, "not_produced")
})

cat("\nverdicts\n")
check("identical values replicate", identical(verdict(0.85, 0.85), "replicated"))
check("0.5% apart replicates", identical(verdict(1.000, 1.004), "replicated"))
check("5% apart is close", identical(verdict(1.00, 1.05), "close"))
check("50% apart differs", identical(verdict(1.0, 1.5), "differs"))
check("opposite signs flag a sign flip", identical(verdict(0.5, -0.5), "sign flip"))
check("missing artma value is unknown", identical(verdict(0.5, NA_real_), "unknown"))

cat("\nconfidence intervals\n")
check("overlapping intervals detected", isTRUE(ci_overlap(0.10, 0.03, 0.12, 0.03)))
check("disjoint intervals detected", identical(ci_overlap(0.10, 0.001, 0.90, 0.001), FALSE))
check("missing standard error yields NA", is.na(ci_overlap(0.10, NA_real_, 0.12, 0.03)))

cat("\nbinding\n")
check("binds estimates across methods", {
  bound <- bind_estimates(list(
    a = list(estimates = fixture_estimates()[1:2, ]),
    b = list(estimates = fixture_estimates()[3:4, ])
  ))
  nrow(bound) == 4L
})

check("skips methods that report no numbers", {
  bound <- bind_estimates(list(
    a = list(estimates = fixture_estimates()),
    plot_only = list(estimates = empty_estimates()),
    nothing = list(plots = list())
  ))
  nrow(bound) == 4L
})

check("all-empty results give an empty frame, not an error", {
  nrow(bind_estimates(list(x = list(plots = list())))) == 0L
})

cat("\nend-to-end comparison\n")
manifest <- list(
  id = "fixture", author = "Fixture Author", year = 2025,
  reported = list(
    list(
      label = "PET", artma_method = "linear_tests", artma_model = "OLS",
      artma_term = "\\(Intercept\\)", estimate = 0.12, std_error = 0.03,
      source = "Table 5.2"
    ),
    list(
      label = "FAT", artma_method = "linear_tests", artma_model = "OLS",
      artma_term = "se", estimate = -0.60, std_error = 0.20, source = "Table 5.2"
    ),
    list(
      label = "MAIVE", artma_method = "maive", artma_model = "MAIVE",
      artma_term = "effect", estimate = 0.30, std_error = NA_real_, source = "Table 6.1"
    )
  )
)

comparison <- compare_manifest(manifest, est)

check("one row per reported quantity", nrow(comparison) == 3L)
check("exact agreement is marked replicated", identical(comparison$verdict[1], "replicated"))
check("a materially different value is marked differs", identical(comparison$verdict[2], "differs"))
check("an unrun method is marked not_produced", identical(comparison$verdict[3], "not_produced"))
check("difference is artma minus author", isTRUE(all.equal(comparison$difference[2], -0.25)))
check("relative difference is signed and in percent", isTRUE(all.equal(comparison$rel_diff_pct[2], -41.6666667)))
check("overlapping CIs recorded", isTRUE(comparison$ci_overlap[1]))
check("missing artma estimate leaves NA, not an error", is.na(comparison$artma_estimate[3]))

cat("\nmarkdown rendering\n")
md <- comparison_markdown(comparison)
check("renders a header row", grepl("| Quantity | Author | artma |", md, fixed = TRUE))
check("renders one line per quantity", length(strsplit(md, "\n")[[1]]) == 5L)
check("formats estimate with its standard error", grepl("0.1200 (0.0300)", md, fixed = TRUE))
check("renders missing values as a dash", grepl("| -- |", md, fixed = TRUE))
check("empty comparison renders a placeholder", grepl("No reported", comparison_markdown(empty_comparison())))

cat("\nmanifest validation\n")
tmp <- tempfile(fileext = ".yaml")
on.exit(unlink(tmp), add = TRUE)

write_manifest <- function(text) writeLines(text, tmp)

write_manifest(c(
  "id: good", "title: T", "author: A", "supervisor: S", "year: 2025",
  "data: {file: d.csv}", "columns: {effect: e, se: s}", "methods: [linear_tests]",
  "reported:", "  - label: PET", "    artma_method: linear_tests",
  "    artma_term: '(Intercept)'", "    estimate: 0.1"
))
check("accepts a well-formed manifest", {
  m <- read_manifest(tmp)
  identical(m$id, "good") && length(m$reported) == 1L && is.na(m$reported[[1]]$std_error)
})

write_manifest(c("id: bad", "title: T"))
check("rejects a manifest missing required fields", {
  inherits(tryCatch(read_manifest(tmp), error = function(e) e), "error")
})

write_manifest(c(
  "id: bad2", "title: T", "author: A", "supervisor: S", "year: 2025",
  "data: {file: d.csv}", "columns: {effect: e}", "methods: [linear_tests]"
))
check("rejects a manifest without an `se` column mapping", {
  inherits(tryCatch(read_manifest(tmp), error = function(e) e), "error")
})

write_manifest(c(
  "id: bad3", "title: T", "author: A", "supervisor: S", "year: 2025",
  "data: {file: d.csv}", "columns: {effect: e, se: s}", "methods: [linear_tests]",
  "reported:", "  - label: PET", "    artma_method: linear_tests"
))
check("rejects a reported entry with no estimate", {
  inherits(tryCatch(read_manifest(tmp), error = function(e) e), "error")
})

write_manifest(c(
  "id: ok_no_reported", "title: T", "author: A", "supervisor: S", "year: 2025",
  "data: {file: d.csv}", "columns: {effect: e, se: s}", "methods: [linear_tests]"
))
check("allows a manifest with no transcribed numbers yet", {
  length(read_manifest(tmp)$reported) == 0L
})

cat("\ndata pipeline\n")
# Sourcing the runner must not start a run; it only brings its helpers into
# scope. `main()` itself is not exercised here -- it needs artma.
source(file.path(here, "run_replication.R"))

fixture_csv <- tempfile(fileext = ".csv")
on.exit(unlink(fixture_csv), add = TRUE)
utils::write.csv(
  data.frame(
    es = c(0.1, 0.2, 0.3), stderr = c(0.01, 0.02, 0.03),
    paper = c("a", "b", "c"), unrelated = 1:3
  ),
  fixture_csv,
  row.names = FALSE
)

check("reads a csv dataset", nrow(read_dataset(fixture_csv)) == 3L)

check("renames thesis columns onto canonical names", {
  mapped <- apply_column_mapping(
    read_dataset(fixture_csv),
    list(effect = "es", se = "stderr", study_id = "paper")
  )
  identical(names(mapped), c("effect", "se", "study_id"))
})

check("drops columns the manifest does not map", {
  mapped <- apply_column_mapping(
    read_dataset(fixture_csv),
    list(effect = "es", se = "stderr")
  )
  !("unrelated" %in% names(mapped))
})

check("names a column that is absent from the data", {
  msg <- tryCatch(
    apply_column_mapping(read_dataset(fixture_csv), list(effect = "nope", se = "stderr")),
    error = function(e) conditionMessage(e)
  )
  grepl("absent from the data", msg)
})

check("rejects an unsupported data format", {
  msg <- tryCatch(read_dataset("thesis.parquet"), error = function(e) conditionMessage(e))
  grepl("Unsupported data format", msg)
})

check("parses --ids into a character vector", {
  identical(parse_args(c("--ids", "a,b,c"))$ids, c("a", "b", "c"))
})

check("parses --refresh-data", isTRUE(parse_args("--refresh-data")$refresh_data))
check("defaults to all manifests and no refresh", {
  args <- parse_args(character(0))
  is.null(args$ids) && identical(args$refresh_data, FALSE)
})
check("rejects an unknown argument", {
  inherits(tryCatch(parse_args("--nope"), error = function(e) e), "error")
})

check("skips checksum verification when none is pinned", {
  is.null(verify_checksum(fixture_csv, NULL))
})

cat(sprintf("\n%s\n", strrep("-", 46)))
if (failures > 0L) {
  cat(sprintf("%d check(s) FAILED\n", failures))
  quit(status = 1L)
}
cat("All checks passed.\n")
