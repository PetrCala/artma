#!/usr/bin/env Rscript
# Tests for the replication harness.
#
#   Rscript scripts/replication/test_harness.R
#
# These cover the harness's pure functions only: manifest validation, claim
# matching, scoring, dataset shaping and summary rendering. Nothing here touches
# the network or runs artma, so the suite stays fast and runs anywhere. The last
# block validates the real manifests in manifests/, which catches a malformed
# manifest without needing its dataset.
#
# Deliberately written against base R rather than testthat: the harness is a
# standalone script, and its tests should run wherever artma's own dependencies
# are not yet installed.

HERE <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(f)) dirname(normalizePath(f)) else file.path(getwd(), "scripts", "replication")
})
source(file.path(HERE, "harness", "manifest.R"))
source(file.path(HERE, "harness", "dataset.R"))
source(file.path(HERE, "harness", "compare.R"))
source(file.path(HERE, "harness", "summary.R"))

.passed <- 0L
.failed <- character(0)

ok <- function(cond, what) {
  if (isTRUE(cond)) {
    .passed <<- .passed + 1L
  } else {
    .failed <<- c(.failed, what)
    cat("  FAIL: ", what, "\n", sep = "")
  }
}
eq <- function(actual, expected, what) ok(isTRUE(all.equal(actual, expected)), what)
throws <- function(expr, pattern, what) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  if (is.null(e)) {
    ok(FALSE, paste0(what, " (expected an error, got none)"))
  } else {
    ok(grepl(pattern, e, ignore.case = TRUE),
       paste0(what, " (message was: ", e, ")"))
  }
}
section <- function(x) cat("\n", x, "\n", sep = "")

# Overrides *replace* the named top-level element rather than merging into it,
# so a test can hand in a deliberately incomplete `thesis` or an empty `claims`.
good_manifest <- function(...) {
  m <- list(
    id = "demo-2024-topic",
    thesis = list(author = "Nov\u00e1k, Jan", title = "A Meta-Analysis", degree = "master",
                  year = 2024, advisor = "Havr\u00e1nek, Tom\u00e1\u0161", handle = "20.500.11956/1"),
    dataset = list(url = "https://example.org/d.csv", code_published = TRUE),
    columns = list(effect = "PCC", se = "PCCSE", study_id = "ID", n_obs = "N"),
    claims = list(list(
      id = "mean", label = "Mean effect", source = "Table 4.1, p. 30",
      method = "effect_summary_stats", artma_model = "^All Data$", artma_term = "^mean$",
      reported = 0.1
    ))
  )
  overrides <- list(...)
  for (nm in names(overrides)) m[[nm]] <- overrides[[nm]]
  m
}

section("manifest validation")
{
  m <- validate_manifest(good_manifest())
  eq(m$id, "demo-2024-topic", "a well-formed manifest validates")
  eq(m$claims[[1]]$tolerance, REPLICATION_DEFAULT_TOLERANCE, "default tolerance is filled in")

  throws(validate_manifest(good_manifest(id = "")), "id", "empty id rejected")
  throws(validate_manifest(good_manifest(thesis = list(author = "A"))), "thesis\\.", "incomplete thesis rejected")

  bad_degree <- good_manifest(); bad_degree$thesis$degree <- "phd"
  throws(validate_manifest(bad_degree), "degree", "non bachelor/master degree rejected")

  bad_year <- good_manifest(); bad_year$thesis$year <- 1500
  throws(validate_manifest(bad_year), "year", "implausible year rejected")

  throws(validate_manifest(good_manifest(columns = list(effect = "a"))),
         "missing required", "an incomplete column mapping is rejected")
  throws(validate_manifest(good_manifest(claims = list())), "non-empty", "empty claim list rejected")

  no_code <- good_manifest(); no_code$dataset$code_published <- NULL
  throws(validate_manifest(no_code), "code_published", "missing code_published rejected")

  dup <- good_manifest(); dup$claims <- list(dup$claims[[1]], dup$claims[[1]])
  throws(validate_manifest(dup), "duplicate", "duplicate claim ids rejected")

  nopage <- good_manifest(); nopage$claims[[1]]$source <- "Table 4.1"
  throws(validate_manifest(nopage), "page", "source without a page citation rejected")

  noval <- good_manifest(); noval$claims[[1]]$reported <- "0.1"
  throws(validate_manifest(noval), "finite number", "non-numeric reported value rejected")

  badre <- good_manifest(); badre$claims[[1]]$artma_term <- "("
  throws(validate_manifest(badre), "regex", "invalid regex rejected")

  eq(manifest_methods(validate_manifest(good_manifest())), "effect_summary_stats",
     "manifest_methods lists the referenced methods")
}

section("tolerance resolution")
{
  eq(resolve_tolerance(NULL), REPLICATION_DEFAULT_TOLERANCE, "NULL tolerance falls back to the default")
  eq(resolve_tolerance(list(abs = 0.01))$abs, 0.01, "abs override applies")
  eq(resolve_tolerance(list(abs = 0.01))$rel, REPLICATION_DEFAULT_TOLERANCE$rel,
     "unspecified half of the tolerance keeps its default")
  throws(resolve_tolerance(list(abs = -1)), "non-negative", "negative tolerance rejected")
}

section("claim matching")
{
  est <- data.frame(
    method = c("linear_tests", "linear_tests", "linear_tests", "effect_summary_stats"),
    model = c("ols", "ols", "fe", "All Data"),
    term = c("effect", "publication_bias", "effect", "mean"),
    estimate = c(0.10, -0.50, 0.12, 0.20),
    std_error = c(0.01, 0.05, 0.02, 0.03),
    p_value = NA_real_, conf_low = NA_real_, conf_high = NA_real_, n_obs = 100,
    stringsAsFactors = FALSE
  )
  cl <- function(...) utils::modifyList(
    list(id = "c", method = "linear_tests", artma_model = "^ols$", artma_term = "^effect$"), list(...))

  m1 <- match_claim(est, cl())
  eq(m1$status, "ok", "an unambiguous claim matches")
  eq(m1$row$estimate, 0.10, "the matched row carries the right estimate")

  m2 <- match_claim(est, cl(artma_term = "effect"))
  eq(m2$status, "ok", "'effect' anchored to ols still matches one row")

  m3 <- match_claim(est, cl(artma_model = "."))
  eq(m3$status, "ambiguous", "a regex matching several rows is ambiguous")

  m4 <- match_claim(est, cl(artma_term = "^nope$"))
  eq(m4$status, "unmatched", "a regex matching nothing is unmatched")
  ok(any(grepl("ols / effect", m4$candidates)), "unmatched claims report the available model/term pairs")

  m5 <- match_claim(est, cl(method = "maive"))
  eq(m5$status, "unmatched", "a method that produced nothing is unmatched")
  eq(length(m5$candidates), 0L, "no candidates for an absent method")

  # A model recorded as NA (bma emits one) must still be matchable.
  est_na <- data.frame(method = "bma", model = NA_character_, term = "Se", estimate = 0.3,
                       std_error = NA_real_, p_value = NA_real_, conf_low = NA_real_,
                       conf_high = NA_real_, n_obs = NA_real_, stringsAsFactors = FALSE)
  m6 <- match_claim(est_na, cl(method = "bma", artma_model = "", artma_term = "^Se$"))
  eq(m6$status, "ok", "an NA model matches an empty model regex")
}

section("scoring")
{
  tol <- list(abs = 0.005, rel = 0.05)
  eq(score_claim(0.100, 0.102, tol)$verdict, "match", "small absolute difference is a match")
  eq(score_claim(1.000, 1.040, tol)$verdict, "match", "within 5% relative is a match")
  # tolerance here is 0.005, so `close` spans a difference in (0.005, 0.015].
  eq(score_claim(0.100, 0.112, tol)$verdict, "close", "moderately outside tolerance is close")
  eq(score_claim(0.100, 0.118, tol)$verdict, "mismatch", "beyond 3x tolerance is a mismatch")
  eq(score_claim(0.100, 0.900, tol)$verdict, "mismatch", "far outside tolerance is a mismatch")
  eq(score_claim(0.100, -0.101, tol)$verdict, "mismatch", "a sign flip is never close")
  eq(score_claim(0.100, NA_real_, tol)$verdict, "error", "a non-finite estimate scores as error")
  eq(score_claim(0.100, 0.102, tol)$diff, 0.002, "diff is artma minus reported")
  eq(score_claim(0, 0, tol)$verdict, "match", "zero against zero is a match")
}

section("evaluate_claims")
{
  est <- data.frame(method = "linear_tests", model = "ols", term = "effect", estimate = 0.10,
                    std_error = 0.01, p_value = NA_real_, conf_low = NA_real_,
                    conf_high = NA_real_, n_obs = 100, stringsAsFactors = FALSE)
  m <- validate_manifest(good_manifest(claims = list(
    list(id = "pet", label = "PET", source = "Table 5.1, p. 35", method = "linear_tests",
         artma_model = "^ols$", artma_term = "^effect$", reported = 0.1),
    list(id = "gone", label = "Absent", source = "Table 5.2, p. 36", method = "linear_tests",
         artma_model = "^nope$", artma_term = "^effect$", reported = 0.2)
  )))
  res <- evaluate_claims(m, est)
  eq(nrow(res), 2L, "every claim yields a row, matched or not")
  eq(res$verdict, c("match", "unmatched"), "verdicts are per claim")
  ok(is.na(res$artma[2]), "an unmatched claim has no artma value")
}

section("collect_estimates")
{
  fake <- list(
    linear_tests = list(estimates = data.frame(
      method = "linear_tests", model = "ols", term = "effect", estimate = 0.1,
      std_error = 0.01, p_value = 0.5, conf_low = 0, conf_high = 0.2, n_obs = 10,
      stringsAsFactors = FALSE)),
    funnel_plot = list(estimates = NULL)
  )
  e <- collect_estimates(fake)
  eq(nrow(e), 1L, "plot-only methods contribute no rows")
  eq(nrow(collect_estimates(list())), 0L, "an empty result gives an empty frame")
  eq(names(collect_estimates(list())), names(empty_estimates()), "the empty frame keeps the schema")
}

section("dataset parsing")
{
  df <- data.frame(a = c("0,25", "1,5"), b = c("x", "y"), c = c("1", "2"), stringsAsFactors = FALSE)
  fixed <- repair_comma_decimals(df)
  eq(fixed$a, c(0.25, 1.5), "comma decimals become numeric")
  eq(fixed$b, c("x", "y"), "genuine text columns are left alone")

  tmp <- tempfile(fileext = ".csv")
  writeLines(c("a;b;c", "1;2;3"), tmp)
  eq(sniff_delim(tmp), ";", "semicolon delimiter is detected")
  writeLines(c("a,b,c", "1,2,3"), tmp)
  eq(sniff_delim(tmp), ",", "comma delimiter is detected")
  unlink(tmp)
}

section("duplicate column names")
{
  df <- data.frame(a = 1, b = 2, c = 3)
  names(df) <- c("gdp", "se", "se")
  out <- dedupe_names(df)
  eq(names(out), c("gdp", "se", "se__2"), "later duplicates are suffixed, the first keeps its name")
  eq(names(dedupe_names(data.frame(x = 1, y = 2))), c("x", "y"), "unique names are untouched")
  df3 <- data.frame(a = 1, b = 2, c = 3, d = 4)
  names(df3) <- c("se", "se", "se", "t")
  eq(names(dedupe_names(df3)), c("se", "se__2", "se__3", "t"), "three-way duplicates number in order")
}

section("row filtering")
{
  df <- data.frame(Estimate = c(1, 2, 3), Milk = c(1, 0, 1), stringsAsFactors = FALSE)
  eq(nrow(apply_row_filter(df, "Milk == 1")), 2L, "a row filter selects the subgroup")
  eq(nrow(apply_row_filter(df, NULL)), 3L, "no filter leaves the frame untouched")
  eq(nrow(apply_row_filter(df, "")), 3L, "an empty filter leaves the frame untouched")
  throws(apply_row_filter(df, "Milk == 9"), "selected no rows", "a filter matching nothing fails loudly")
  throws(apply_row_filter(df, "Estimate"), "one logical per row", "a non-logical filter is rejected")
  throws(apply_row_filter(df, "nope == 1"), "failed", "a filter naming an absent column fails loudly")

  # NA in the filter column must drop the row rather than propagate an NA index.
  df2 <- data.frame(Estimate = c(1, 2), Milk = c(1, NA))
  eq(nrow(apply_row_filter(df2, "Milk == 1")), 1L, "NA in the filter column drops the row")
}

section("prepare_for_artma")
{
  raw <- data.frame(PCC = c(0.1, 0.2, NA, 0.4), PCCSE = c(0.01, 0, 0.03, 0.04),
                    ID = c("a", "b", "c", "d"), N = c(10, 20, 30, 40), stringsAsFactors = FALSE)
  cmap <- list(effect = "PCC", se = "PCCSE", study_id = "ID", n_obs = "N")
  out <- prepare_for_artma(raw, cmap)
  eq(nrow(out), 2L, "rows with NA effect or non-positive se are dropped")
  eq(attr(out, "n_dropped"), 2L, "the drop count is recorded")
  eq(names(out), c("effect", "se", "study_id", "n_obs"), "columns are renamed to artma's canonical names")
  eq(out$study_id, c("a", "d"), "study_id survives as a character key")

  throws(prepare_for_artma(raw, utils::modifyList(cmap, list(effect = "nope"))),
         "not found", "a missing source column is reported by name")
  allbad <- data.frame(PCC = c(NA, NA), PCCSE = c(1, 1), ID = c("a", "b"), N = c(1, 1))
  throws(prepare_for_artma(allbad, cmap), "no usable rows", "an all-unusable dataset fails loudly")
}

section("summary rendering")
{
  m <- validate_manifest(good_manifest())
  res <- list(list(
    manifest = m, n_used = 100L, n_dropped = 3L, error = NULL,
    claims = data.frame(
      claim_id = "mean", label = "Mean effect", source = "Table 4.1, p. 30",
      method = "effect_summary_stats", model = "All Data", term = "mean",
      reported = 0.1, artma = 0.102, artma_se = 0.01, diff = 0.002,
      verdict = "match", note = NA_character_, candidates = "", stringsAsFactors = FALSE)
  ))
  md <- render_summary(res, generated_at = "2026-01-01")
  ok(any(grepl("^# Replication summary", md)), "summary has a title")
  ok(any(grepl("Nov\u00e1k, Jan", md)), "summary names the author")
  ok(any(grepl("Claims checked: \\*\\*1\\*\\*", md)), "summary counts the claims")
  ok(any(grepl("\\| Mean effect \\|", md)), "summary renders the claim row")
  ok(any(grepl("100 estimates used", md)), "summary reports the fitted sample size")
  ok(any(grepl("3 rows dropped", md)), "summary reports dropped rows")

  failed <- list(list(manifest = m, error = "dataset 404", claims = NULL,
                      n_used = NA_integer_, n_dropped = NA_integer_))
  md2 <- render_summary(failed)
  ok(any(grepl("Replication failed.*dataset 404", md2)), "a failed thesis is reported, not hidden")

  eq(fmt_num(NA_real_), "--", "NA formats as a dash")
  eq(fmt_num(0.12345, 3), "0.123", "numbers round for display")

  # Agreement pools match+close and ignores unresolved claims, so a manifest bug
  # can never be counted as a disagreement with the thesis.
  mixed <- list(list(manifest = m, n_used = 10L, n_dropped = 0L, error = NULL,
    claims = data.frame(
      claim_id = c("a", "b", "c", "d"), label = "l", source = "Table 1, p. 1",
      method = "linear_tests", model = c("ols", "ols", "fe", "fe"),
      term = c("effect", "publication_bias", "effect", "publication_bias"),
      reported = 1, artma = 1, artma_se = NA_real_, diff = 0,
      verdict = c("match", "close", "mismatch", "unmatched"),
      note = NA_character_, candidates = "", stringsAsFactors = FALSE)))
  tab <- agreement_by(mixed, "model", method_filter = "linear_tests")
  eq(tab$key, c("fe", "ols"), "agreement is grouped by model")
  eq(tab$n, c(1L, 2L), "unresolved claims are excluded from the denominator")
  eq(tab$pct, c(0, 100), "match and close both count as agreement")
  ok(is.null(agreement_by(list(), "model")), "no claims yields no agreement table")
  ok(length(render_agreement_table(NULL, "h", "l")) == 0L, "an absent table renders nothing")
}

section("real manifests")
{
  mdir <- file.path(HERE, "manifests")
  files <- list.files(mdir, pattern = "\\.ya?ml$", full.names = TRUE)
  if (length(files) == 0L) {
    cat("  (no manifests yet - skipping)\n")
  } else {
    ms <- tryCatch(load_manifests(mdir), error = function(e) e)
    ok(!inherits(ms, "error"),
       paste0("every manifest in manifests/ validates",
              if (inherits(ms, "error")) paste0(" -- ", conditionMessage(ms)) else ""))
    if (!inherits(ms, "error")) {
      ids <- vapply(ms, function(m) m$id, character(1))
      ok(!anyDuplicated(ids), "manifest ids are unique across the directory")
      ok(all(vapply(ms, function(m) length(m$claims) > 0L, logical(1))),
         "every manifest carries at least one claim")
      stems <- tools::file_path_sans_ext(basename(files))
      ok(identical(sort(ids), sort(stems)), "each manifest's id matches its filename")
    }
  }
}

cat(sprintf("\n%d passed, %d failed\n", .passed, length(.failed)))
if (length(.failed)) {
  cat("\nFailures:\n"); for (f in .failed) cat("  - ", f, "\n", sep = "")
  quit(status = 1)
}
quit(status = 0)
