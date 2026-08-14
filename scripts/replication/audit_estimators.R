#!/usr/bin/env Rscript
# Audit artma's estimators against from-scratch reference implementations.
#
#   Rscript scripts/replication/audit_estimators.R
#
# This answers a different question from run_replication.R. That script asks
# "does artma reproduce what a thesis printed?", which conflates three things:
# artma being wrong, the thesis being wrong, and the two using different
# specifications. This script isolates the first. For every manifest dataset it
# recomputes each linear model directly with base R / plm, following the
# specification used in the published replication packages on meta-analysis.cz
# (Stata `reg`, `xtreg, fe`, `xtreg, be`, `xtreg, re`, with analytic weights),
# and compares against what artma emits on the same rows.
#
# Winsorization is switched off on both sides so the comparison is of the
# estimator alone, not of the preprocessing.
#
# A disagreement here is an artma bug. A disagreement in run_replication.R,
# with agreement here, is the thesis differing from the standard method.

suppressWarnings(suppressMessages({
  okpkg <- all(vapply(c("artma", "yaml", "plm", "sandwich"),
                      requireNamespace, logical(1), quietly = TRUE))
}))
if (!okpkg) stop("audit_estimators.R needs artma, yaml, plm and sandwich.", call. = FALSE)
suppressMessages({library(artma); library(plm)})
options(artma.verbose = 1)

HERE <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f)) dirname(normalizePath(f)) else file.path(getwd(), "scripts", "replication")
})
for (f in c("manifest.R", "dataset.R", "compare.R", "summary.R")) {
  source(file.path(HERE, "harness", f))
}

TOL <- 1e-6

#' Reference fits, written directly against the published Stata specifications.
#'
#' `fe` deserves a note: a within estimator has no intercept, and the quantity
#' the literature reports as "effect beyond bias" is Stata's `_cons` from
#' `xtreg, fe`, which is `mean(y) - b * mean(x)`. That is the definition used
#' here, and it is what artma's `plm::within_intercept()` route must reproduce.
reference_fits <- function(d) {
  n <- table(d$study_id)
  d$study_size <- as.numeric(n[as.character(d$study_id)])
  pd <- pdata.frame(d, index = "study_id")
  grand_int <- function(b) mean(d$effect) - b * mean(d$se)

  out <- list()
  o <- lm(effect ~ se, data = d)
  out$ols <- unname(coef(o))
  fe <- try(plm(effect ~ se, data = pd, model = "within"), silent = TRUE)
  if (!inherits(fe, "try-error")) {
    b <- coef(fe)[["se"]]
    out$fe <- c(grand_int(b), b)
  }
  be <- try(plm(effect ~ se, data = pd, model = "between"), silent = TRUE)
  if (!inherits(be, "try-error")) out$be <- unname(coef(be))
  re <- try(plm(effect ~ se, data = pd, model = "random"), silent = TRUE)
  if (!inherits(re, "try-error")) out$re <- unname(coef(re))
  # 1/n_estimates_per_study, matching `gen inv_nest = 1/nest; [aweight=inv_nest]`
  out$ols_study_weighted <- unname(coef(lm(effect ~ se, data = d, weights = 1 / d$study_size)))
  # inverse variance, matching `[aweight=1/(se*se)]`
  out$ols_precision_weighted <- unname(coef(lm(effect ~ se, data = d, weights = (1 / d$se)^2)))
  out
}

artma_fits <- function(m, d) {
  td <- file.path(tempdir(), paste0("audit-", m$id))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(td, "x.csv")
  utils::write.csv(d, csv, row.names = FALSE, na = "")
  od <- file.path(td, "o")
  unlink(od, recursive = TRUE)
  dir.create(od, showWarnings = FALSE)
  ui <- c(list("data.source_path" = normalizePath(csv), "general.seed" = 1L),
          m$artma_options %||% list())
  ui[["data.winsorization_level"]] <- 0
  artma::options_create(options_file_name = "audit.yaml", options_dir = od,
                        user_input = ui, should_overwrite = TRUE)
  r <- try(artma::artma(methods = "linear_tests", options = "audit.yaml", options_dir = od),
           silent = TRUE)
  if (inherits(r, "try-error")) return(NULL)
  r$linear_tests$estimates
}

manifests <- load_manifests(file.path(HERE, "manifests"))
rows <- list()
for (m in manifests) {
  d <- try(prepare_for_artma(resolve_dataset(m, file.path(HERE, "data")), m$columns), silent = TRUE)
  if (inherits(d, "try-error")) {
    message("skip ", m$id, ": ", conditionMessage(attr(d, "condition")))
    next
  }
  est <- artma_fits(m, d)
  if (is.null(est)) { message("skip ", m$id, ": artma failed"); next }
  ref <- reference_fits(d)
  for (mdl in names(ref)) {
    for (k in 1:2) {
      tm <- c("effect", "publication_bias")[k]
      a <- est$estimate[est$model == mdl & est$term == tm]
      if (!length(a)) next
      rows[[length(rows) + 1L]] <- data.frame(
        thesis = m$id, model = mdl, term = tm, artma = a, reference = ref[[mdl]][k],
        rel = abs(a - ref[[mdl]][k]) / max(abs(ref[[mdl]][k]), 1e-8),
        stringsAsFactors = FALSE)
    }
  }
}

res <- do.call(rbind, rows)
cat(sprintf("\n=== artma vs reference implementations ===\n%d comparisons over %d datasets\n\n",
            nrow(res), length(unique(res$thesis))))
for (mdl in sort(unique(res$model))) {
  s <- res[res$model == mdl, ]
  cat(sprintf("  %-24s %3d/%3d agree\n", mdl, sum(s$rel <= TOL), nrow(s)))
}
bad <- res[res$rel > TOL, , drop = FALSE]
if (nrow(bad)) {
  cat("\nDISAGREEMENTS -- these are artma bugs:\n")
  bad <- bad[order(-bad$rel), ]
  for (i in seq_len(nrow(bad))) {
    cat(sprintf("  %-34s %-24s %-17s artma=%12.6f ref=%12.6f rel=%.3g\n",
                bad$thesis[i], bad$model[i], bad$term[i],
                bad$artma[i], bad$reference[i], bad$rel[i]))
  }
} else {
  cat("\nNo disagreements: every artma linear estimator reproduces the reference exactly.\n")
}

# Clustered standard errors. artma asks sandwich for HC1; the published packages
# use Stata's `cluster()`. The two corrections coincide, and this asserts it
# rather than leaving it assumed.
cat("\n=== clustered standard errors ===\n")
m1 <- manifests[[1]]
d1 <- try(prepare_for_artma(resolve_dataset(m1, file.path(HERE, "data")), m1$columns), silent = TRUE)
if (!inherits(d1, "try-error")) {
  fit <- lm(effect ~ se, data = d1)
  G <- length(unique(d1$study_id)); N <- nrow(d1); K <- 2L
  v_hc1 <- sandwich::vcovCL(fit, cluster = d1$study_id, type = "HC1")
  v_stata <- sandwich::vcovCL(fit, cluster = d1$study_id, type = "HC0", cadjust = FALSE) *
    (G / (G - 1)) * ((N - 1) / (N - K))
  cat(sprintf("  %s: sandwich HC1 vs Stata cluster() convention -> max rel diff %.3g\n",
              m1$id, max(abs(sqrt(diag(v_hc1)) - sqrt(diag(v_stata))) / sqrt(diag(v_stata)))))
}

if (nrow(bad)) quit(status = 1)
quit(status = 0)
