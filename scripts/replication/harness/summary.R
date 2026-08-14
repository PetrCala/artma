# Rendering the replication results as SUMMARY.md.

VERDICT_MARK <- c(
  match = "match", close = "close", mismatch = "MISMATCH",
  unmatched = "no match", ambiguous = "ambiguous", error = "error"
)

fmt_num <- function(x, digits = 4) {
  if (length(x) == 0L || is.na(x) || !is.finite(x)) return("--")
  formatC(x, format = "f", digits = digits)
}

md_escape <- function(x) gsub("|", "\\|", x, fixed = TRUE)

#' One markdown section per thesis: provenance, then the claim table.
render_thesis_section <- function(res) {
  m <- res$manifest
  th <- m$thesis
  out <- c(
    sprintf("### %s (%d) \u2014 %s", th$author, th$year, md_escape(th$title)),
    "",
    sprintf("- **Degree**: %s thesis, supervised by %s", th$degree, th$advisor),
    sprintf("- **Repository**: [%s](%s)", th$handle,
            th$url %||% paste0("https://dspace.cuni.cz/handle/", th$handle)),
    sprintf("- **Dataset**: [%s](%s)%s", basename(sub("\\?.*$", "", m$dataset$url)),
            m$dataset$url,
            if (isTRUE(m$dataset$code_published)) " \u2014 code published alongside" else " \u2014 data only, no code"),
    ""
  )

  if (!is.null(res$error)) {
    return(c(out, sprintf("> **Replication failed**: %s", res$error), ""))
  }

  out <- c(out, sprintf(
    "- **Sample**: %d estimates used%s",
    res$n_used,
    if (res$n_dropped > 0) sprintf(" (%d rows dropped: non-finite or non-positive `se`)", res$n_dropped) else ""
  ), "")

  cl <- res$claims
  header <- c(
    "| Claim | Source in thesis | Reported | artma | Diff | Verdict |",
    "| --- | --- | ---: | ---: | ---: | --- |"
  )
  rows <- vapply(seq_len(nrow(cl)), function(i) {
    r <- cl[i, ]
    sprintf("| %s | %s | %s | %s | %s | %s |",
            md_escape(r$label), md_escape(r$source),
            fmt_num(r$reported), fmt_num(r$artma), fmt_num(r$diff),
            VERDICT_MARK[[r$verdict]])
  }, character(1))
  out <- c(out, header, rows, "")

  bad <- cl[cl$verdict %in% c("unmatched", "ambiguous"), , drop = FALSE]
  if (nrow(bad)) {
    out <- c(out, "<details><summary>Unresolved claims: what artma emitted</summary>", "")
    for (i in seq_len(nrow(bad))) {
      out <- c(out, sprintf("- `%s`: %s", bad$claim_id[i], bad$note[i]),
               sprintf("  - available `model / term` for `%s`: %s",
                       bad$method[i], if (nzchar(bad$candidates[i])) bad$candidates[i] else "(none)"))
    }
    out <- c(out, "", "</details>", "")
  }
  out
}

#' Agreement rate broken down by some column of the claim table.
#'
#' `match` and `close` are pooled: both mean artma landed on the thesis's
#' number, the split between them being how tightly. Only resolved claims count,
#' so a manifest bug cannot masquerade as a disagreement.
agreement_by <- function(results, column, method_filter = NULL) {
  cl <- do.call(rbind, lapply(results, function(r) r$claims))
  if (is.null(cl)) return(NULL)
  cl <- cl[cl$verdict %in% c("match", "close", "mismatch"), , drop = FALSE]
  if (!is.null(method_filter)) cl <- cl[cl$method == method_filter, , drop = FALSE]
  if (nrow(cl) == 0L) return(NULL)
  keys <- cl[[column]]
  do.call(rbind, lapply(sort(unique(keys)), function(k) {
    sub <- cl[keys == k, , drop = FALSE]
    agree <- sum(sub$verdict %in% c("match", "close"))
    data.frame(key = k, n = nrow(sub), agree = agree,
               pct = round(100 * agree / nrow(sub)), stringsAsFactors = FALSE)
  }))
}

render_agreement_table <- function(tab, heading, label) {
  if (is.null(tab)) return(character(0))
  c(sprintf("**%s**", heading), "",
    sprintf("| %s | Claims | Agreeing | Rate |", label),
    "| --- | ---: | ---: | ---: |",
    sprintf("| `%s` | %d | %d | %d%% |", tab$key, tab$n, tab$agree, tab$pct),
    "")
}

#' Counts by verdict across every claim in the run.
verdict_tally <- function(results) {
  cl <- do.call(rbind, lapply(results, function(r) r$claims))
  if (is.null(cl)) return(setNames(integer(length(REPLICATION_VERDICTS)), REPLICATION_VERDICTS))
  tab <- table(factor(cl$verdict, levels = REPLICATION_VERDICTS))
  setNames(as.integer(tab), names(tab))
}

render_summary <- function(results, generated_at = NULL) {
  ok <- Filter(function(r) is.null(r$error), results)
  tally <- verdict_tally(ok)
  n_claims <- sum(tally)

  head <- c(
    "# Replication summary",
    "",
    paste0(
      "Replication of IES (Charles University) bachelor's and master's meta-analysis ",
      "theses supervised by Tom\u00e1\u0161 Havr\u00e1nek or Zuzana Havr\u00e1nkov\u00e1, ",
      "using `artma`. ",
      "Each thesis's published dataset is re-analysed and the results compared ",
      "against the numbers printed in the thesis."
    ),
    "",
    sprintf("- Theses attempted: **%d** (%d replicated, %d failed)",
            length(results), length(ok), length(results) - length(ok)),
    sprintf("- Claims checked: **%d**", n_claims),
    sprintf("- Verdicts: %s",
            paste(sprintf("%s **%d**", names(tally), tally)[tally > 0], collapse = ", ")),
    ""
  )
  if (!is.null(generated_at)) {
    head <- c(head, sprintf("Generated %s by `scripts/replication/run_replication.R`.", generated_at), "")
  }

  legend <- c(
    "**Verdicts.** `match`: within tolerance (default 0.005 absolute or 5% relative, ",
    "whichever is looser). `close`: within 3x tolerance and the same sign. ",
    "`mismatch`: outside that. `no match`/`ambiguous`: the claim's `artma_model`/",
    "`artma_term` regexes selected zero or several rows \u2014 a manifest bug, not a ",
    "disagreement with the thesis.",
    ""
  )

  where <- c(
    "## Where artma agrees and where it does not",
    "",
    paste0(
      "Pooling `match` and `close` as agreement, across every resolved claim. ",
      "These rates are about *this* set of theses, not a general benchmark, but ",
      "the pattern is consistent enough to be worth reading."
    ),
    "",
    render_agreement_table(
      agreement_by(ok, "term", method_filter = "linear_tests"),
      "By term (linear_tests)", "Term"
    ),
    render_agreement_table(
      agreement_by(ok, "model", method_filter = "linear_tests"),
      "By model (linear_tests)", "Model"
    ),
    render_agreement_table(
      agreement_by(ok, "method"), "By method", "Method"
    )
  )

  body <- unlist(lapply(results, render_thesis_section), use.names = FALSE)
  c(head, paste(legend, collapse = ""), "", where, "## Per-thesis results", "", body)
}

#' Write SUMMARY.md as UTF-8 regardless of the ambient locale.
#'
#' These scripts routinely run under C/POSIX, where a plain `writeLines()` would
#' emit the Czech names and the em dashes as mojibake. Encoding the lines
#' explicitly and writing bytes keeps the file correct everywhere.
write_summary <- function(results, path, generated_at = NULL) {
  lines <- enc2utf8(render_summary(results, generated_at))
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(lines, con, useBytes = TRUE)
  invisible(path)
}
