# Matching artma estimates against reported numbers, and the verdict logic.
#
# Like lib/manifest.R this layer needs nothing beyond base R, so the whole
# comparison pipeline can be exercised without artma installed.

# `replicated` means within TOL_EXACT of the reported value (relative),
# `close` within TOL_CLOSE. Point estimates rarely reproduce bit-for-bit:
# software versions, cluster definitions and bootstrap seeds all move the last
# digits, so `replicated` is a tolerance, not an equality test.
TOL_EXACT <- 0.01
TOL_CLOSE <- 0.10

#' The shared estimates schema every artma method emits.
#' @keywords internal
ESTIMATES_COLUMNS <- c(
  "method", "model", "term", "estimate", "std_error", "statistic", "p_value",
  "conf_low", "conf_high", "n_obs", "n_clusters", "note"
)

#' A zero-row frame in the shared estimates schema.
empty_estimates <- function() {
  frame <- data.frame(
    method = character(0), model = character(0), term = character(0),
    estimate = numeric(0), std_error = numeric(0), statistic = numeric(0),
    p_value = numeric(0), conf_low = numeric(0), conf_high = numeric(0),
    n_obs = integer(0), n_clusters = integer(0), note = character(0),
    stringsAsFactors = FALSE
  )
  frame[, ESTIMATES_COLUMNS]
}

#' Bind the `estimates` frames of every method result into one frame.
#'
#' Methods that produced no numbers (plot-only methods, or an empty frame) are
#' skipped; an all-empty result list yields a zero-row frame, not an error.
#'
#' @param results A named list of method results, each possibly carrying a
#'   data.frame under `$estimates`.
bind_estimates <- function(results) {
  frames <- Filter(
    function(x) is.data.frame(x) && nrow(x) > 0L,
    lapply(results, function(result) result$estimates)
  )

  if (length(frames) == 0L) {
    return(empty_estimates())
  }

  frames <- lapply(frames, function(frame) frame[, ESTIMATES_COLUMNS, drop = FALSE])
  bound <- do.call(rbind, frames)
  rownames(bound) <- NULL
  bound
}

#' Match values against a pattern: exact string match, or anchored regex.
#'
#' The exact branch lets a manifest pin a literal name (`(Intercept)`) without
#' regex escaping; the regex branch is anchored so a substring can never match.
#' @keywords internal
matches_anchored <- function(pattern, values) {
  exact <- values == pattern
  regex <- tryCatch(
    grepl(paste0("^(?:", pattern, ")$"), values, perl = TRUE),
    error = function(e) rep(FALSE, length(values))
  )
  exact | regex
}

#' Look one reported entry up in the bound estimates frame.
#'
#' `artma_method` is matched exactly; `artma_model` and `artma_term` accept an
#' anchored regular expression (or a literal name). A `NULL` `artma_model`
#' applies no model filter, which is only useful when the method emits a single
#' model.
#'
#' @return A list with `hit` (a one-row frame, or `NULL`) and `reason`
#'   (`"matched"`, `"not_produced"`, or `"ambiguous"`).
match_estimate <- function(estimates, entry) {
  hits <- estimates[estimates$method == entry$artma_method, , drop = FALSE]

  if (nrow(hits) > 0L && !is.null(entry$artma_model)) {
    hits <- hits[matches_anchored(entry$artma_model, hits$model), , drop = FALSE]
  }
  if (nrow(hits) > 0L && !is.null(entry$artma_term)) {
    hits <- hits[matches_anchored(entry$artma_term, hits$term), , drop = FALSE]
  }

  if (nrow(hits) == 0L) {
    return(list(hit = NULL, reason = "not_produced"))
  }
  if (nrow(hits) > 1L) {
    return(list(hit = NULL, reason = "ambiguous"))
  }
  list(hit = hits, reason = "matched")
}

#' Verdict for one (author, artma) pair of point estimates.
verdict <- function(author, artma) {
  if (is.na(author) || is.na(artma)) {
    return("unknown")
  }
  if (author != 0 && artma != 0 && sign(author) != sign(artma)) {
    return("sign flip")
  }

  relative <- if (author == 0) {
    if (artma == 0) 0 else Inf
  } else {
    abs(artma - author) / abs(author)
  }

  if (relative <= TOL_EXACT) {
    "replicated"
  } else if (relative <= TOL_CLOSE) {
    "close"
  } else {
    "differs"
  }
}

#' Do two normal-approximation confidence intervals overlap?
#'
#' @return `TRUE`/`FALSE`, or `NA` when any input is missing.
ci_overlap <- function(est_a, se_a, est_b, se_b, level = 0.95) {
  inputs <- c(est_a, se_a, est_b, se_b)
  if (length(inputs) != 4L || anyNA(inputs)) {
    return(NA)
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  isTRUE(est_a - z * se_a <= est_b + z * se_b && est_b - z * se_b <= est_a + z * se_a)
}

#' A zero-row comparison frame.
empty_comparison <- function() {
  data.frame(
    thesis_id = character(0), quantity = character(0),
    author_estimate = numeric(0), author_se = numeric(0),
    artma_estimate = numeric(0), artma_se = numeric(0),
    difference = numeric(0), rel_diff_pct = numeric(0),
    ci_overlap = logical(0), verdict = character(0), source = character(0),
    stringsAsFactors = FALSE
  )
}

#' Compare one manifest's reported numbers against a bound estimates frame.
#'
#' @return One row per reported entry. `difference` is artma minus author;
#'   `rel_diff_pct` is that difference as a signed percentage of the author's
#'   absolute value.
compare_manifest <- function(manifest, estimates) {
  rows <- lapply(manifest$reported, function(entry) {
    if (nrow(estimates) == 0L) {
      hit <- NULL
      reason <- "no_estimates"
    } else {
      matched <- match_estimate(estimates, entry)
      hit <- matched$hit
      reason <- matched$reason
    }

    artma_estimate <- if (is.null(hit)) NA_real_ else hit$estimate
    artma_se <- if (is.null(hit)) NA_real_ else hit$std_error
    author_se <- entry$std_error %||% NA_real_

    row_verdict <- if (identical(reason, "matched")) {
      verdict(entry$estimate, artma_estimate)
    } else {
      reason
    }

    difference <- artma_estimate - entry$estimate
    rel_diff_pct <- if (is.na(difference) || entry$estimate == 0) {
      NA_real_
    } else {
      100 * difference / abs(entry$estimate)
    }

    data.frame(
      thesis_id = manifest$id,
      quantity = entry$label,
      author_estimate = entry$estimate,
      author_se = author_se,
      artma_estimate = artma_estimate,
      artma_se = artma_se,
      difference = difference,
      rel_diff_pct = rel_diff_pct,
      ci_overlap = ci_overlap(entry$estimate, author_se, artma_estimate, artma_se),
      verdict = row_verdict,
      source = entry$source %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  if (length(rows) == 0L) {
    return(empty_comparison())
  }
  bound <- do.call(rbind, rows)
  rownames(bound) <- NULL
  bound
}

#' Format one point estimate, with its standard error when known.
#' @keywords internal
format_estimate <- function(estimate, std_error) {
  if (is.na(estimate)) {
    return("--")
  }
  if (is.na(std_error)) {
    return(sprintf("%.4f", estimate))
  }
  sprintf("%.4f (%.4f)", estimate, std_error)
}

#' Render a comparison frame as one markdown table.
#'
#' When the frame spans several manifests (the combined summary), a leading
#' Study column keys each row.
comparison_markdown <- function(comparison) {
  if (nrow(comparison) == 0L) {
    return("No reported quantities transcribed yet.")
  }

  multi <- length(unique(comparison$thesis_id)) > 1L

  header <- c(if (multi) "Study", "Quantity", "Author", "artma", "Rel. diff", "Verdict")

  rows <- vapply(seq_len(nrow(comparison)), function(i) {
    row <- comparison[i, ]
    rel <- if (is.na(row$rel_diff_pct)) "--" else sprintf("%+.1f%%", row$rel_diff_pct)
    cells <- c(
      if (multi) row$thesis_id,
      row$quantity,
      format_estimate(row$author_estimate, row$author_se),
      format_estimate(row$artma_estimate, row$artma_se),
      rel,
      row$verdict
    )
    paste0("| ", paste(cells, collapse = " | "), " |")
  }, character(1))

  paste(
    c(
      paste0("| ", paste(header, collapse = " | "), " |"),
      paste0("| ", paste(rep("---", length(header)), collapse = " | "), " |"),
      rows
    ),
    collapse = "\n"
  )
}
