# Matching manifest claims against artma's estimates frame, and scoring them.
#
# artma returns one long-format `estimates` frame per method with the shared
# schema (method, model, term, estimate, std_error, ...). A claim names the
# method plus regexes for `model` and `term`; this file resolves that to a
# single row and compares the number against what the thesis reported.

REPLICATION_VERDICTS <- c("match", "close", "mismatch", "unmatched", "ambiguous", "error")

#' Bind the per-method estimates frames from an artma() result into one frame.
#'
#' Methods that produced no estimates (plot-only methods, or ones that skipped)
#' contribute nothing rather than a row of NAs.
collect_estimates <- function(result) {
  frames <- lapply(names(result), function(nm) {
    e <- result[[nm]]$estimates
    if (!is.data.frame(e) || nrow(e) == 0L) return(NULL)
    if (!"method" %in% names(e) || all(is.na(e$method))) e$method <- nm
    e
  })
  frames <- Filter(Negate(is.null), frames)
  if (length(frames) == 0L) return(empty_estimates())
  keep <- c("method", "model", "term", "estimate", "std_error", "p_value",
            "conf_low", "conf_high", "n_obs")
  frames <- lapply(frames, function(f) {
    for (k in setdiff(keep, names(f))) f[[k]] <- NA
    f[, keep, drop = FALSE]
  })
  do.call(rbind, frames)
}

empty_estimates <- function() {
  data.frame(
    method = character(0), model = character(0), term = character(0),
    estimate = numeric(0), std_error = numeric(0), p_value = numeric(0),
    conf_low = numeric(0), conf_high = numeric(0), n_obs = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Resolve one claim to a single estimates row.
#'
#' Returns a list with `status` ("ok"/"unmatched"/"ambiguous"), the matched
#' `row` when unambiguous, and `candidates`: the model/term pairs available for
#' that method. The candidate list is what makes an unmatched claim actionable —
#' it tells the manifest author what artma actually emitted.
match_claim <- function(estimates, claim) {
  pool <- estimates[!is.na(estimates$method) & estimates$method == claim$method, , drop = FALSE]
  candidates <- if (nrow(pool)) {
    unique(paste0(ifelse(is.na(pool$model), "<NA>", pool$model), " / ", pool$term))
  } else {
    character(0)
  }
  if (nrow(pool) == 0L) {
    return(list(status = "unmatched", row = NULL, candidates = candidates,
                reason = sprintf("method '%s' produced no estimates", claim$method)))
  }

  model_chr <- ifelse(is.na(pool$model), "", as.character(pool$model))
  hit <- grepl(claim$artma_model, model_chr) & grepl(claim$artma_term, as.character(pool$term))
  sel <- pool[hit, , drop = FALSE]

  if (nrow(sel) == 0L) {
    return(list(status = "unmatched", row = NULL, candidates = candidates,
                reason = "no model/term matched the regexes"))
  }
  if (nrow(sel) > 1L) {
    return(list(status = "ambiguous", row = NULL, candidates = candidates,
                reason = sprintf("%d rows matched; tighten the regexes", nrow(sel))))
  }
  list(status = "ok", row = sel[1, , drop = FALSE], candidates = candidates, reason = NA_character_)
}

#' Score a reported number against artma's value.
#'
#' `match` when inside tolerance; `close` when within three times tolerance and
#' the sign agrees; `mismatch` otherwise. Sign disagreement is never `close` —
#' a bias-corrected effect that flips sign is a substantively different finding.
score_claim <- function(reported, actual, tolerance) {
  if (!is.finite(actual)) {
    return(list(verdict = "error", diff = NA_real_, threshold = NA_real_))
  }
  thr <- max(tolerance$abs, tolerance$rel * abs(reported))
  diff <- actual - reported
  same_sign <- (reported == 0 & actual == 0) | (sign(reported) == sign(actual))
  verdict <- if (abs(diff) <= thr) {
    "match"
  } else if (abs(diff) <= 3 * thr && same_sign) {
    "close"
  } else {
    "mismatch"
  }
  list(verdict = verdict, diff = diff, threshold = thr)
}

#' Evaluate every claim in a manifest against a collected estimates frame.
#'
#' Always returns one row per claim, so a failed match is visible in the summary
#' rather than silently dropped.
evaluate_claims <- function(manifest, estimates) {
  rows <- lapply(manifest$claims, function(cl) {
    mt <- match_claim(estimates, cl)
    if (mt$status != "ok") {
      return(data.frame(
        claim_id = cl$id, label = cl$label, source = cl$source,
        method = cl$method, model = NA_character_, term = NA_character_,
        reported = cl$reported, artma = NA_real_, artma_se = NA_real_,
        diff = NA_real_, verdict = mt$status, note = mt$reason,
        candidates = paste(mt$candidates, collapse = "; "),
        stringsAsFactors = FALSE
      ))
    }
    sc <- score_claim(cl$reported, mt$row$estimate, cl$tolerance)
    data.frame(
      claim_id = cl$id, label = cl$label, source = cl$source,
      method = cl$method,
      model = as.character(mt$row$model), term = as.character(mt$row$term),
      reported = cl$reported, artma = mt$row$estimate, artma_se = mt$row$std_error,
      diff = sc$diff, verdict = sc$verdict, note = NA_character_,
      candidates = "", stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
