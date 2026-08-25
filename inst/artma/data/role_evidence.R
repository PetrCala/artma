#' @title Data-evidence layer for column role detection
#' @description
#' Value-based evidence that complements the name-based column matching in
#' `data/column_recognition.R`. Column names are treated as a prior; the
#' functions here score how much the actual values of a column look like a
#' given standard role (effect, se, t_stat, n_obs), detect identifier-like
#' columns (row counters, per-study coefficient counters, years, near-unique
#' integer codes), and check cross-column consistency: whether a candidate
#' (effect, se) pair produces a plausible t-statistic distribution and whether
#' a third column reproduces effect / se. `assign_core_roles()` combines the
#' two signal families into one joint assignment for the numeric core roles so
#' that a mutually consistent triple outranks any name-only match and an
#' identifier column can never be claimed as an effect size just because its
#' name contains "coef".

box::use(artma / const[CONST])

#' Minimum non-missing rows before value-based evidence is trusted. Below this
#' the caller falls back to name-based matching (tiny data frames carry too
#' little distributional information).
MIN_ROWS_FOR_EVIDENCE <- 15L

#' Minimum complete cases for the pair/triple consistency checks.
MIN_COMPLETE_PAIRS <- 15L

#' The roles resolved jointly by `assign_core_roles()`.
CORE_EVIDENCE_ROLES <- c("effect", "se", "t_stat")

#' Thresholds for the confirm-me candidate set (see `assign_core_roles()`).
#' A provisional candidate is never auto-accepted; it is only offered to the
#' user for a single confirmation question in an interactive session, so these
#' bars sit below the acceptance thresholds without loosening them.
#' - min_evidence: value evidence the candidate must reach on its own
#' - min_pair: consistency required with an already-accepted counterpart
#' - min_margin: how close a competitor must sit to the best candidate before
#'   it joins the same question instead of being dropped
#' - tie_margin: how close a runner-up must sit to an accepted column before
#'   the pick counts as a near-tie worth asking about
PROVISIONAL_THRESHOLDS <- list(
  min_evidence = 0.55,
  min_pair = 0.5,
  min_margin = 0.1,
  tie_margin = 0.05
)

clamp01 <- function(x) max(0, min(1, x))

#' Stata-style missing-value markers that can survive into a raw text column.
#' Read-time normalization already replaces these with `NA` for every input
#' format (`replace_stata_missing`), so this is a backstop for callers that
#' profile a frame which did not come through `normalize_read_df`: a genuinely
#' numeric column exported from Stata with real missingness is not mistaken
#' for a non-numeric one just because its NA values are spelled ".".
STATA_MISSING_PATTERN <- CONST$DATA$STATA_MISSING_PATTERN

#' @title Coerce a column to numeric when it plausibly holds numbers
#' @description Returns a numeric vector when the input is numeric or when at
#'   least 90 percent of its non-empty, non-missing-marker values parse as
#'   numbers (Excel files are read as text). Stata-style missing markers
#'   (`.`, `.a`-`.z`) are treated as missing rather than as failed parses, so
#'   they do not drag a genuinely numeric column below the coverage floor.
#'   Returns NULL for genuinely non-numeric columns.
#' @param values *\[vector\]* Column values.
#' @return *\[numeric|NULL\]* The coerced values, or NULL.
coerce_numeric_column <- function(values) {
  if (is.numeric(values)) {
    return(values)
  }
  if (is.logical(values)) {
    return(as.numeric(values))
  }
  if (is.factor(values)) {
    values <- as.character(values)
  }
  if (!is.character(values)) {
    return(NULL)
  }
  trimmed <- trimws(values)
  is_missing_marker <- grepl(STATA_MISSING_PATTERN, trimmed, ignore.case = TRUE)
  non_empty <- !is.na(values) & nzchar(trimmed) & !is_missing_marker
  if (!any(non_empty)) {
    return(NULL)
  }
  converted <- suppressWarnings(as.numeric(values))
  converted[is_missing_marker] <- NA_real_
  if (sum(!is.na(converted[non_empty])) / sum(non_empty) < 0.9) {
    return(NULL)
  }
  converted
}

#' @title Profile a column for role-evidence scoring
#' @description Computes the distributional facts the role scorers need:
#'   integer share, sign shares, uniqueness, and the identifier-pattern flags
#'   (global sequence, arithmetic progression, per-group counter, year-like
#'   values, near-unique integer codes).
#' @param values *\[vector\]* Column values.
#' @return *\[list|NULL\]* Profile fields, or NULL when the column is not
#'   numeric-like or has no finite values.
profile_role_values <- function(values) {
  numeric_values <- coerce_numeric_column(values)
  if (is.null(numeric_values)) {
    return(NULL)
  }
  x <- numeric_values[!is.na(numeric_values) & is.finite(numeric_values)]
  n <- length(x)
  if (n == 0) {
    return(NULL)
  }

  n_distinct <- length(unique(x))
  integer_share <- mean(abs(x - round(x)) < 1e-9)
  non_integer_share <- 1 - integer_share
  negative_share <- mean(x < 0)
  positive_share <- mean(x > 0)
  zero_share <- mean(x == 0)
  abs_x <- abs(x)

  d <- if (n >= 2) diff(x) else numeric(0)
  step_one <- if (length(d)) abs(d - 1) < 1e-9 else logical(0)
  resets <- if (length(d)) d < -0.5 else logical(0)

  # Global sequence with a constant step of one (1, 2, 3, ...).
  is_sequential <- n >= 3 && all(step_one)
  # Any constant nonzero step (10, 20, 30, ...) is equally identifier-like.
  is_arithmetic <- n >= 3 &&
    n_distinct > 1 &&
    length(d) > 0 &&
    all(abs(d - d[1]) < 1e-9) &&
    abs(d[1]) > 1e-12
  # Counter restarting at 1 within groups (like a per-study coefficient index):
  # steps of one dominated by occasional resets back down.
  is_within_group_counter <- n >= 10 &&
    integer_share > 0.99 &&
    abs(min(x) - 1) < 1e-9 &&
    length(d) > 0 &&
    mean(step_one) >= 0.5 &&
    mean(step_one | resets) >= 0.98 &&
    sum(resets) >= 2
  is_year_like <- integer_share > 0.99 &&
    min(x) >= 1500 &&
    max(x) <= 2100 &&
    n_distinct <= 200
  is_near_unique_integer <- n >= 10 &&
    integer_share > 0.99 &&
    n_distinct / n >= 0.95

  list(
    n = n,
    coverage = n / length(values),
    n_distinct = n_distinct,
    uniqueness_ratio = n_distinct / n,
    integer_share = integer_share,
    non_integer_share = non_integer_share,
    negative_share = negative_share,
    positive_share = positive_share,
    zero_share = zero_share,
    has_both_signs = negative_share > 0 && positive_share > 0,
    min = min(x),
    max = max(x),
    median = stats::median(x),
    median_abs = stats::median(abs_x),
    share_abs_gt_100 = mean(abs_x > 100),
    is_sequential = is_sequential,
    is_arithmetic = is_arithmetic,
    is_within_group_counter = is_within_group_counter,
    is_year_like = is_year_like,
    is_near_unique_integer = is_near_unique_integer,
    is_id_like = is_sequential || is_arithmetic || is_within_group_counter ||
      is_year_like || is_near_unique_integer
  )
}

score_effect_evidence <- function(p) {
  if (p$is_id_like || p$n_distinct <= 2) {
    return(0)
  }
  score <- 0.6
  if (p$has_both_signs) score <- score + 0.15
  if (p$non_integer_share >= 0.5) {
    score <- score + 0.2
  } else if (p$non_integer_share >= 0.2) {
    score <- score + 0.1
  }
  if (p$uniqueness_ratio >= 0.25) score <- score + 0.05
  # Small sets of pure integers look like category codes, not effect sizes.
  if (p$integer_share > 0.95 && p$n_distinct <= 20) score <- score - 0.35
  clamp01(score)
}

score_se_evidence <- function(p) {
  if (p$is_id_like || p$n_distinct <= 2) {
    return(0)
  }
  # With hundreds of rows, a standard-error column has many distinct values
  # even under heavy rounding; a handful of levels is a flag, a category code
  # or a bound indicator (values like 1/2), no matter how se-like its name is.
  if (p$n >= 100 && p$n_distinct <= 10) {
    return(0.1)
  }
  score <- 0.55
  if (p$negative_share > 0.005) {
    score <- score - 0.5
  } else {
    score <- score + 0.2
  }
  if (p$zero_share > 0.2) score <- score - 0.2
  if (p$non_integer_share >= 0.5) {
    score <- score + 0.2
  } else if (p$non_integer_share >= 0.2) {
    score <- score + 0.1
  }
  clamp01(score)
}

score_t_stat_evidence <- function(p) {
  if (p$is_id_like || p$n_distinct <= 2) {
    return(0)
  }
  score <- 0.5
  if (p$has_both_signs) score <- score + 0.15
  if (p$median_abs >= 0.2 && p$median_abs <= 15) score <- score + 0.2
  if (p$share_abs_gt_100 > 0.02) score <- score - 0.4
  if (p$non_integer_share >= 0.5) score <- score + 0.15
  clamp01(score)
}

score_n_obs_evidence <- function(p) {
  if (p$is_sequential || p$is_arithmetic || p$is_within_group_counter) {
    return(0)
  }
  if (p$n_distinct <= 1) {
    return(0)
  }
  score <- 0.5
  if (p$integer_share >= 0.95) {
    score <- score + 0.2
  } else if (p$non_integer_share > 0.5) {
    score <- score - 0.3
  }
  if (p$min >= 5) score <- score + 0.1
  if (p$min <= 0) score <- score - 0.4
  if (p$median >= 20) score <- score + 0.1
  if (p$uniqueness_ratio <= 0.8) score <- score + 0.05
  clamp01(score)
}

#' @title Score how much a column's values look like a given role
#' @description Distribution-based plausibility in \[0, 1\]. 0 means the values
#'   contradict the role (an identifier posing as an effect size), values near
#'   1 mean the distribution is exactly what the role predicts. Returns NA when
#'   there is too little data to judge (fewer than `MIN_ROWS_FOR_EVIDENCE`
#'   usable values) so callers can fall back to name evidence alone.
#' @param values *\[vector\]* Column values.
#' @param role *\[character\]* One of "effect", "se", "t_stat", "n_obs".
#' @return *\[numeric\]* Evidence score in \[0, 1\], or NA.
score_role_evidence <- function(values, role) {
  p <- profile_role_values(values)
  if (is.null(p)) {
    # A non-numeric column is conclusive counter-evidence for numeric roles.
    return(0)
  }
  if (p$n < MIN_ROWS_FOR_EVIDENCE) {
    return(NA_real_)
  }
  score <- switch(role,
    effect = score_effect_evidence(p),
    se = score_se_evidence(p),
    t_stat = score_t_stat_evidence(p),
    n_obs = score_n_obs_evidence(p),
    NA_real_
  )
  # A column populated on a small fraction of rows must not out-rank a full
  # alternative on the strength of the few values it does have (a 98%-empty
  # se column is a bad mapping even when those 2% look perfect). The discount
  # is quadratic so moderate missingness is tolerated and severe sparsity is
  # decisive.
  if (!is.na(score) && p$coverage < 0.5) {
    score <- score * (p$coverage / 0.5)^2
  }
  score
}

#' @title Score the internal consistency of a candidate (effect, se) pair
#' @description Checks that dividing the candidate effect by the candidate
#'   standard error yields a plausible t-statistic distribution and that the
#'   two columns live on compatible scales. Returns NA when there are too few
#'   complete cases to judge.
#' @param effect_values *\[vector\]* Candidate effect column values.
#' @param se_values *\[vector\]* Candidate standard-error column values.
#' @return *\[numeric\]* Consistency score in \[0, 1\], or NA.
score_pair_consistency <- function(effect_values, se_values) {
  e <- coerce_numeric_column(effect_values)
  s <- coerce_numeric_column(se_values)
  if (is.null(e) || is.null(s) || length(e) != length(s)) {
    return(NA_real_)
  }
  both_finite <- !is.na(e) & !is.na(s) & is.finite(e) & is.finite(s)
  usable <- both_finite & s > 0
  if (sum(usable) < MIN_COMPLETE_PAIRS) {
    return(NA_real_)
  }
  # A real standard-error column is positive almost everywhere.
  if (sum(usable) / sum(both_finite) < 0.9) {
    return(0)
  }
  t_implied <- e[usable] / s[usable]
  med_abs_t <- stats::median(abs(t_implied))

  score <- 0
  if (med_abs_t >= 0.05 && med_abs_t <= 30) score <- score + 0.5
  if (mean(abs(t_implied) <= 50) >= 0.95) score <- score + 0.25
  effect_spread <- stats::sd(e[usable])
  if (is.finite(effect_spread) && effect_spread > 0) {
    scale_ratio <- stats::median(s[usable]) / effect_spread
    if (scale_ratio >= 0.01 && scale_ratio <= 30) score <- score + 0.25
  }
  score
}

#' @title Score whether a third column reproduces effect / se
#' @description The share of rows where the candidate t-statistic column
#'   matches the implied ratio effect / se within a 5 percent tolerance
#'   (rounded published values pass). A column storing absolute t-statistics
#'   scores slightly lower via the sign-blind comparison. Returns NA when there
#'   are too few complete cases.
#' @param effect_values *\[vector\]* Candidate effect column values.
#' @param se_values *\[vector\]* Candidate standard-error column values.
#' @param t_values *\[vector\]* Candidate t-statistic column values.
#' @return *\[numeric\]* Consistency score in \[0, 1\], or NA.
score_triple_consistency <- function(effect_values, se_values, t_values) {
  e <- coerce_numeric_column(effect_values)
  s <- coerce_numeric_column(se_values)
  t <- coerce_numeric_column(t_values)
  if (is.null(e) || is.null(s) || is.null(t) ||
    length(e) != length(s) || length(e) != length(t)) {
    return(NA_real_)
  }
  usable <- !is.na(e) & !is.na(s) & !is.na(t) &
    is.finite(e) & is.finite(s) & is.finite(t) & s > 0
  if (sum(usable) < MIN_COMPLETE_PAIRS) {
    return(NA_real_)
  }
  implied <- e[usable] / s[usable]
  denom <- pmax(abs(implied), 1)
  signed_match <- mean(abs(t[usable] - implied) / denom < 0.05)
  abs_match <- mean(abs(abs(t[usable]) - abs(implied)) / denom < 0.05) * 0.9
  max(signed_match, abs_match)
}

#' Combine a name prior with value evidence for one role and column.
#' When evidence is NA (too little data) the name score stands alone.
#' @keywords internal
combine_name_and_evidence <- function(name_score, evidence_score) {
  if (is.na(evidence_score)) {
    return(name_score)
  }
  min(1, 0.5 * name_score + 0.5 * evidence_score)
}

#' @title Jointly assign the numeric core roles (effect, se, t_stat)
#' @description Scores candidate assignments of data columns to the core roles
#'   so that the mutually consistent set wins and no column is claimed twice.
#'   Every candidate role score blends the name prior with value evidence;
#'   consistent (effect, se) pairs earn a bonus and a third column reproducing
#'   effect / se earns a larger one, down-weighted when that witness column
#'   has no t-statistic name support (derived columns reproduce the wrong
#'   pair's ratio just as perfectly). Consistency bonuses are withheld from
#'   columns whose own value evidence is very low, so an identifier column
#'   cannot ride a coincidentally plausible ratio into an effect mapping.
#' @param df *\[data.frame\]* The data frame.
#' @param name_scores *\[matrix\]* Numeric matrix of name-match scores with
#'   rownames `CORE_EVIDENCE_ROLES` and one column per data column.
#' @param required_confidence *\[numeric\]* Acceptance threshold for the
#'   required roles (effect, se).
#' @param optional_confidence *\[numeric\]* Acceptance threshold for the
#'   optional t_stat role.
#' @return *\[list\]* Accepted assignments keyed by role, each a
#'   `list(column, score)`. Roles without a confident candidate are absent.
#'   Two attributes carry the confirm-me layer, which never changes what is
#'   accepted: `provisional` holds one question per required role, if any is
#'   worth asking (`kind = "unmapped"` for a declined role, `kind = "tie"` for
#'   an accepted role with a near-tied runner-up), with `column` the leading
#'   candidate and `alternatives` any competitor too close to separate from
#'   it; `declined` holds the ranked evidence behind each decline.
assign_core_roles <- function(df,
                              name_scores,
                              required_confidence,
                              optional_confidence) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.data.frame(df),
    is.matrix(name_scores),
    all(CORE_EVIDENCE_ROLES %in% rownames(name_scores))
  )

  cols <- colnames(name_scores)
  if (length(cols) == 0 || nrow(df) < MIN_ROWS_FOR_EVIDENCE) {
    return(list())
  }

  evidence <- vapply(cols, function(col) {
    vapply(CORE_EVIDENCE_ROLES, function(role) {
      score_role_evidence(df[[col]], role)
    }, numeric(1))
  }, numeric(length(CORE_EVIDENCE_ROLES)))
  evidence <- matrix(
    evidence,
    nrow = length(CORE_EVIDENCE_ROLES),
    dimnames = list(CORE_EVIDENCE_ROLES, cols)
  )

  base <- matrix(
    NA_real_,
    nrow = length(CORE_EVIDENCE_ROLES),
    ncol = length(cols),
    dimnames = list(CORE_EVIDENCE_ROLES, cols)
  )
  for (role in CORE_EVIDENCE_ROLES) {
    for (col in cols) {
      base[role, col] <- combine_name_and_evidence(
        name_scores[role, col],
        evidence[role, col]
      )
    }
  }

  role_pool <- function(role) {
    in_pool <- name_scores[role, ] >= 0.4 |
      (!is.na(evidence[role, ]) & evidence[role, ] >= 0.55)

    # A column whose name clearly favors a DIFFERENT core role must not
    # compete for this one just because its raw values are superficially
    # plausible. (effect, se) and the mathematically related (t_stat, 1/se)
    # are easy to confuse on value evidence alone (t_stat = effect / se by
    # construction); the name is decisive when it disagrees this strongly.
    other_roles <- setdiff(CORE_EVIDENCE_ROLES, role)
    prefers_other <- Reduce(`|`, lapply(other_roles, function(other) {
      name_scores[other, ] >= 0.4 & name_scores[other, ] > name_scores[role, ]
    }))
    in_pool <- in_pool & !prefers_other

    pool <- cols[in_pool]
    pool <- pool[order(base[role, pool], decreasing = TRUE)]
    top <- utils::head(pool, 8)
    # A wide dataset can carry more name-matched columns than the pool holds
    # (eight X_se moderator columns), crowding out an anonymous column whose
    # values are the real role. Keep the strongest pure-evidence candidates
    # (no name support of their own) eligible alongside the name-ranked head.
    has_evidence <- !is.na(evidence[role, pool]) & evidence[role, pool] >= 0.55
    anonymous <- pool[name_scores[role, pool] < 0.4 & has_evidence]
    anonymous <- anonymous[order(evidence[role, anonymous], decreasing = TRUE)]
    union(top, utils::head(anonymous, 3))
  }

  effect_pool <- role_pool("effect")
  se_pool <- role_pool("se")
  t_pool <- role_pool("t_stat")

  # Columns with essentially no value evidence for a role get no consistency
  # bonus for that role: consistency must confirm plausible values, not rescue
  # an identifier.
  bonus_gate <- function(role, col) {
    ev <- evidence[role, col]
    is.na(ev) || ev > 0.2
  }

  best <- NULL
  for (effect_col in effect_pool) {
    for (se_col in setdiff(se_pool, effect_col)) {
      pair_score <- score_pair_consistency(df[[effect_col]], df[[se_col]])
      pair_known <- !is.na(pair_score)
      pair_value <- if (pair_known) pair_score else 0

      t_col <- NA_character_
      triple_score <- 0
      for (t_candidate in setdiff(t_pool, c(effect_col, se_col))) {
        triple <- score_triple_consistency(
          df[[effect_col]], df[[se_col]], df[[t_candidate]]
        )
        # A real t-statistic column reproduces effect / se on nearly every
        # row (rounded published values still clear 0.9); a column matching
        # on a minority of rows is coincidence, not corroboration.
        if (is.na(triple) || triple < 0.5) next
        # t = effect / se by construction, so any set of mutually derived
        # columns (interval bounds, widths, rescaled duplicates) can reproduce
        # the ratio of the WRONG pair perfectly. A witness whose own name
        # carries no t-statistic signal is weak corroboration; only a column
        # named like a t-statistic earns the full bonus.
        weight <- if (name_scores["t_stat", t_candidate] >= 0.4) 1 else 0.4
        weighted <- triple * weight
        if (weighted > triple_score) {
          triple_score <- weighted
          t_col <- t_candidate
        }
      }

      bonus <- 0.15 * pair_value + 0.25 * triple_score
      penalty <- if (pair_known && pair_value < 0.25) 0.15 else 0

      effect_score <- base["effect", effect_col]
      if (bonus_gate("effect", effect_col)) {
        effect_score <- min(1, effect_score + bonus)
      }
      se_score <- base["se", se_col]
      if (bonus_gate("se", se_col)) {
        se_score <- min(1, se_score + bonus)
      }
      effect_score <- max(0, effect_score - penalty)
      se_score <- max(0, se_score - penalty)

      total <- effect_score + se_score + 0.5 * triple_score
      if (is.null(best) || total > best$total) {
        best <- list(
          effect_col = effect_col,
          se_col = se_col,
          t_col = t_col,
          effect_score = effect_score,
          se_score = se_score,
          triple_score = triple_score,
          total = total
        )
      }
    }
  }

  assignments <- list()
  if (!is.null(best)) {
    if (best$effect_score >= required_confidence) {
      assignments$effect <- list(column = best$effect_col, score = best$effect_score)
    }
    if (best$se_score >= required_confidence) {
      assignments$se <- list(column = best$se_col, score = best$se_score)
    }
    if (!is.null(assignments$effect) &&
      !is.null(assignments$se) &&
      !is.na(best$t_col) &&
      best$triple_score >= 0.8) {
      t_score <- min(1, base["t_stat", best$t_col] + 0.3)
      if (t_score >= optional_confidence) {
        assignments$t_stat <- list(column = best$t_col, score = t_score)
      }
    }
  }

  # Roles the pair search left unassigned fall back to their best standalone
  # candidate (this covers datasets missing one half of the pair entirely).
  used <- vapply(assignments, function(a) a$column, character(1))
  standalone <- list(
    effect = required_confidence,
    se = required_confidence,
    t_stat = optional_confidence
  )
  for (role in names(standalone)) {
    if (!is.null(assignments[[role]])) next
    candidates <- setdiff(role_pool(role), used)
    if (length(candidates) == 0) next
    best_col <- candidates[which.max(base[role, candidates])]
    if (base[role, best_col] >= standalone[[role]]) {
      assignments[[role]] <- list(column = best_col, score = base[role, best_col])
      used <- c(used, best_col)
    }
  }

  result <- assignments[intersect(CORE_EVIDENCE_ROLES, names(assignments))]

  # The confirm-me layer. Nothing below changes what is accepted: it records
  # why the required roles were decided the way they were. `provisional` holds
  # at most one candidate per role that is worth a single confirmation
  # question (a role the thresholds declined, or an accepted role with a
  # near-tied runner-up); `declined` holds the ranked evidence behind a
  # decline, for the log and for external tooling.
  counterpart_of <- list(effect = "se", se = "effect")

  pair_consistency_with <- function(role, col, counterpart) {
    if (is.null(counterpart) || is.na(counterpart) || identical(col, counterpart)) {
      return(NA_real_)
    }
    if (identical(role, "effect")) {
      score_pair_consistency(df[[col]], df[[counterpart]])
    } else {
      score_pair_consistency(df[[counterpart]], df[[col]])
    }
  }

  # Rank a role's candidates on the same scale the acceptance path uses: the
  # blended name/value score plus the pair bonus the candidate would earn
  # against the counterpart column that was accepted.
  rank_candidates <- function(role, pool, counterpart) {
    if (length(pool) == 0) {
      return(NULL)
    }
    scores <- vapply(pool, function(col) {
      score <- base[role, col]
      pair <- pair_consistency_with(role, col, counterpart)
      if (!is.na(pair)) score <- min(1, score + 0.15 * pair)
      score
    }, numeric(1))
    ord <- order(scores, decreasing = TRUE)
    list(columns = pool[ord], scores = unname(scores[ord]))
  }

  describe_candidate <- function(role, col, score, counterpart) {
    p <- profile_role_values(df[[col]])
    list(
      column = col,
      score = score,
      name_score = unname(name_scores[role, col]),
      evidence = unname(evidence[role, col]),
      pair_consistency = pair_consistency_with(role, col, counterpart),
      n = if (is.null(p)) NA_integer_ else p$n,
      coverage = if (is.null(p)) NA_real_ else p$coverage,
      n_distinct = if (is.null(p)) NA_integer_ else p$n_distinct,
      non_integer_share = if (is.null(p)) NA_real_ else p$non_integer_share,
      has_both_signs = if (is.null(p)) NA else p$has_both_signs,
      median_abs = if (is.null(p)) NA_real_ else p$median_abs
    )
  }

  # Effect and se are measured quantities: a column of nothing but whole
  # numbers is a count or a code, however well it pairs with the rest. The
  # automatic path vetoes such columns outright, and the confirm-me layer must
  # not smuggle one back in as a question. Mirrors
  # `column_recognition::looks_like_continuous_measure()`.
  looks_measured <- function(col) {
    p <- profile_role_values(df[[col]])
    !is.null(p) && (p$n < 20 || p$integer_share < 0.95)
  }

  # Why a candidate is not even worth asking about. NULL means it is.
  decline_reason <- function(role, cand, counterpart) {
    thresholds <- PROVISIONAL_THRESHOLDS
    if (is.na(cand$evidence)) {
      return("too few usable values to judge the column")
    }
    if (!looks_measured(cand$column)) {
      return(sprintf(
        "%s holds only whole numbers, so it is a count or a code rather than a measured %s",
        cand$column, role
      ))
    }
    if (cand$evidence < thresholds$min_evidence) {
      return(sprintf(
        "the values of %s carry only weak %s evidence (%.2f)",
        cand$column, role, cand$evidence
      ))
    }
    # Value evidence alone is what the acceptance path already declined. What
    # makes a candidate worth a question is corroboration from another column:
    # the accepted counterpart role must exist, and the pair must behave like
    # an (effect, se) pair. Without a counterpart there is nothing to check
    # against, and the role is left for the ordinary mapping prompts.
    if (is.null(counterpart)) {
      return(sprintf(
        "no %s column is mapped, so nothing corroborates the values of %s",
        counterpart_of[[role]], cand$column
      ))
    }
    if (is.na(cand$pair_consistency) || cand$pair_consistency < thresholds$min_pair) {
      return(sprintf(
        "%s does not form a consistent pair with the accepted %s column",
        cand$column, counterpart
      ))
    }
    NULL
  }

  qualifies <- function(role, col, score, counterpart) {
    is.null(decline_reason(role, describe_candidate(role, col, score, counterpart), counterpart))
  }

  provisional <- list()
  declined <- list()
  claimed <- vapply(result, function(a) a$column, character(1))

  for (role in c("effect", "se")) {
    counterpart_role <- counterpart_of[[role]]
    counterpart <- if (is.null(result[[counterpart_role]])) {
      NULL
    } else {
      result[[counterpart_role]]$column
    }
    accepted <- if (is.null(result[[role]])) NA_character_ else result[[role]]$column
    pool <- setdiff(role_pool(role), setdiff(claimed, accepted))
    ranked <- rank_candidates(role, pool, counterpart)
    if (is.null(ranked)) next

    top <- lapply(seq_len(min(3L, length(ranked$columns))), function(i) {
      describe_candidate(role, ranked$columns[i], ranked$scores[i], counterpart)
    })

    if (is.na(accepted)) {
      cand <- top[[1]]
      margin <- if (length(ranked$scores) > 1) {
        ranked$scores[1] - ranked$scores[2]
      } else {
        Inf
      }
      reason <- decline_reason(role, cand, counterpart)
      if (is.null(reason)) {
        # Competitors sitting within the margin of the best candidate are not
        # a reason to stay silent: they are the rest of the question. The user
        # picks one instead of the recognizer guessing between twins
        # (est / LB / UB, size / size_unwinsorized).
        near <- setdiff(
          which(ranked$scores >= ranked$scores[1] - PROVISIONAL_THRESHOLDS$min_margin),
          1L
        )
        near <- Filter(
          function(i) qualifies(role, ranked$columns[i], ranked$scores[i], counterpart),
          near
        )
        near <- utils::head(near, 3L)

        provisional[[role]] <- list(
          kind = "unmapped",
          role = role,
          column = cand$column,
          score = cand$score,
          evidence = cand$evidence,
          name_score = cand$name_score,
          pair_consistency = cand$pair_consistency,
          pair_with = if (is.null(counterpart)) NA_character_ else counterpart,
          runner_up = if (length(ranked$columns) > 1) ranked$columns[2] else NA_character_,
          margin = margin,
          alternatives = ranked$columns[near],
          summary = cand,
          alternative_summaries = lapply(near, function(i) {
            describe_candidate(role, ranked$columns[i], ranked$scores[i], counterpart)
          })
        )
        reason <- "no candidate cleared the acceptance threshold; the best one is offered for confirmation"
      }
      declined[[role]] <- list(
        role = role,
        reason = reason,
        required_confidence = required_confidence,
        candidates = top
      )
      next
    }

    # An accepted role with a runner-up sitting within a whisker of it: the
    # pick is kept (non-interactive behavior is unchanged) but recorded as a
    # near-tie so an interactive session can ask instead of guessing.
    accepted_at <- match(accepted, ranked$columns)
    if (is.na(accepted_at)) next
    tie_floor <- ranked$scores[accepted_at] - PROVISIONAL_THRESHOLDS$tie_margin
    alt_idx <- which(
      ranked$columns != accepted &
        ranked$scores >= tie_floor &
        !is.na(evidence[role, ranked$columns]) &
        evidence[role, ranked$columns] >= PROVISIONAL_THRESHOLDS$min_evidence &
        vapply(ranked$columns, looks_measured, logical(1))
    )
    if (length(alt_idx) == 0) next
    alt_idx <- utils::head(alt_idx, 2L)
    provisional[[role]] <- list(
      kind = "tie",
      role = role,
      column = accepted,
      score = ranked$scores[accepted_at],
      evidence = unname(evidence[role, accepted]),
      name_score = unname(name_scores[role, accepted]),
      pair_consistency = pair_consistency_with(role, accepted, counterpart),
      pair_with = if (is.null(counterpart)) NA_character_ else counterpart,
      runner_up = ranked$columns[alt_idx[1]],
      margin = ranked$scores[accepted_at] - ranked$scores[alt_idx[1]],
      alternatives = ranked$columns[alt_idx],
      summary = describe_candidate(role, accepted, ranked$scores[accepted_at], counterpart),
      alternative_summaries = lapply(alt_idx, function(i) {
        describe_candidate(role, ranked$columns[i], ranked$scores[i], counterpart)
      })
    )
  }

  attr(result, "provisional") <- provisional
  attr(result, "declined") <- declined
  result
}

box::export(
  MIN_ROWS_FOR_EVIDENCE,
  PROVISIONAL_THRESHOLDS,
  MIN_COMPLETE_PAIRS,
  CORE_EVIDENCE_ROLES,
  coerce_numeric_column,
  profile_role_values,
  score_role_evidence,
  score_pair_consistency,
  score_triple_consistency,
  assign_core_roles
)
