box::use(
  artma / data / profile[profile_column],
  artma / data / role_evidence[
    MIN_ROWS_FOR_EVIDENCE,
    CORE_EVIDENCE_ROLES,
    assign_core_roles,
    profile_role_values
  ]
)

#' Single home for every confidence threshold the column-matching engine uses.
#' Both the recognition flow (options creation, auto-detection) and the schema
#' reconciliation flow (rename proposals) read from this list, so the
#' thresholds cannot drift apart again.
#' - required_confidence: minimum score to auto-accept a match for a required column
#' - optional_confidence: stricter minimum for optional columns (fewer false positives)
#' - rename_suggest: minimum score for a rename proposal to be shown to the user
#' - rename_auto: minimum score for a rename to be accepted without confirmation
MATCH_THRESHOLDS <- list(
  required_confidence = 0.7,
  optional_confidence = 0.95,
  rename_suggest = 0.5,
  rename_auto = 0.75
)

#' @title Define column patterns for recognition
#' @description Returns a list of patterns for recognizing standard columns
#' @return *\[list\]* Named list of regex patterns and keywords for each standard column
get_column_patterns <- function() {
  list(
    effect = list(
      patterns = c(
        "^effect[_\\.]?(size)?$",
        "^estimate[sd]?$",
        "^coeff?(icient)?$",
        "^beta$",
        "^b$",
        "^e$",
        "^es$",
        "^d$",
        "^g$",
        "^r$",
        "^yi$",
        "^pcc$",
        "^pearson[_\\.]?r$",
        "^cohen[_\\.]?d$",
        "^hedges[_\\.]?g$",
        "^odds[_\\.]?ratio$",
        "^or$",
        "^risk[_\\.]?ratio$",
        "^rr$",
        "^hazard[_\\.]?ratio$",
        "^hr$"
      ),
      keywords = c("effect", "estimate", "coef", "beta", "es", "pcc", "pearson", "cohen", "hedges", "odds", "ratio", "risk", "hazard"),
      priority = 1,
      # Identifier-flavored names (idcoeff, coef_id, estimate_no) must never
      # ride an effect keyword into a match; the entries below are regexes.
      # They name the estimate they index, not the estimate itself.
      exclude_keywords = c(
        "standard", "error", "se",
        "(^|[_\\.])id", "id([_\\.]|$)", "index",
        "(^|[_\\.])no([_\\.]|$)", "(^|[_\\.])num(ber)?([_\\.]|$)", "count"
      )
    ),
    se = list(
      patterns = c(
        "^se$",
        "^sei$",
        "^std[_\\.]?err(or)?$",
        "^standard[_\\.]?error$",
        "^stderr$",
        "^s\\.e\\.$"
      ),
      keywords = c("se", "stderr", "error", "standard"),
      priority = 1,
      require_all_keywords = FALSE
    ),
    n_obs = list(
      patterns = c(
        "^n[_\\.]?obs$",
        "^n$",
        "^sample[_\\.]?size$",
        # Compressed sample-size names common in econ meta-analyses:
        # samsize, sampsize, samplesize, TotalObs, total_observations.
        "^sam(p|ple)?[_\\.]?size$",
        "^total[_\\.]?obs(ervations)?$",
        "^observations?$",
        "^n[_\\.]?observations?$",
        "^obs[_\\.]?n$"
      ),
      keywords = c("obs", "sample", "size"),
      priority = 2
    ),
    t_stat = list(
      patterns = c(
        "^t[_\\.]?stat(istic)?s?$",
        "^t[_\\.]?value$",
        "^t$",
        "^tval$",
        # Compound names where "tstat" is a token alongside a variable suffix
        # (tstat_premium, TSTAT_L): token-boundary anchored so it cannot fire
        # mid-word (test_stat, statutory) the way a bare keyword substring would.
        "(^|[_\\.])t[_\\.]?stat(istic)?s?([_\\.]|$)"
      ),
      keywords = c("stat", "tvalue", "tval"),
      priority = 2
    ),
    study_id = list(
      patterns = c(
        "^study[_\\.]?id$",
        "^studyid$",
        "^id[_\\.]?study$",
        "^sid$",
        "^study[_\\.]?name$",
        "^study$",
        "^author[_\\.]?name$",
        "^paper$",
        "^publication$",
        "^source$"
      ),
      keywords = c("study", "studyid", "name", "author", "paper", "publication"),
      exclude_keywords = c("size"),
      priority = 1
    ),
    obs_id = list(
      patterns = c(
        "^obs[_\\.]?id$",
        "^observation[_\\.]?id$",
        "^row[_\\.]?id$",
        "^obs[_\\.]?n$",
        "^n[_\\.]?obs$"
      ),
      keywords = c("obs_id", "observation_id", "row_id"),
      exclude_keywords = c("region", "africa", "asia", "america", "europe", "middle", "east", "north", "south"),
      priority = 3
    ),
    reg_dof = list(
      patterns = c(
        "^reg[_\\.]?d[eo]f$",
        "^reg[_\\.]?df$",
        "^degrees?[_\\.]?of[_\\.]?freedom$",
        "^dof$"
      ),
      keywords = c("reg_dof", "regdof", "reg_df"),
      exclude_keywords = c("index", "freedom_index"),
      priority = 3
    ),
    precision = list(
      patterns = c(
        "^precision$",
        "^prec$",
        "^weight$"
      ),
      keywords = c("precision", "prec", "weight"),
      priority = 3
    ),
    study_size = list(
      patterns = c(
        "^study[_\\.]?size$",
        "^n[_\\.]?estimates$"
      ),
      keywords = c("study", "size"),
      priority = 3
    )
  )
}


#' @title Calculate string similarity
#' @description Calculate similarity between two strings (0-1 scale)
#' @param str1 *\[character\]* First string
#' @param str2 *\[character\]* Second string
#' @return *\[numeric\]* Similarity score (0 = no match, 1 = perfect match)
string_similarity <- function(str1, str2) {
  str1 <- tolower(trimws(str1))
  str2 <- tolower(trimws(str2))

  if (str1 == str2) {
    return(1.0)
  }

  # Exact substring match
  if (grepl(str2, str1, fixed = TRUE) || grepl(str1, str2, fixed = TRUE)) {
    return(0.8)
  }

  # Calculate Levenshtein distance-based similarity
  max_len <- max(nchar(str1), nchar(str2))
  if (max_len == 0) {
    return(0)
  }

  dist <- utils::adist(str1, str2)[1, 1]
  similarity <- 1 - (dist / max_len)

  similarity
}


#' Normalize a raw column name for matching.
#' @keywords internal
clean_column_name <- function(col_name) {
  col_name_clean <- tolower(trimws(col_name))
  gsub("[^a-z0-9_]", "_", col_name_clean)
}


#' @title Whether a keyword appears as a whole underscore-delimited token
#' @description A plain `grepl(kw, name)` substring test lets short, generic
#'   keywords ("es", "se") and even full-word ones ("effect") match mid-word
#'   inside unrelated names: "es" inside "stakes", "se" inside "observations",
#'   "effect" inside "government_effectiveness". Requiring a token boundary
#'   (start/end of the cleaned name, or an underscore) keeps the keyword
#'   signal for genuine matches ("se" in "std_se", "effect" in "effect_size")
#'   while rejecting embedded coincidences.
#' @param col_name_clean *\[character\]* Cleaned column name.
#' @param kw *\[character\]* Keyword to look for.
#' @return *\[logical\]* TRUE if `kw` is a whole token in `col_name_clean`.
#' @keywords internal
keyword_token_present <- function(col_name_clean, kw) {
  grepl(paste0("(^|_)", kw, "(_|$)"), col_name_clean, ignore.case = TRUE)
}


#' @title Score a cleaned column name against one role's pattern definition
#' @description The name-matching signal for a single standard column: 1.0 for
#'   a regex match, up to 0.95 for keyword similarity, 0 when an exclude
#'   pattern fires or nothing matches. Keyword credit requires the keyword to
#'   appear as a whole token (see `keyword_token_present()`), not merely as a
#'   substring, so generic keywords cannot match mid-word inside an unrelated
#'   name.
#' @param col_name_clean *\[character\]* Cleaned column name (see
#'   `clean_column_name`).
#' @param pattern_def *\[list\]* One entry of `get_column_patterns()`.
#' @return *\[list\]* With 'score' (0-1) and 'method' ("regex", "keyword", NA).
score_name_for_role <- function(col_name_clean, pattern_def) {
  # Regex patterns are authoritative and skip the exclusion check
  for (pattern in pattern_def$patterns) {
    if (grepl(pattern, col_name_clean, ignore.case = TRUE)) {
      return(list(score = 1.0, method = "regex"))
    }
  }

  keywords <- pattern_def$keywords
  exclude_keywords <- if (is.null(pattern_def$exclude_keywords)) character(0) else pattern_def$exclude_keywords

  has_exclude <- any(vapply(exclude_keywords, function(kw) {
    grepl(kw, col_name_clean, ignore.case = TRUE)
  }, logical(1)))

  if (has_exclude) {
    return(list(score = 0, method = NA_character_))
  }

  keyword_present <- vapply(keywords, function(kw) {
    keyword_token_present(col_name_clean, kw)
  }, logical(1))
  n_keywords_found <- sum(keyword_present)

  if (n_keywords_found > 0) {
    max_keyword_score <- max(vapply(keywords[keyword_present], function(kw) {
      string_similarity(col_name_clean, kw)
    }, numeric(1)))
    keyword_score <- max_keyword_score + (n_keywords_found - 1) * 0.1
    keyword_score <- min(keyword_score, 0.95) # Cap below regex matches
    return(list(score = keyword_score, method = "keyword"))
  }

  list(score = 0, method = NA_character_)
}


#' @title Match column name to standard column
#' @description Attempts to match a data frame column name to a standard column
#' @param col_name *\[character\]* Column name from the data frame
#' @param patterns *\[list\]* Patterns for recognition (from get_column_patterns)
#' @return *\[list\]* Match result with 'match' (column name or NA), 'score' (0-1), 'method' (how it matched)
match_column_name <- function(col_name, patterns) {
  col_name_clean <- clean_column_name(col_name)

  best_score <- 0
  best_match <- NA_character_
  best_method <- NA_character_

  for (std_col in names(patterns)) {
    result <- score_name_for_role(col_name_clean, patterns[[std_col]])
    if (result$score > best_score) {
      best_score <- result$score
      best_match <- std_col
      best_method <- result$method
    }
  }

  list(
    match = best_match,
    score = best_score,
    method = best_method
  )
}


#' @title Analyze column values to determine semantic type
#' @description Analyzes actual data values to help discriminate between
#'   ambiguous column matches. Thin re-export of the shared
#'   `data/profile.R::profile_column` profiler.
#' @param values *\[vector\]* Column values to analyze
#' @return *\[list\]* Analysis results with various heuristics
analyze_column_values <- profile_column


#' @title Detect if a column is likely a numeric identifier
#' @description Checks whether a candidate column behaves like numeric IDs (sequential or near-unique numerics)
#' @param values *\[vector\]* Column values to analyze
#' @return *\[logical\]* TRUE if the column likely contains numeric IDs
is_likely_numeric_id <- function(values) {
  values_clean <- values[!is.na(values)]
  analysis <- analyze_column_values(values)

  if (!analysis$is_numeric) {
    return(FALSE)
  }

  if (analysis$is_sequential) {
    return(TRUE)
  }

  if (analysis$uniqueness_ratio >= 0.95) {
    return(TRUE)
  }

  numeric_values <- suppressWarnings(as.numeric(values_clean))
  numeric_values <- numeric_values[!is.na(numeric_values)]

  if (length(numeric_values) < 2) {
    return(FALSE)
  }

  integer_like_ratio <- mean(abs(numeric_values - round(numeric_values)) < 1e-10)
  n_unique <- length(unique(numeric_values))

  integer_like_ratio >= 0.95 && n_unique >= 2
}


#' @title Detect whether a column plausibly holds a continuous measurement
#' @description
#' Effects, standard errors and t-statistics are measured quantities: across a
#' reasonably sized sample they vary continuously and are not whole numbers.
#' A column that is entirely integer-valued is an index, a count or a code, no
#' matter how promising its name looks. This is the value-level backstop behind
#' the name patterns, and it is what keeps a column like `idcoeff` (a
#' within-study coefficient number) from being accepted as the effect size.
#'
#' Small frames are exempt: with only a handful of rows, whole numbers carry no
#' signal, and test fixtures and tiny datasets should not be rejected on it.
#'
#' @param values *\[vector\]* Column values to analyze
#' @param min_n *\[integer\]* Minimum number of non-missing values before the
#'   integer check is trusted. Defaults to 20.
#' @return *\[logical\]* TRUE when the column could be a continuous measurement
looks_like_continuous_measure <- function(values, min_n = 20L) {
  numeric_values <- suppressWarnings(as.numeric(values))
  numeric_values <- numeric_values[is.finite(numeric_values)]

  if (length(numeric_values) < min_n) {
    return(TRUE)
  }

  integer_like_ratio <- mean(abs(numeric_values - round(numeric_values)) < 1e-10)
  if (integer_like_ratio >= 0.95) {
    return(FALSE)
  }

  TRUE
}


#' @title Standard columns that must hold continuous measurements
#' @description The roles `looks_like_continuous_measure()` gates. Kept next to
#'   the check so the two never drift apart.
CONTINUOUS_MEASURE_COLUMNS <- c("effect", "se", "t_stat", "precision")


#' @title Detect if a column is likely a usable study key
#' @description Checks whether a candidate column has label-like values suitable for study keys
#' @param values *\[vector\]* Column values to analyze
#' @return *\[logical\]* TRUE if the column likely contains usable study labels/keys
is_likely_study_key <- function(values) {
  values_clean <- values[!is.na(values)]

  if (length(values_clean) < 2) {
    return(FALSE)
  }

  analysis <- analyze_column_values(values_clean)
  # A real meta-analysis routinely carries dozens or hundreds of estimates per
  # study, so a legitimate study-label column can have a very low uniqueness
  # ratio (42 authors across 3500+ rows is normal, not degenerate). Only
  # reject the pathological case of a handful of unique values swamped by
  # thousands of rows of one repeated value.
  if (analysis$uniqueness_ratio < 0.005) {
    return(FALSE)
  }

  values_chr <- trimws(as.character(values_clean))
  values_chr <- values_chr[nzchar(values_chr)]

  if (length(values_chr) < 2) {
    return(FALSE)
  }

  # If values are purely numeric-like strings, treat as numeric IDs rather than label keys.
  numeric_like_ratio <- mean(grepl("^[-+]?[0-9]+(\\.[0-9]+)?$", values_chr))
  if (numeric_like_ratio > 0.8) {
    return(FALSE)
  }

  # Require some textual structure (letters and/or punctuation common in citation-like keys).
  has_letters <- mean(grepl("[A-Za-z]", values_chr)) >= 0.6
  has_key_punct <- mean(grepl("[()_.,-]", values_chr)) >= 0.3

  has_letters || has_key_punct
}


#' @title Score candidate column for a specific standard column type
#' @description Uses value analysis to score how well a candidate matches expected properties
#' @param df *\[data.frame\]* The data frame
#' @param candidate_col *\[character\]* Name of candidate column
#' @param std_col *\[character\]* Standard column type (e.g., "n_obs", "obs_id")
#' @param name_score *\[numeric\]* Score from name matching
#' @return *\[numeric\]* Adjusted score based on value analysis
score_candidate_values <- function(df, candidate_col, std_col, name_score) {
  analysis <- analyze_column_values(df[[candidate_col]])

  # Identifier-pattern flags from the shared role-evidence profiler: per-group
  # counters (an idcoeff-style coefficient index) and year columns are strong
  # negative signals for every data role.
  role_profile <- profile_role_values(df[[candidate_col]])
  counter_like <- !is.null(role_profile) && role_profile$is_within_group_counter
  year_like <- !is.null(role_profile) && role_profile$is_year_like

  # Apply heuristics based on standard column type
  value_penalty <- 0

  if (std_col == "n_obs") {
    # Sample size columns should:
    # - Not be sequential IDs
    # - Have reasonable variance (not all same value)
    # - Not be perfectly unique (some studies may have same sample size)
    # - Be positive integers typically > 10

    if (analysis$is_sequential) {
      # Strong penalty for sequential patterns
      value_penalty <- value_penalty + 0.3
    }

    if (counter_like) {
      value_penalty <- value_penalty + 0.4
    }

    if (year_like) {
      value_penalty <- value_penalty + 0.25
    }

    if (analysis$is_unique && analysis$uniqueness_ratio > 0.95) {
      # Moderate penalty for high uniqueness (IDs are unique, sample sizes may repeat)
      value_penalty <- value_penalty + 0.15
    }

    if (analysis$is_numeric) {
      # A sample size of 0 or 1 observation is implausible; a column whose
      # minimum is 1 is typically an estimates-per-study count or another
      # counter, not the underlying samples (which start around 5-10 even
      # for tiny experiments).
      if (!is.na(analysis$min) && analysis$min <= 1) {
        value_penalty <- value_penalty + 0.3
      }
      if (!is.na(analysis$max) && analysis$max > 1e6) {
        # Extremely large values unlikely to be sample sizes
        value_penalty <- value_penalty + 0.1
      }
    }

    # With enough rows, a column with two distinct values is a dummy, never
    # a sample size. Tiny frames are exempt: two rows always have at most
    # two distinct values.
    if (!is.null(role_profile) && role_profile$n >= 10 && role_profile$n_distinct <= 2) {
      value_penalty <- value_penalty + 0.5
    }

    # A sample-size column is essentially always integer-valued. A column
    # with a meaningful non-integer share (e.g. a log-transformed count) is
    # not n_obs even when its name matches perfectly.
    if (!is.null(role_profile) && role_profile$non_integer_share > 0.3) {
      value_penalty <- value_penalty + 0.4
    }
  } else if (std_col == "obs_id") {
    # Observation ID columns should:
    # - Be sequential or unique
    # - Have high uniqueness ratio

    if (analysis$is_sequential) {
      # Bonus for sequential patterns
      value_penalty <- value_penalty - 0.2
    }

    if (!analysis$is_unique) {
      # Penalty for non-unique values
      value_penalty <- value_penalty + 0.2
    }

    if (analysis$uniqueness_ratio < 0.95) {
      # Penalty for low uniqueness
      value_penalty <- value_penalty + 0.15
    }
  } else if (std_col == "study_id") {
    # Study ID columns should:
    # - Have high uniqueness (but not necessarily perfect if multiple obs per study)
    # - Not be sequential in most cases

    if (analysis$uniqueness_ratio < 0.5) {
      # Multiple observations per study is fine, but too many repetitions is suspicious
      value_penalty <- value_penalty + 0.1
    }

    if (counter_like) {
      # A per-study counter restarts within studies, so it cannot key them
      value_penalty <- value_penalty + 0.3
    }
  } else if (std_col %in% c("effect", "se", "t_stat")) {
    # Effect sizes, standard errors, t-stats should:
    # - Not be sequential or identifier-like
    # - Have reasonable variance
    # - Not be all unique (some repetition expected)

    if (analysis$is_sequential) {
      value_penalty <- value_penalty + 0.3
    }

    if (counter_like) {
      value_penalty <- value_penalty + 0.4
    }

    if (year_like) {
      value_penalty <- value_penalty + 0.3
    }

    if (analysis$is_unique && analysis$uniqueness_ratio > 0.98) {
      value_penalty <- value_penalty + 0.1
    }

    if (analysis$is_numeric && !is.na(analysis$variance) && analysis$variance < 1e-10) {
      # No variance suggests not a real data column
      value_penalty <- value_penalty + 0.2
    }
  }

  # Apply penalty and ensure score stays in valid range
  adjusted_score <- max(0, min(1, name_score - value_penalty))
  adjusted_score
}


#' @title Resolve multiple candidate matches using value analysis
#' @description When multiple columns match a standard column, use value analysis to pick best
#' @param df *\[data.frame\]* The data frame
#' @param candidates *\[character\]* Vector of candidate column names
#' @param std_col *\[character\]* Standard column type
#' @param matches *\[list\]* Match results from match_column_name
#' @return *\[character\]* Best candidate column name
resolve_multiple_matches <- function(df, candidates, std_col, matches) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (length(candidates) == 1) {
    return(candidates[1])
  }

  # Score each candidate using value analysis
  candidate_scores <- vapply(candidates, function(cand) {
    name_score <- matches[[cand]]$score
    value_score <- score_candidate_values(df, cand, std_col, name_score)

    # Bonus for exact or near-exact name matches
    cand_clean <- tolower(gsub("[^a-z0-9]", "", cand))
    std_clean <- tolower(gsub("[^a-z0-9]", "", std_col))

    if (cand_clean == std_clean) {
      # Exact match (ignoring separators) - significant bonus
      value_score <- value_score + 0.15
    } else if (grepl(std_clean, cand_clean, fixed = TRUE) || grepl(cand_clean, std_clean, fixed = TRUE)) {
      # Substring match - moderate bonus
      value_score <- value_score + 0.08
    }

    # Ensure score stays in valid range
    min(1.0, value_score)
  }, numeric(1))

  best_candidate <- candidates[which.max(candidate_scores)]

  if (std_col == "study_id") {
    string_like_candidates <- candidates[vapply(candidates, function(cand) {
      is_likely_study_key(df[[cand]])
    }, logical(1))]

    numeric_id_candidates <- candidates[vapply(candidates, function(cand) {
      is_likely_numeric_id(df[[cand]])
    }, logical(1))]

    if (length(string_like_candidates) > 0 && length(numeric_id_candidates) > 0) {
      best_string <- string_like_candidates[which.max(candidate_scores[string_like_candidates])]
      best_numeric <- numeric_id_candidates[which.max(candidate_scores[numeric_id_candidates])]
      score_gap <- candidate_scores[best_numeric] - candidate_scores[best_string]

      # Conservative preference: use string keys if they are plausible and not meaningfully weaker.
      if (score_gap <= 0.1) {
        best_candidate <- best_string
      }
    }
  }

  if (get_verbosity() >= 4) {
    cli::cli_inform("Resolved multiple matches for {.field {std_col}}:")
    for (cand in candidates) {
      name_score <- matches[[cand]]$score
      value_score <- candidate_scores[cand]
      analysis <- analyze_column_values(df[[cand]])
      marker <- if (cand == best_candidate) "\u2713" else " "
      type_label <- if (std_col == "study_id") {
        sprintf(
          ", label_key=%s, numeric_id=%s",
          is_likely_study_key(df[[cand]]),
          is_likely_numeric_id(df[[cand]])
        )
      } else {
        ""
      }
      cli::cli_inform(
        "  {marker} {.field {cand}}: name={round(name_score, 2)}, adjusted={round(value_score, 2)} (seq={analysis$is_sequential}, uniq={round(analysis$uniqueness_ratio, 2)}{type_label})"
      )
    }
  }

  best_candidate
}


#' @title Score a rename candidate
#' @description Scores how likely `candidate` is a rename of `stored_name`.
#'   Combines the string-similarity signal with the pattern-based recognition
#'   signal when the standard column the record maps to is known, so both the
#'   recognition flow and schema reconciliation share one matching engine.
#' @param stored_name *\[character\]* The column name stored in the config that
#'   is now missing from the data frame.
#' @param candidate *\[character\]* An unmatched column present in the data frame.
#' @param std_name *\[character, optional\]* The standard column (role) the
#'   stored name maps to, if any. Enables the pattern/value-analysis signal.
#' @param df *\[data.frame, optional\]* The data frame, for value analysis.
#' @return *\[numeric\]* Similarity score between 0 and 1.
score_rename_candidate <- function(stored_name, candidate, std_name = NULL, df = NULL) {
  score <- string_similarity(stored_name, candidate)

  patterns <- get_column_patterns()
  if (!is.null(std_name) && std_name %in% names(patterns)) {
    match_result <- match_column_name(candidate, patterns[std_name])
    if (!is.na(match_result$match)) {
      pattern_score <- match_result$score
      if (!is.null(df) && candidate %in% names(df)) {
        pattern_score <- score_candidate_values(df, candidate, std_name, pattern_score)
      }
      score <- max(score, pattern_score)
    }
  }

  score
}

#' @title Recognize columns in data frame
#' @description Automatically recognize which columns correspond to standard
#'   columns. Column names act as a prior; when the data frame is large enough
#'   to carry distributional information, the numeric core roles (effect, se,
#'   t_stat) are resolved jointly from name and value evidence (see
#'   `artma / data / role_evidence`), so an internally consistent
#'   (effect, se, t) triple outranks any name-only match and identifier
#'   columns are never auto-accepted as data roles.
#' @param df *\[data.frame\]* The data frame
#' @param min_confidence *\[numeric\]* Minimum confidence score (0-1) to accept a match
#' @return *\[list\]* Named list mapping standard columns to data frame columns
recognize_columns <- function(df, min_confidence = MATCH_THRESHOLDS$required_confidence) {
  box::use(
    artma / libs / core / log[log_debug, log_warn],
    artma / libs / core / validation[validate],
    artma / libs / core / utils[get_verbosity]
  )

  validate(is.data.frame(df))

  patterns <- get_column_patterns()
  col_names <- names(df)

  # Match each column
  matches <- lapply(col_names, function(col_name) {
    match_column_name(col_name, patterns)
  })
  names(matches) <- col_names

  # Build mapping from standard column to data column
  mapping <- list()
  used_cols <- character(0)

  # Joint, evidence-based assignment of the numeric core roles. Only runs when
  # the data carries enough rows to judge distributions; tiny frames keep the
  # name-based path below.
  core_resolved <- FALSE
  if (nrow(df) >= MIN_ROWS_FOR_EVIDENCE && length(col_names) > 0) {
    name_scores <- vapply(col_names, function(col_name) {
      col_name_clean <- clean_column_name(col_name)
      vapply(CORE_EVIDENCE_ROLES, function(role) {
        score_name_for_role(col_name_clean, patterns[[role]])$score
      }, numeric(1))
    }, numeric(length(CORE_EVIDENCE_ROLES)))
    name_scores <- matrix(
      name_scores,
      nrow = length(CORE_EVIDENCE_ROLES),
      dimnames = list(CORE_EVIDENCE_ROLES, col_names)
    )

    core_assignments <- assign_core_roles(
      df,
      name_scores = name_scores,
      required_confidence = min_confidence,
      optional_confidence = MATCH_THRESHOLDS$optional_confidence
    )

    for (role in names(core_assignments)) {
      assignment <- core_assignments[[role]]
      mapping[[role]] <- assignment$column
      used_cols <- c(used_cols, assignment$column)
      if (get_verbosity() >= 4) {
        cli::cli_inform(
          "Recognized {.field {assignment$column}} as {.field {role}} (joint evidence, score: {round(assignment$score, 2)})"
        )
      }
    }
    core_resolved <- TRUE
  }

  # Sort patterns by priority
  pattern_priority <- vapply(patterns, function(p) as.integer(p$priority), integer(1))
  sorted_std_cols <- names(patterns)[order(pattern_priority)]

  # Get required column names
  required_cols <- get_required_column_names()

  for (std_col in sorted_std_cols) {
    # Core roles were already decided (possibly declined) by the joint pass
    if (core_resolved && std_col %in% CORE_EVIDENCE_ROLES) next
    # Higher confidence threshold for optional columns to reduce false positives
    is_required <- std_col %in% required_cols
    confidence_threshold <- if (is_required) min_confidence else MATCH_THRESHOLDS$optional_confidence

    # Find all columns that matched this standard column
    all_candidates <- names(matches)[vapply(matches, function(m) {
      !is.na(m$match) && m$match == std_col && m$score >= confidence_threshold
    }, logical(1))]

    # Remove already used columns
    candidates <- setdiff(all_candidates, used_cols)

    # Value-level veto. score_candidate_values() only runs when several columns
    # compete for a role, so a single well-named but implausible candidate used
    # to be accepted unchecked. Continuous-measure roles get checked always.
    if (length(candidates) > 0 && std_col %in% CONTINUOUS_MEASURE_COLUMNS) {
      implausible <- candidates[!vapply(
        candidates,
        function(cand) looks_like_continuous_measure(df[[cand]]),
        logical(1)
      )]

      if (length(implausible) > 0) {
        candidates <- setdiff(candidates, implausible)
        reason <- "they hold only whole numbers, so they look like identifiers or counts rather than measured values"
        if (length(candidates) == 0) {
          # The veto left the role unmapped, so the user is about to be asked
          # for it (or the run stops in a non-interactive session). Say why.
          log_warn("No column matched {.field {std_col}}: {.field {implausible}} matched by name but {reason}.")
        } else {
          log_debug("Skipped {.field {implausible}} as {.field {std_col}}: {reason}.")
        }
      }
    }

    # Same value-level veto for n_obs: without it, score_candidate_values()
    # only runs when several columns compete, so a lone name-matched dummy
    # (rel_size), counter, or log-scale count would be accepted unchecked.
    # The margin below the acceptance threshold keeps this a veto of clear
    # value contradictions; merely weak candidates still compete on rank.
    if (length(candidates) > 0 && std_col == "n_obs") {
      implausible <- candidates[vapply(
        candidates,
        function(cand) {
          score_candidate_values(df, cand, std_col, matches[[cand]]$score) < confidence_threshold - 0.1
        },
        logical(1)
      )]

      if (length(implausible) > 0) {
        candidates <- setdiff(candidates, implausible)
        reason <- "their values do not look like sample sizes (dummies, counters, or transformed counts)"
        if (length(candidates) == 0) {
          log_warn("No column matched {.field {std_col}}: {.field {implausible}} matched by name but {reason}.")
        } else {
          log_debug("Skipped {.field {implausible}} as {.field {std_col}}: {reason}.")
        }
      }
    }

    if (length(candidates) > 0) {
      # If multiple candidates, use value analysis to resolve
      best_candidate <- if (length(candidates) > 1) {
        resolve_multiple_matches(df, candidates, std_col, matches)
      } else {
        candidates[1]
      }

      mapping[[std_col]] <- best_candidate
      used_cols <- c(used_cols, best_candidate)

      if (get_verbosity() >= 4) {
        score <- matches[[best_candidate]]$score
        method <- matches[[best_candidate]]$method
        req_label <- if (is_required) "required" else "optional"
        n_candidates <- length(candidates) + length(intersect(all_candidates, used_cols))
        multi_label <- if (n_candidates > 1) paste0(" [", n_candidates, " candidates]") else ""
        cli::cli_inform("Recognized {.field {best_candidate}} as {.field {std_col}} ({req_label}, score: {round(score, 2)}, method: {method}){multi_label}")
      }
    }
  }

  # Convert to format expected by artma (standard_name = data_name)
  mapping
}


#' @title Get required column names for artma
#' @description Returns the list of required column names for artma to function.
#'   Delegates to the template-derived single source of truth.
#' @return *\[character\]* Vector of required column names
get_required_column_names <- function() {
  box::use(artma / data / utils[get_required_colnames])
  get_required_colnames()
}


#' @title Check if recognized columns are sufficient
#' @description Check if the recognized columns include all required columns
#' @param mapping *\[list\]* Column mapping from recognize_columns
#' @return *\[list\]* List with 'complete' (logical), 'missing' (character vector)
check_mapping_completeness <- function(mapping) {
  required <- get_required_column_names()
  recognized <- names(mapping)

  missing <- setdiff(required, recognized)

  list(
    complete = length(missing) == 0,
    missing = missing,
    recognized = recognized
  )
}


box::export(
  CONTINUOUS_MEASURE_COLUMNS,
  MATCH_THRESHOLDS,
  get_column_patterns,
  looks_like_continuous_measure,
  clean_column_name,
  keyword_token_present,
  score_name_for_role,
  match_column_name,
  recognize_columns,
  get_required_column_names,
  check_mapping_completeness,
  score_rename_candidate,
  string_similarity,
  analyze_column_values,
  is_likely_numeric_id,
  is_likely_study_key,
  score_candidate_values,
  resolve_multiple_matches
)
