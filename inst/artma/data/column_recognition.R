#' @title Define column patterns for recognition
#' @description Returns a list of patterns for recognizing standard columns
#' @return *\[list\]* Named list of regex patterns and keywords for each standard column
get_column_patterns <- function() {
  list(
    study = list(
      patterns = c(
        "^study[_\\.]?name$",
        "^study$",
        "^author[_\\.]?name$",
        "^paper$",
        "^publication$",
        "^source$"
      ),
      keywords = c("study", "name", "author", "paper", "publication"),
      exclude_keywords = c("id", "size"),
      priority = 1
    ),
    effect = list(
      patterns = c(
        "^effect[_\\.]?(size)?$",
        "^estimate[sd]?$",
        "^coeff?(icient)?$",
        "^beta$",
        "^b$",
        "^es$",
        "^d$",
        "^g$",
        "^r$",
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
      exclude_keywords = c("standard", "error", "se")
    ),
    se = list(
      patterns = c(
        "^se$",
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
        "^observations?$",
        "^n[_\\.]?observations?$",
        "^obs[_\\.]?n$"
      ),
      keywords = c("obs", "sample", "size"),
      priority = 2
    ),
    t_stat = list(
      patterns = c(
        "^t[_\\.]?stat(istic)?$",
        "^t[_\\.]?value$",
        "^tval$"
      ),
      keywords = c("stat", "tvalue", "tval"),
      priority = 2
    ),
    study_id = list(
      patterns = c(
        "^study[_\\.]?id$",
        "^studyid$",
        "^sid$"
      ),
      keywords = c("studyid"),
      exclude_keywords = c("name", "size"),
      priority = 3
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


#' @title Match column name to standard column
#' @description Attempts to match a data frame column name to a standard column
#' @param col_name *\[character\]* Column name from the data frame
#' @param patterns *\[list\]* Patterns for recognition (from get_column_patterns)
#' @return *\[list\]* Match result with 'match' (column name or NA), 'score' (0-1), 'method' (how it matched)
match_column_name <- function(col_name, patterns) {
  col_name_clean <- tolower(trimws(col_name))
  col_name_clean <- gsub("[^a-z0-9_]", "_", col_name_clean)

  best_score <- 0
  best_match <- NA_character_
  best_method <- NA_character_

  for (std_col in names(patterns)) {
    pattern_def <- patterns[[std_col]]

    # Check regex patterns (highest priority)
    for (pattern in pattern_def$patterns) {
      if (grepl(pattern, col_name_clean, ignore.case = TRUE)) {
        score <- 1.0
        if (score > best_score) {
          best_score <- score
          best_match <- std_col
          best_method <- "regex"
        }
      }
    }

    # Check keyword matching
    keywords <- pattern_def$keywords
    exclude_keywords <- if (is.null(pattern_def$exclude_keywords)) character(0) else pattern_def$exclude_keywords

    # Check if any exclude keywords are present
    has_exclude <- any(vapply(exclude_keywords, function(kw) {
      grepl(kw, col_name_clean, ignore.case = TRUE)
    }, logical(1)))

    if (has_exclude) next

    # Calculate keyword match score
    keyword_matches <- vapply(keywords, function(kw) {
      string_similarity(col_name_clean, kw)
    }, numeric(1))

    max_keyword_score <- max(keyword_matches, 0)

    # For multi-keyword matches, boost score
    n_keywords_found <- sum(vapply(keywords, function(kw) {
      grepl(kw, col_name_clean, ignore.case = TRUE)
    }, logical(1)))

    if (n_keywords_found > 0) {
      keyword_score <- max_keyword_score + (n_keywords_found - 1) * 0.1
      keyword_score <- min(keyword_score, 0.95) # Cap below regex matches

      if (keyword_score > best_score) {
        best_score <- keyword_score
        best_match <- std_col
        best_method <- "keyword"
      }
    }
  }

  list(
    match = best_match,
    score = best_score,
    method = best_method
  )
}


#' @title Analyze column values to determine semantic type
#' @description Analyzes actual data values to help discriminate between ambiguous column matches
#' @param values *\[vector\]* Column values to analyze
#' @return *\[list\]* Analysis results with various heuristics
analyze_column_values <- function(values) {
  # Remove NA values for analysis
  values_clean <- values[!is.na(values)]

  if (length(values_clean) == 0) {
    return(list(
      is_sequential = FALSE,
      is_unique = FALSE,
      is_numeric = FALSE,
      uniqueness_ratio = 0,
      mean = NA,
      variance = NA,
      min = NA,
      max = NA
    ))
  }

  # Check if numeric (coercible to numeric)
  is_numeric <- is.numeric(values_clean) || !any(is.na(suppressWarnings(as.numeric(values_clean))))
  numeric_values <- if (is_numeric) {
    if (is.numeric(values_clean)) values_clean else as.numeric(values_clean)
  } else {
    numeric(0)
  }

  # Sequential pattern detection (like 1, 2, 3, 4, 5...)
  is_sequential <- FALSE
  if (is_numeric && length(numeric_values) >= 3) {
    diffs <- diff(numeric_values)
    # Check if differences are constant (allowing for some tolerance)
    is_sequential <- all(abs(diffs - diffs[1]) < 1e-10) && abs(diffs[1] - 1) < 1e-10
  }

  # Uniqueness analysis
  is_unique <- length(unique(values_clean)) == length(values_clean)
  uniqueness_ratio <- length(unique(values_clean)) / length(values_clean)

  # Statistical properties
  stats <- if (is_numeric && length(numeric_values) > 0) {
    list(
      mean = mean(numeric_values, na.rm = TRUE),
      variance = stats::var(numeric_values, na.rm = TRUE),
      min = min(numeric_values, na.rm = TRUE),
      max = max(numeric_values, na.rm = TRUE)
    )
  } else {
    list(mean = NA, variance = NA, min = NA, max = NA)
  }

  list(
    is_sequential = is_sequential,
    is_unique = is_unique,
    is_numeric = is_numeric,
    uniqueness_ratio = uniqueness_ratio,
    mean = stats$mean,
    variance = stats$variance,
    min = stats$min,
    max = stats$max
  )
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

    if (analysis$is_unique && analysis$uniqueness_ratio > 0.95) {
      # Moderate penalty for high uniqueness (IDs are unique, sample sizes may repeat)
      value_penalty <- value_penalty + 0.15
    }

    if (analysis$is_numeric) {
      # Check if values are in reasonable range for sample sizes
      if (!is.na(analysis$min) && analysis$min < 1) {
        value_penalty <- value_penalty + 0.1
      }
      if (!is.na(analysis$max) && analysis$max > 1e6) {
        # Extremely large values unlikely to be sample sizes
        value_penalty <- value_penalty + 0.1
      }
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
  } else if (std_col %in% c("effect", "se", "t_stat")) {
    # Effect sizes, standard errors, t-stats should:
    # - Not be sequential
    # - Have reasonable variance
    # - Not be all unique (some repetition expected)

    if (analysis$is_sequential) {
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

  if (get_verbosity() >= 4) {
    cli::cli_inform("Resolved multiple matches for {.field {std_col}}:")
    for (cand in candidates) {
      name_score <- matches[[cand]]$score
      value_score <- candidate_scores[cand]
      analysis <- analyze_column_values(df[[cand]])
      marker <- if (cand == best_candidate) "\u2713" else " "
      cli::cli_inform(
        "  {marker} {.field {cand}}: name={round(name_score, 2)}, adjusted={round(value_score, 2)} (seq={analysis$is_sequential}, uniq={round(analysis$uniqueness_ratio, 2)})"
      )
    }
  }

  best_candidate
}


#' @title Recognize columns in data frame
#' @description Automatically recognize which columns correspond to standard columns
#' @param df *\[data.frame\]* The data frame
#' @param min_confidence *\[numeric\]* Minimum confidence score (0-1) to accept a match
#' @return *\[list\]* Named list mapping standard columns to data frame columns
recognize_columns <- function(df, min_confidence = 0.7) {
  box::use(
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

  # Sort patterns by priority
  pattern_priority <- vapply(patterns, function(p) as.integer(p$priority), integer(1))
  sorted_std_cols <- names(patterns)[order(pattern_priority)]

  # Get required column names
  required_cols <- get_required_column_names()

  for (std_col in sorted_std_cols) {
    # Higher confidence threshold for optional columns to reduce false positives
    is_required <- std_col %in% required_cols
    confidence_threshold <- if (is_required) min_confidence else 0.95

    # Find all columns that matched this standard column
    candidates <- names(matches)[vapply(matches, function(m) {
      !is.na(m$match) && m$match == std_col && m$score >= confidence_threshold
    }, logical(1))]

    # Remove already used columns
    candidates <- setdiff(candidates, used_cols)

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
        n_candidates <- length(candidates) + length(intersect(names(matches)[vapply(matches, function(m) {
          !is.na(m$match) && m$match == std_col && m$score >= confidence_threshold
        }, logical(1))], used_cols))
        multi_label <- if (n_candidates > 1) paste0(" [", n_candidates, " candidates]") else ""
        cli::cli_inform("Recognized {.field {best_candidate}} as {.field {std_col}} ({req_label}, score: {round(score, 2)}, method: {method}){multi_label}")
      }
    }
  }

  # Convert to format expected by artma (standard_name = data_name)
  mapping
}


#' @title Get required column names for artma
#' @description Returns the list of required column names for artma to function
#' @return *\[character\]* Vector of required column names
get_required_column_names <- function() {
  c("study", "effect", "se", "n_obs")
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


# Helper for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x # nolint


box::export(
  get_column_patterns,
  match_column_name,
  recognize_columns,
  get_required_column_names,
  check_mapping_completeness,
  string_similarity,
  analyze_column_values,
  score_candidate_values,
  resolve_multiple_matches
)
