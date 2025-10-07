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
        "^r$"
      ),
      keywords = c("effect", "estimate", "coef", "beta", "es"),
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

  if (str1 == str2) return(1.0)

  # Exact substring match
  if (grepl(str2, str1, fixed = TRUE) || grepl(str1, str2, fixed = TRUE)) {
    return(0.8)
  }

  # Calculate Levenshtein distance-based similarity
  max_len <- max(nchar(str1), nchar(str2))
  if (max_len == 0) return(0)

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
    exclude_keywords <- pattern_def$exclude_keywords %||% character(0)

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


#' @title Recognize columns in data frame
#' @description Automatically recognize which columns correspond to standard columns
#' @param df *\[data.frame\]* The data frame
#' @param min_confidence *\[numeric\]* Minimum confidence score (0-1) to accept a match
#' @return *\[list\]* Named list mapping standard columns to data frame columns
recognize_columns <- function(df, min_confidence = 0.7) {
  box::use(
    artma / libs / validation[validate],
    artma / libs / utils[get_verbosity]
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

  for (std_col in sorted_std_cols) {
    # Higher confidence threshold for optional columns to reduce false positives
    is_required <- patterns[[std_col]]$priority == 1
    confidence_threshold <- if (is_required) min_confidence else 0.95

    # Find all columns that matched this standard column
    candidates <- names(matches)[vapply(matches, function(m) {
      !is.na(m$match) && m$match == std_col && m$score >= confidence_threshold
    }, logical(1))]

    # Remove already used columns
    candidates <- setdiff(candidates, used_cols)

    if (length(candidates) > 0) {
      # Pick the best match
      candidate_scores <- vapply(candidates, function(c) matches[[c]]$score, numeric(1))
      best_candidate <- candidates[which.max(candidate_scores)]

      mapping[[std_col]] <- best_candidate
      used_cols <- c(used_cols, best_candidate)

      if (get_verbosity() >= 4) {
        score <- matches[[best_candidate]]$score
        method <- matches[[best_candidate]]$method
        req_label <- if (is_required) "required" else "optional"
        cli::cli_inform("Recognized {.field {best_candidate}} as {.field {std_col}} ({req_label}, score: {round(score, 2)}, method: {method})")
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
`%||%` <- function(x, y) if (is.null(x)) y else x


box::export(
  get_column_patterns,
  match_column_name,
  recognize_columns,
  get_required_column_names,
  check_mapping_completeness,
  string_similarity
)
