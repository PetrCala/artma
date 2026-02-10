#' @title Variable suggestion for effect summary statistics
#' @description
#' Provides functionality to automatically suggest variables for effect summary
#' statistics analysis. Uses decision logic to determine which variables contain
#' meaningful information worth displaying to the user.

#' Suggest variables for effect summary statistics
#'
#' @description
#' Automatically suggests which variables should be included in effect summary
#' statistics analysis. Uses decision logic to evaluate variables based on:
#' - Data type (numeric, binary, ratio)
#' - Value distribution and variance
#' - Relationship to the effect variable
#' - Group membership
#'
#' The function determines appropriate split methods for each variable:
#' - Binary/dummy: equality splits (= 0, = 1)
#' - Ratio (0-1): split at 0.5
#' - Integer/numeric: split at mean or median
#'
#' @param df *\[data.frame\]* The data frame containing the data
#' @param config *\[list, optional\]* Data configuration list
#' @param min_obs_per_split *\[integer\]* Minimum observations required in each
#' split to suggest the variable. Defaults to 5.
#' @param min_variance_ratio *\[numeric\]* Minimum ratio of variance to mean
#' for numeric variables to be considered informative. Defaults to 0.01.
#' @param exclude_reference *\[logical\]* If TRUE, exclude reference variables
#' from dummy groups. Defaults to TRUE.
#'
#' @return A data frame with columns:
#' - var_name: Variable name
#' - suggested: Logical indicating if variable is suggested
#' - split_method: Suggested split method ("equal", "gltl", or NA)
#' - split_value: Suggested split value (for equal: the value to match; for gltl: threshold)
#' - reason: Reason for inclusion/exclusion
#' - group_id: Group identifier from detect_variable_groups
#'
#' @export
suggest_variables_for_effect_summary <- function(df, config = NULL,
                                                 min_obs_per_split = 5,
                                                 min_variance_ratio = 0.01,
                                                 exclude_reference = TRUE) {
  box::use(
    artma / variable / detection[detect_variable_groups],
    artma / libs / core / validation[validate, assert],
    artma / data / utils[determine_vector_type],
    artma / const[CONST]
  )

  validate(
    is.data.frame(df),
    is.numeric(min_obs_per_split),
    is.numeric(min_variance_ratio),
    is.logical(exclude_reference)
  )

  assert(
    "effect" %in% names(df),
    msg = "Data frame must contain an 'effect' column"
  )

  # Detect variable groups
  groups <- detect_variable_groups(df, config = config)

  # Reserved columns that should never be suggested
  reserved <- c("effect", "se", "study_id", "study_label", "study_size", "sample_size", "dof")

  results <- list()

  for (i in seq_len(nrow(groups))) {
    var_name <- groups$var_name[i]

    # Skip reserved columns
    if (var_name %in% reserved) {
      next
    }

    # Skip if not in data frame
    if (!var_name %in% names(df)) {
      next
    }

    var_data <- df[[var_name]]

    # Skip non-numeric variables
    if (!is.numeric(var_data) && !is.integer(var_data)) {
      results[[length(results) + 1]] <- data.frame(
        var_name = var_name,
        suggested = FALSE,
        split_method = NA_character_,
        split_value = NA_character_,
        reason = "non-numeric",
        group_id = groups$group_id[i],
        stringsAsFactors = FALSE
      )
      next
    }

    # Remove NA values for analysis
    valid_data <- var_data[!is.na(var_data) & is.finite(var_data)]
    valid_effect <- df$effect[!is.na(var_data) & is.finite(var_data) & is.finite(df$effect)]

    # Skip if insufficient data
    if (length(valid_data) < min_obs_per_split * 2) {
      results[[length(results) + 1]] <- data.frame(
        var_name = var_name,
        suggested = FALSE,
        split_method = NA_character_,
        split_value = NA_character_,
        reason = "insufficient_data",
        group_id = groups$group_id[i],
        stringsAsFactors = FALSE
      )
      next
    }

    # Skip if no variance (constant variable)
    if (length(unique(valid_data)) <= 1) {
      results[[length(results) + 1]] <- data.frame(
        var_name = var_name,
        suggested = FALSE,
        split_method = NA_character_,
        split_value = NA_character_,
        reason = "no_variance",
        group_id = groups$group_id[i],
        stringsAsFactors = FALSE
      )
      next
    }

    # Determine data type
    data_type <- tryCatch(
      determine_vector_type(var_data, CONST$DATA_CONFIG$DATA_TYPES),
      error = function(e) "unknown"
    )

    # Decision logic based on data type and values
    suggestion <- decide_variable_suggestion(
      var_data = valid_data,
      effect_data = valid_effect,
      data_type = data_type,
      is_reference = groups$is_reference[i],
      group_type = groups$group_type[i],
      min_obs_per_split = min_obs_per_split,
      min_variance_ratio = min_variance_ratio,
      exclude_reference = exclude_reference
    )

    results[[length(results) + 1]] <- data.frame(
      var_name = var_name,
      suggested = suggestion$suggested,
      split_method = suggestion$split_method,
      split_value = suggestion$split_value,
      reason = suggestion$reason,
      group_id = groups$group_id[i],
      stringsAsFactors = FALSE
    )
  }

  if (length(results)) {
    do.call(rbind, results)
  } else {
    data.frame(
      var_name = character(0),
      suggested = logical(0),
      split_method = character(0),
      split_value = character(0),
      reason = character(0),
      group_id = character(0),
      stringsAsFactors = FALSE
    )
  }
}


#' Decide whether to suggest a variable
#'
#' @description
#' Core decision logic for variable suggestion. Evaluates a variable based on
#' its characteristics and determines if it should be suggested for effect
#' summary statistics, along with the appropriate split method.
#'
#' @param var_data *\[numeric\]* Variable data (cleaned, no NAs)
#' @param effect_data *\[numeric\]* Corresponding effect data (cleaned, no NAs)
#' @param data_type *\[character\]* Data type from determine_vector_type
#' @param is_reference *\[logical\]* Is this a reference variable in a group?
#' @param group_type *\[character\]* Type of group this variable belongs to
#' @param min_obs_per_split *\[integer\]* Minimum observations per split
#' @param min_variance_ratio *\[numeric\]* Minimum variance ratio
#' @param exclude_reference *\[logical\]* Should reference variables be excluded?
#'
#' @return A list with:
#' - suggested: Logical
#' - split_method: Character ("equal", "gltl", or NA)
#' - split_value: Character (value or threshold)
#' - reason: Character (explanation)
#'
#' @keywords internal
decide_variable_suggestion <- function(var_data, effect_data, data_type,
                                       is_reference, group_type,
                                       min_obs_per_split, min_variance_ratio,
                                       exclude_reference) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.numeric(var_data),
    is.numeric(effect_data),
    is.character(data_type),
    is.logical(is_reference),
    is.character(group_type)
  )

  # Exclude reference variables in dummy groups
  if (exclude_reference && is_reference && group_type == "dummy") {
    return(list(
      suggested = FALSE,
      split_method = NA_character_,
      split_value = NA_character_,
      reason = "reference_variable"
    ))
  }

  unique_vals <- unique(var_data)
  n_unique <- length(unique_vals)

  # DUMMY VARIABLES (binary 0/1)
  if (data_type == "dummy" || (n_unique == 2 && all(unique_vals %in% c(0, 1)))) {
    # Check both splits have sufficient observations
    n_zero <- sum(var_data == 0)
    n_one <- sum(var_data == 1)

    if (n_zero >= min_obs_per_split && n_one >= min_obs_per_split) {
      # Check if there's meaningful difference in effects
      effect_zero <- effect_data[var_data == 0]
      effect_one <- effect_data[var_data == 1]

      mean_diff <- abs(mean(effect_zero, na.rm = TRUE) - mean(effect_one, na.rm = TRUE))
      pooled_sd <- sqrt((stats::var(effect_zero, na.rm = TRUE) + stats::var(effect_one, na.rm = TRUE)) / 2)

      # Suggest if there's at least some difference (not requiring significance)
      if (is.finite(mean_diff) && is.finite(pooled_sd) && pooled_sd > 0) {
        effect_size <- mean_diff / pooled_sd
        if (effect_size > 0.1) {  # Small effect threshold
          return(list(
            suggested = TRUE,
            split_method = "equal",
            split_value = "1",
            reason = "informative_dummy"
          ))
        }
      }

      return(list(
        suggested = TRUE,
        split_method = "equal",
        split_value = "1",
        reason = "dummy_sufficient_obs"
      ))
    }

    return(list(
      suggested = FALSE,
      split_method = NA_character_,
      split_value = NA_character_,
      reason = "insufficient_obs_per_category"
    ))
  }

  # PERCENTAGE/RATIO VARIABLES (0 to 1)
  if (data_type == "perc" || (min(var_data) >= 0 && max(var_data) <= 1 && n_unique > 2)) {
    # Split at 0.5
    n_below <- sum(var_data < 0.5)
    n_above <- sum(var_data >= 0.5)

    if (n_below >= min_obs_per_split && n_above >= min_obs_per_split) {
      return(list(
        suggested = TRUE,
        split_method = "gltl",
        split_value = "0.5",
        reason = "ratio_variable"
      ))
    }

    return(list(
      suggested = FALSE,
      split_method = NA_character_,
      split_value = NA_character_,
      reason = "insufficient_obs_per_split"
    ))
  }

  # INTEGER AND FLOAT VARIABLES
  if (data_type %in% c("int", "float")) {
    # Check variance is meaningful
    var_mean <- mean(var_data, na.rm = TRUE)
    var_sd <- stats::sd(var_data, na.rm = TRUE)

    if (!is.finite(var_mean) || !is.finite(var_sd) || var_mean == 0) {
      return(list(
        suggested = FALSE,
        split_method = NA_character_,
        split_value = NA_character_,
        reason = "undefined_variance"
      ))
    }

    coef_variation <- var_sd / abs(var_mean)

    if (coef_variation < min_variance_ratio) {
      return(list(
        suggested = FALSE,
        split_method = NA_character_,
        split_value = NA_character_,
        reason = "low_variance"
      ))
    }

    # Decide between mean and median based on skewness
    var_median <- stats::median(var_data, na.rm = TRUE)
    skewness <- (var_mean - var_median) / var_sd

    # Use median if highly skewed, otherwise mean
    split_val <- if (abs(skewness) > 1) {
      var_median
    } else {
      var_mean
    }

    # Check both splits have sufficient observations
    n_below <- sum(var_data < split_val)
    n_above <- sum(var_data >= split_val)

    if (n_below >= min_obs_per_split && n_above >= min_obs_per_split) {
      split_method <- if (abs(skewness) > 1) "median" else "mean"

      return(list(
        suggested = TRUE,
        split_method = "gltl",
        split_value = split_method,
        reason = "informative_numeric"
      ))
    }

    return(list(
      suggested = FALSE,
      split_method = NA_character_,
      split_value = NA_character_,
      reason = "insufficient_obs_per_split"
    ))
  }

  # CATEGORICAL VARIABLES (few unique values)
  if (data_type == "category" || n_unique <= 10) {
    # Check if any category has sufficient observations
    val_counts <- table(var_data)
    has_sufficient <- any(val_counts >= min_obs_per_split)

    if (has_sufficient) {
      # Suggest the most common category
      most_common <- names(val_counts)[which.max(val_counts)]

      return(list(
        suggested = TRUE,
        split_method = "equal",
        split_value = as.character(most_common),
        reason = "categorical_variable"
      ))
    }

    return(list(
      suggested = FALSE,
      split_method = NA_character_,
      split_value = NA_character_,
      reason = "insufficient_obs_per_category"
    ))
  }

  # DEFAULT: Treat as numeric with mean split
  var_mean <- mean(var_data, na.rm = TRUE)
  n_below <- sum(var_data < var_mean)
  n_above <- sum(var_data >= var_mean)

  if (n_below >= min_obs_per_split && n_above >= min_obs_per_split) {
    return(list(
      suggested = TRUE,
      split_method = "gltl",
      split_value = "mean",
      reason = "default_numeric"
    ))
  }

  list(
    suggested = FALSE,
    split_method = NA_character_,
    split_value = NA_character_,
    reason = "insufficient_obs_per_split"
  )
}

box::export(
  suggest_variables_for_effect_summary,
  decide_variable_suggestion
)
