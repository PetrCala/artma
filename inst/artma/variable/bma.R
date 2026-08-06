#' @title BMA Variable Suggestion
#' @description Suggest variables for Bayesian Model Averaging with
#'   derived-encoding exclusion, collinearity detection, and a priority
#'   mechanism for important variables.

#' @title Get priority variables for BMA
#' @description Returns a list of variables that should be prioritized for BMA
#'   analysis due to their theoretical or methodological importance.
#' @return *\[character\]* Vector of priority variable names
get_bma_priority_variables <- function() {
  c(
    "se", # Standard error - critical for meta-analysis
    "study_size" # Sample size - theoretical importance
  )
}

#' @title Detect derived encodings of the effect or standard error
#' @description Flags candidate moderators that are near-deterministic
#'   transforms of the effect or its standard error, such as winsorized
#'   copies, t-statistics, or inverse standard errors. Meta-analysis datasets
#'   commonly ship such helper columns (e.g. `<effect>_w`, `tstat`, `invse`);
#'   letting one into the moderator set makes BMA regress the effect on an
#'   encoding of itself, which drives the inclusion probability of every
#'   genuine moderator toward zero and collapses downstream estimates (e.g.
#'   the best-practice estimate) to roughly the sample mean.
#'
#'   A candidate is flagged when its absolute correlation with any target
#'   series reaches `cor_threshold`. Targets are the effect, the standard
#'   error, and (when the standard error is available) the derived precision
#'   (1/se) and t-statistic (effect/se). Two correlation measures are taken
#'   per target and the larger one wins: Spearman rank correlation, which
#'   catches any monotone transform (winsorization, log, inverse), and
#'   Pearson correlation after clipping both series at their 1st and 99th
#'   percentiles, which catches near-linear copies without letting a few
#'   outliers mask the relationship.
#' @param df *\[data.frame\]* The data frame; must contain an `effect` column
#'   for the effect-based targets and an `se` column for the SE-based ones.
#' @param var_names *\[character\]* Candidate variable names to test.
#' @param cor_threshold *\[numeric\]* Absolute correlation at or above which a
#'   candidate counts as a derived encoding. Defaults to 0.9.
#' @param min_complete_pairs *\[integer\]* Minimum number of complete pairs
#'   required before a correlation is computed. Defaults to 10.
#' @return *\[data.frame\]* One row per flagged candidate, with columns:
#'   - `var_name` (character) - the flagged candidate
#'   - `target` (character) - label of the best-matching target series
#'   - `correlation` (numeric) - the winning absolute correlation
detect_derived_effect_encodings <- function(
  df,
  var_names,
  cor_threshold = 0.9,
  min_complete_pairs = 10
) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.data.frame(df),
    is.character(var_names),
    is.numeric(cor_threshold),
    is.numeric(min_complete_pairs)
  )

  empty <- data.frame(
    var_name = character(0),
    target = character(0),
    correlation = numeric(0),
    stringsAsFactors = FALSE
  )

  var_names <- intersect(var_names, names(df))
  if (!length(var_names)) {
    return(empty)
  }

  targets <- list()
  if ("effect" %in% names(df) && is.numeric(df$effect)) {
    effect <- as.numeric(df$effect)
    targets[["effect"]] <- effect
  }
  if ("se" %in% names(df) && is.numeric(df$se)) {
    se <- as.numeric(df$se)
    targets[["se"]] <- se
    safe_se <- se
    safe_se[!is.finite(safe_se) | safe_se == 0] <- NA_real_
    targets[["1/se (precision)"]] <- 1 / safe_se
    if (!is.null(targets[["effect"]])) {
      targets[["effect/se (t-statistic)"]] <- targets[["effect"]] / safe_se
    }
  }

  if (!length(targets)) {
    return(empty)
  }

  clip_tails <- function(x) {
    bounds <- stats::quantile(x, probs = c(0.01, 0.99), names = FALSE, type = 7)
    pmin(pmax(x, bounds[1]), bounds[2])
  }

  rows <- lapply(var_names, function(var_name) {
    x <- df[[var_name]]
    if (!is.numeric(x)) {
      return(NULL)
    }
    x <- as.numeric(x)

    best_target <- NA_character_
    best_cor <- -Inf

    for (target_name in names(targets)) {
      t_vals <- targets[[target_name]]
      ok <- is.finite(x) & is.finite(t_vals)
      if (sum(ok) < min_complete_pairs) {
        next
      }
      xs <- x[ok]
      ts <- t_vals[ok]

      cors <- suppressWarnings(c(
        stats::cor(clip_tails(xs), clip_tails(ts)),
        stats::cor(xs, ts, method = "spearman")
      ))
      cors <- abs(cors[is.finite(cors)])
      if (!length(cors)) {
        next
      }

      score <- max(cors)
      if (score > best_cor) {
        best_cor <- score
        best_target <- target_name
      }
    }

    if (!is.finite(best_cor) || best_cor < cor_threshold) {
      return(NULL)
    }

    data.frame(
      var_name = var_name,
      target = best_target,
      correlation = best_cor,
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(empty)
  }

  do.call(rbind, rows)
}

#' @title Filter out derived encodings of the effect or standard error
#' @description Runs `detect_derived_effect_encodings()` on the given
#'   candidates, honors per-column `bma_allow_derived` overrides from the data
#'   config, and reports what was flagged. Shared by the automatic suggestion
#'   path (which drops the flagged candidates) and the config-driven path in
#'   the bma method (which only warns).
#' @param df *\[data.frame\]* The data frame
#' @param var_names *\[character\]* Candidate variable names to test
#' @param config *\[list, optional\]* Data configuration with per-column
#'   entries; entries with `bma_allow_derived = TRUE` are never flagged.
#' @return *\[data.frame\]* The flagged subset, in the shape returned by
#'   `detect_derived_effect_encodings()`.
flag_derived_bma_candidates <- function(df, var_names, config = NULL) {
  box::use(artma / libs / core / utils[get_verbosity])

  derived <- detect_derived_effect_encodings(df, var_names)

  if (nrow(derived) && length(config)) {
    allowed <- vapply(derived$var_name, function(v) {
      isTRUE(config[[make.names(v)]]$bma_allow_derived)
    }, logical(1))

    if (any(allowed) && get_verbosity() >= 3) {
      cli::cli_inform(
        "Keeping {.val {derived$var_name[allowed]}} as BMA moderator candidate{?s} despite high correlation with the effect/SE ({.code bma_allow_derived} is set in the data config)."
      )
    }

    derived <- derived[!allowed, , drop = FALSE]
  }

  derived
}

#' @title Detect perfect collinearity in variable set
#' @description Tests whether a set of variables has perfect collinearity
#'   (dummy variable trap, linear dependencies, etc.)
#' @param df *\[data.frame\]* The data frame
#' @param var_names *\[character\]* Variable names to test
#' @return *\[list\]* List with:
#'   - `has_collinearity` (logical) - Whether collinearity detected
#'   - `aliased` (character) - Names of aliased variables
#'   - `rank` (integer) - Matrix rank
detect_perfect_collinearity <- function(df, var_names) {
  box::use(artma / libs / core / validation[validate])

  validate(is.data.frame(df), is.character(var_names))

  if (!length(var_names)) {
    return(list(
      has_collinearity = FALSE,
      aliased = character(0),
      rank = 0L
    ))
  }

  # Filter to existing columns
  var_names <- intersect(var_names, names(df))
  if (!length(var_names)) {
    return(list(
      has_collinearity = FALSE,
      aliased = character(0),
      rank = 0L
    ))
  }

  # Get complete cases
  subset_df <- df[, var_names, drop = FALSE]
  complete_rows <- stats::complete.cases(subset_df)
  subset_df <- subset_df[complete_rows, , drop = FALSE]

  if (nrow(subset_df) == 0) {
    return(list(
      has_collinearity = TRUE,
      aliased = var_names,
      rank = 0L
    ))
  }

  # Convert to numeric matrix
  X <- tryCatch(
    as.matrix(subset_df),
    error = function(e) NULL
  )

  if (is.null(X) || !is.numeric(X)) {
    return(list(
      has_collinearity = FALSE,
      aliased = character(0),
      rank = length(var_names)
    ))
  }

  # Add intercept
  X <- cbind(1, X)
  colnames(X) <- c("(Intercept)", var_names)

  # Check rank
  qr_decomp <- qr(X)
  rank <- qr_decomp$rank
  expected_rank <- ncol(X)

  if (rank < expected_rank) {
    # Identify aliased columns
    pivot <- qr_decomp$pivot
    aliased_idx <- pivot[(rank + 1):expected_rank]
    # Subtract 1 because first column is intercept
    aliased_idx <- aliased_idx[aliased_idx > 1] - 1
    aliased <- var_names[aliased_idx]

    return(list(
      has_collinearity = TRUE,
      aliased = aliased,
      rank = rank
    ))
  }

  list(
    has_collinearity = FALSE,
    aliased = character(0),
    rank = rank
  )
}

#' @title Remove collinear variables iteratively
#' @description Removes variables one by one until no perfect collinearity remains.
#'   Prioritizes keeping important variables and removing less informative ones.
#' @param df *\[data.frame\]* The data frame
#' @param var_names *\[character\]* Variable names to filter
#' @param priority_vars *\[character\]* Variables to prioritize keeping
#' @return *\[character\]* Filtered variable names without collinearity
remove_collinear_variables <- function(df, var_names, priority_vars = character(0)) {
  box::use(artma / libs / core / validation[validate])

  validate(is.data.frame(df), is.character(var_names))

  if (!length(var_names)) {
    return(character(0))
  }

  current_vars <- var_names
  max_iterations <- length(var_names) * 2

  for (i in seq_len(max_iterations)) {
    result <- detect_perfect_collinearity(df, current_vars)

    if (!result$has_collinearity) {
      return(current_vars)
    }

    aliased <- result$aliased
    if (!length(aliased)) {
      # No specific aliased variables identified, can't proceed
      return(current_vars)
    }

    # Choose which variable to remove
    # Prefer removing non-priority variables
    non_priority <- setdiff(aliased, priority_vars)

    if (length(non_priority)) {
      to_remove <- non_priority[1]
    } else {
      # All aliased are priority - have to remove one anyway
      to_remove <- aliased[1]
    }

    current_vars <- setdiff(current_vars, to_remove)

    if (!length(current_vars)) {
      return(character(0))
    }
  }

  # Hit max iterations - return what we have
  current_vars
}

#' @title Detect dummy variable groups with traps
#' @description Identifies dummy variable groups where all members are included,
#'   creating a dummy variable trap.
#' @param df *\[data.frame\]* The data frame
#' @param var_names *\[character\]* Variable names to check
#' @param config *\[list, optional\]* Data configuration
#' @return *\[list\]* List with:
#'   - `has_trap` (logical) - Whether dummy trap detected
#'   - `groups_with_trap` (list) - Groups that have all members included
detect_dummy_traps <- function(df, var_names, config = NULL) {
  box::use(
    artma / libs / core / validation[validate],
    artma / variable / detection[detect_variable_groups]
  )

  validate(is.data.frame(df), is.character(var_names))

  # Don't require config - detect_variable_groups can work without it
  groups_df <- detect_variable_groups(df, var_names = var_names, config = config)

  # Find dummy groups
  dummy_mask <- groups_df$group_type == "dummy"
  if (!any(dummy_mask)) {
    return(list(
      has_trap = FALSE,
      groups_with_trap = list()
    ))
  }

  dummy_groups_df <- groups_df[dummy_mask, , drop = FALSE]
  unique_group_ids <- unique(dummy_groups_df$group_id)

  groups_with_trap <- list()

  for (grp_id in unique_group_ids) {
    grp_vars <- dummy_groups_df$var_name[dummy_groups_df$group_id == grp_id]

    # Get members that are in var_names
    included_members <- intersect(grp_vars, var_names)

    # If all group members are included, it's a trap
    # (need at least 2 members and all present)
    if (length(included_members) >= 2 && setequal(grp_vars, included_members)) {
      # Find reference variable if any
      ref_vars <- dummy_groups_df$var_name[
        dummy_groups_df$group_id == grp_id &
          dummy_groups_df$is_reference
      ]
      reference <- if (length(ref_vars)) ref_vars[1] else NA_character_

      groups_with_trap[[grp_id]] <- list(
        group_id = grp_id,
        members = grp_vars,
        reference = reference
      )
    }
  }

  list(
    has_trap = length(groups_with_trap) > 0,
    groups_with_trap = groups_with_trap
  )
}

#' @title Remove dummy trap variables
#' @description Removes one variable from each dummy group that has all members
#'   included, preferring to keep priority variables.
#' @param df *\[data.frame\]* The data frame
#' @param var_names *\[character\]* Variable names to filter
#' @param priority_vars *\[character\]* Variables to prioritize keeping
#' @param config *\[list, optional\]* Data configuration
#' @return *\[character\]* Filtered variable names without dummy traps
remove_dummy_trap_variables <- function(df, var_names, priority_vars = character(0), config = NULL) {
  box::use(artma / libs / core / validation[validate])

  validate(is.data.frame(df), is.character(var_names))

  if (!length(var_names)) {
    return(character(0))
  }

  trap_result <- detect_dummy_traps(df, var_names, config)

  if (!trap_result$has_trap) {
    return(var_names)
  }

  vars_to_remove <- character(0)

  for (group in trap_result$groups_with_trap) {
    members <- group$members
    reference <- group$reference

    # Get members that are in var_names
    included_members <- intersect(members, var_names)

    if (!length(included_members)) {
      next
    }

    # Prefer removing reference if present
    if (length(reference) && reference %in% included_members) {
      vars_to_remove <- c(vars_to_remove, reference)
    } else {
      # Remove non-priority member
      non_priority <- setdiff(included_members, priority_vars)
      if (length(non_priority)) {
        vars_to_remove <- c(vars_to_remove, non_priority[1])
      } else {
        # All are priority - remove first one
        vars_to_remove <- c(vars_to_remove, included_members[1])
      }
    }
  }

  setdiff(var_names, vars_to_remove)
}

#' @title Suggest variables for BMA with collinearity checking
#' @description Suggests variables for Bayesian Model Averaging, excluding
#'   derived encodings of the effect or its standard error, ensuring no
#'   perfect collinearity, and prioritizing important variables.
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list, optional\]* Data configuration
#' @param min_obs_per_split *\[numeric\]* Minimum observations for splits
#' @param min_variance_ratio *\[numeric\]* Minimum coefficient of variation
#' @param exclude_reference *\[logical\]* Exclude reference dummies
#' @param priority_vars *\[character, optional\]* Additional priority variables
#' @return *\[data.frame\]* Suggested variables with metadata
suggest_variables_for_bma <- function(
  df,
  config = NULL,
  min_obs_per_split = 5,
  min_variance_ratio = 0.01,
  exclude_reference = TRUE,
  priority_vars = NULL
) {
  box::use(
    artma / libs / core / validation[validate],
    artma / variable / suggestion[suggest_variables_for_effect_summary],
    artma / libs / core / utils[get_verbosity]
  )

  validate(is.data.frame(df))

  if (is.null(config)) {
    box::use(artma / data_config / read[get_data_config])
    config <- get_data_config()
  }

  # Get base suggestions from effect summary logic
  base_suggestions <- suggest_variables_for_effect_summary(
    df,
    config = config,
    min_obs_per_split = min_obs_per_split,
    min_variance_ratio = min_variance_ratio,
    exclude_reference = exclude_reference
  )

  # Combine priority variables
  all_priority <- unique(c(get_bma_priority_variables(), priority_vars))

  # Add priority variables if they exist and are numeric
  added_priority <- character(0)
  for (pvar in all_priority) {
    if (!pvar %in% names(df)) {
      next
    }

    # Check if already suggested
    if (pvar %in% base_suggestions$var_name) {
      next
    }

    # Check if numeric
    var_data <- df[[pvar]]
    if (!is.numeric(var_data) && !is.integer(var_data)) {
      next
    }

    # Check if has variance
    var_sd <- stats::sd(var_data, na.rm = TRUE)
    if (is.na(var_sd) || var_sd == 0) {
      next
    }

    # Add to suggestions with correct column structure
    new_row <- data.frame(
      var_name = pvar,
      suggested = TRUE,
      split_method = "none",
      split_value = "none",
      reason = "priority_variable",
      group_id = "priority",
      stringsAsFactors = FALSE
    )

    base_suggestions <- rbind(base_suggestions, new_row)
    added_priority <- c(added_priority, pvar)
  }

  if (length(added_priority) > 0 && get_verbosity() >= 3) {
    cli::cli_inform(
      "Including {.val {added_priority}} as priority moderator{?s} by convention (the standard error term captures publication bias inside BMA)."
    )
  }

  # Step 0: Exclude derived encodings of the effect or its standard error
  # (winsorized copies, t-statistics, inverse SEs, ...). These pass the type,
  # variance, and perfect-collinearity filters below, yet keeping one makes
  # BMA regress the effect on an encoding of itself. Priority variables are
  # exempt: the se term is included by convention.
  derived_candidates <- setdiff(
    base_suggestions$var_name[base_suggestions$suggested],
    all_priority
  )
  derived <- flag_derived_bma_candidates(df, derived_candidates, config = config)

  if (nrow(derived)) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning(
        "Excluding {nrow(derived)} candidate moderator{?s} detected as derived encoding{?s} of the effect or its standard error: {.val {derived$var_name}}"
      )
      details <- sprintf(
        "%s: |cor| = %.3f with %s",
        derived$var_name, derived$correlation, derived$target
      )
      cli::cli_bullets(stats::setNames(details, rep("*", length(details))))
      cli::cli_inform(
        "Such columns make BMA regress the effect on a transform of itself, hiding every genuine moderator. To keep one anyway, set {.code bma_allow_derived: true} on its data config entry."
      )
    }
    base_suggestions <- base_suggestions[
      !base_suggestions$var_name %in% derived$var_name, ,
      drop = FALSE
    ]
  }

  # Get variable names
  suggested_vars <- base_suggestions$var_name

  if (get_verbosity() >= 4) {
    cli::cli_inform("Initial suggestions: {.val {length(suggested_vars)}} variables")
  }

  # Step 1: Remove dummy trap variables
  suggested_vars <- remove_dummy_trap_variables(
    df,
    suggested_vars,
    priority_vars = all_priority,
    config = config
  )

  if (get_verbosity() >= 4) {
    cli::cli_inform("After dummy trap removal: {.val {length(suggested_vars)}} variables")
  }

  # Step 2: Remove collinear variables iteratively
  suggested_vars <- remove_collinear_variables(
    df,
    suggested_vars,
    priority_vars = all_priority
  )

  if (get_verbosity() >= 3) {
    cli::cli_inform("After collinearity removal: {.val {length(suggested_vars)}} variables")
  }

  # Filter base_suggestions to only include remaining variables
  base_suggestions <- base_suggestions[base_suggestions$var_name %in% suggested_vars, , drop = FALSE]

  # Final verification
  final_check <- detect_perfect_collinearity(df, suggested_vars)
  if (final_check$has_collinearity) {
    if (get_verbosity() >= 2) {
      cli::cli_warn(
        "Collinearity still detected after filtering. Aliased: {.val {final_check$aliased}}"
      )
    }
  }

  base_suggestions
}

box::export(
  suggest_variables_for_bma,
  detect_derived_effect_encodings,
  detect_perfect_collinearity,
  detect_dummy_traps,
  flag_derived_bma_candidates,
  remove_collinear_variables,
  remove_dummy_trap_variables,
  get_bma_priority_variables
)
