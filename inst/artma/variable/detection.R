#' @title Variable group detection
#' @description
#' Provides functionality to automatically detect variable groups in a dataset.
#' Identifies patterns in data (dummy groups, transformations, etc.).

#' Detect variable groups in a dataset
#'
#' @description
#' Automatically detects groups of related variables based on their names,
#' data types, and values. Identifies:
#' - Dummy variable groups (e.g., country_usa, country_uk, country_france)
#' - Transformed variable groups (e.g., log_gdp, sqrt_gdp)
#' - Power transformation groups (e.g., var, var_sq, var_cu)
#' - Categorical splits (e.g., age_young, age_middle, age_old)
#'
#' @param df *\[data.frame\]* The data frame containing variables to analyze
#' @param var_names *\[character, optional\]* Vector of variable names to analyze.
#' If NULL, all columns except reserved names are analyzed.
#' @param config *\[list, optional\]* Data configuration list. If provided,
#' uses existing group_category values where available.
#'
#' @return A data frame with columns:
#' - var_name: Variable name
#' - group_id: Unique identifier for each group
#' - group_type: Type of group (dummy, transformation, power, categorical, singleton)
#' - group_base: Base name for the group (e.g., "country" for "country_usa")
#' - is_reference: Logical indicating if this is likely a reference variable
#'
#' @export
detect_variable_groups <- function(df, var_names = NULL, config = NULL) {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[validate]
  )

  validate(is.data.frame(df))

  # Reserved column names that should not be grouped
  reserved_names <- c("effect", "se", "study_id", "study_label", "study_size", "sample_size", "dof")

  if (is.null(var_names)) {
    var_names <- setdiff(names(df), reserved_names)
  }

  if (!length(var_names)) {
    return(data.frame(
      var_name = character(0),
      group_id = character(0),
      group_type = character(0),
      group_base = character(0),
      is_reference = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  # Check if we can use existing group categories from config
  existing_groups <- list()
  if (!is.null(config)) {
    for (var in var_names) {
      var_clean <- make.names(var)
      if (var_clean %in% names(config)) {
        grp <- config[[var_clean]]$group_category
        if (!is.null(grp) && !is.na(grp) && nzchar(grp)) {
          existing_groups[[var]] <- grp
        }
      }
    }
  }

  # Initialize results
  groups <- data.frame(
    var_name = var_names,
    group_id = rep(NA_character_, length(var_names)),
    group_type = rep("singleton", length(var_names)),
    group_base = var_names,
    is_reference = rep(FALSE, length(var_names)),
    stringsAsFactors = FALSE
  )

  # Use existing groups if available
  if (length(existing_groups) > 0) {
    for (i in seq_along(var_names)) {
      var <- var_names[i]
      if (var %in% names(existing_groups)) {
        groups$group_id[i] <- existing_groups[[var]]
        groups$group_base[i] <- existing_groups[[var]]
        # Keep singleton unless we detect it's part of a multi-variable group later
      }
    }
  }

  # Pattern detection
  # 1. Dummy variable groups (common prefix or suffix with underscore)
  dummy_groups <- detect_dummy_groups(var_names, df)

  # 2. Transformation groups (log_, sqrt_, ln_, etc.)
  transformation_groups <- detect_transformation_groups(var_names)

  # 3. Power transformation groups (_sq, _cu, etc.)
  power_groups <- detect_power_groups(var_names)

  # 4. Categorical split groups (_low, _high, _young, _old, etc.)
  categorical_groups <- detect_categorical_groups(var_names)

  # Merge all detected groups
  all_detected <- rbind(
    dummy_groups,
    transformation_groups,
    power_groups,
    categorical_groups
  )

  # Update groups data frame with detected patterns
  for (i in seq_len(nrow(all_detected))) {
    var_idx <- which(groups$var_name == all_detected$var_name[i])
    if (length(var_idx)) {
      # Only override if not already set from config
      if (is.na(groups$group_id[var_idx])) {
        groups$group_id[var_idx] <- all_detected$group_id[i]
        groups$group_type[var_idx] <- all_detected$group_type[i]
        groups$group_base[var_idx] <- all_detected$group_base[i]
        groups$is_reference[var_idx] <- all_detected$is_reference[i]
      }
    }
  }

  # Assign group IDs to singletons
  for (i in seq_len(nrow(groups))) {
    if (is.na(groups$group_id[i])) {
      groups$group_id[i] <- paste0("singleton_", groups$var_name[i])
    }
  }

  groups
}


#' Detect dummy variable groups
#'
#' @description
#' Identifies groups of dummy (binary 0/1) variables that share a common prefix,
#' suggesting they represent categories of the same underlying variable.
#'
#' @param var_names *\[character\]* Vector of variable names
#' @param df *\[data.frame\]* Data frame to check if variables are binary
#'
#' @return Data frame with group information for dummy variables
#'
#' @keywords internal
detect_dummy_groups <- function(var_names, df) {
  box::use(artma / libs / core / validation[validate])

  validate(is.character(var_names), is.data.frame(df))

  results <- list()

  # Check if variable is binary (0/1 or TRUE/FALSE)
  is_binary <- function(var_name) {
    if (!var_name %in% names(df)) return(FALSE)
    vals <- unique(df[[var_name]][!is.na(df[[var_name]])])
    length(vals) == 2 && all(vals %in% c(0, 1, TRUE, FALSE))
  }

  # Find variables with common prefixes
  binary_vars <- var_names[vapply(var_names, is_binary, logical(1))]

  if (!length(binary_vars)) {
    return(data.frame(
      var_name = character(0),
      group_id = character(0),
      group_type = character(0),
      group_base = character(0),
      is_reference = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  # Extract potential base names (everything before last underscore)
  extract_base <- function(name) {
    parts <- strsplit(name, "_")[[1]]
    if (length(parts) > 1) {
      paste(parts[-length(parts)], collapse = "_")
    } else {
      NA_character_
    }
  }

  bases <- vapply(binary_vars, extract_base, character(1))
  base_table <- table(bases[!is.na(bases)])

  # Only consider groups with 2+ variables
  valid_bases <- names(base_table)[base_table >= 2]

  if (!length(valid_bases)) {
    return(data.frame(
      var_name = character(0),
      group_id = character(0),
      group_type = character(0),
      group_base = character(0),
      is_reference = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  for (base in valid_bases) {
    group_vars <- binary_vars[!is.na(bases) & bases == base]

    # Try to detect reference variable (often has "other", "ref", "base" suffix)
    ref_patterns <- c("other", "ref", "reference", "base", "baseline")
    is_ref <- rep(FALSE, length(group_vars))

    for (i in seq_along(group_vars)) {
      suffix <- tolower(sub(paste0("^", base, "_"), "", group_vars[i]))
      if (suffix %in% ref_patterns) {
        is_ref[i] <- TRUE
      }
    }

    # If no explicit reference, mark the first as reference
    if (!any(is_ref) && length(group_vars) > 0) {
      is_ref[1] <- TRUE
    }

    for (i in seq_along(group_vars)) {
      results[[length(results) + 1]] <- data.frame(
        var_name = group_vars[i],
        group_id = paste0("dummy_", base),
        group_type = "dummy",
        group_base = base,
        is_reference = is_ref[i],
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(results)) {
    do.call(rbind, results)
  } else {
    data.frame(
      var_name = character(0),
      group_id = character(0),
      group_type = character(0),
      group_base = character(0),
      is_reference = logical(0),
      stringsAsFactors = FALSE
    )
  }
}


#' Detect transformation groups
#'
#' @description
#' Identifies variables that are transformations of base variables
#' (e.g., log_gdp, sqrt_population, ln_income).
#'
#' @param var_names *\[character\]* Vector of variable names
#'
#' @return Data frame with group information for transformed variables
#'
#' @keywords internal
detect_transformation_groups <- function(var_names) {
  box::use(artma / libs / core / validation[validate])

  validate(is.character(var_names))

  results <- list()

  # Common transformation prefixes
  transform_patterns <- c("log_", "ln_", "sqrt_", "exp_", "inv_", "abs_")

  for (var in var_names) {
    for (pattern in transform_patterns) {
      if (grepl(paste0("^", pattern), var)) {
        base <- sub(paste0("^", pattern), "", var)
        transform_type <- sub("_$", "", pattern)

        results[[length(results) + 1]] <- data.frame(
          var_name = var,
          group_id = paste0("transform_", base),
          group_type = "transformation",
          group_base = base,
          is_reference = FALSE,
          stringsAsFactors = FALSE
        )

        # Check if base variable exists (would be the reference)
        if (base %in% var_names) {
          results[[length(results) + 1]] <- data.frame(
            var_name = base,
            group_id = paste0("transform_", base),
            group_type = "transformation",
            group_base = base,
            is_reference = TRUE,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(results)) {
    do.call(rbind, results)
  } else {
    data.frame(
      var_name = character(0),
      group_id = character(0),
      group_type = character(0),
      group_base = character(0),
      is_reference = logical(0),
      stringsAsFactors = FALSE
    )
  }
}


#' Detect power transformation groups
#'
#' @description
#' Identifies variables that are power transformations of base variables
#' (e.g., var, var_sq, var_cu for squared and cubed).
#'
#' @param var_names *\[character\]* Vector of variable names
#'
#' @return Data frame with group information for power-transformed variables
#'
#' @keywords internal
detect_power_groups <- function(var_names) {
  box::use(artma / libs / core / validation[validate])

  validate(is.character(var_names))

  results <- list()

  # Common power suffixes
  power_patterns <- c("_sq", "_squared", "_cu", "_cubed", "_pow2", "_pow3")

  for (var in var_names) {
    for (pattern in power_patterns) {
      if (grepl(paste0(pattern, "$"), var)) {
        base <- sub(paste0(pattern, "$"), "", var)

        results[[length(results) + 1]] <- data.frame(
          var_name = var,
          group_id = paste0("power_", base),
          group_type = "power",
          group_base = base,
          is_reference = FALSE,
          stringsAsFactors = FALSE
        )

        # Check if base variable exists (would be the reference)
        if (base %in% var_names) {
          results[[length(results) + 1]] <- data.frame(
            var_name = base,
            group_id = paste0("power_", base),
            group_type = "power",
            group_base = base,
            is_reference = TRUE,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(results)) {
    do.call(rbind, results)
  } else {
    data.frame(
      var_name = character(0),
      group_id = character(0),
      group_type = character(0),
      group_base = character(0),
      is_reference = logical(0),
      stringsAsFactors = FALSE
    )
  }
}


#' Detect categorical split groups
#'
#' @description
#' Identifies variables that represent categorical splits of a base variable
#' (e.g., age_young, age_old; income_low, income_high).
#'
#' @param var_names *\[character\]* Vector of variable names
#'
#' @return Data frame with group information for categorically split variables
#'
#' @keywords internal
detect_categorical_groups <- function(var_names) {
  box::use(artma / libs / core / validation[validate])

  validate(is.character(var_names))

  results <- list()

  # Common categorical suffixes
  categorical_patterns <- c(
    "_low", "_high", "_medium", "_med",
    "_young", "_old", "_middle",
    "_small", "_large", "_big",
    "_early", "_late",
    "_below", "_above"
  )

  # Track which bases we've seen
  seen_bases <- character(0)

  for (var in var_names) {
    for (pattern in categorical_patterns) {
      if (grepl(paste0(pattern, "$"), var)) {
        base <- sub(paste0(pattern, "$"), "", var)

        # Check if other variables with the same base exist
        other_vars <- character(0)
        for (other_pattern in categorical_patterns) {
          potential_var <- paste0(base, other_pattern)
          if (potential_var %in% var_names && potential_var != var) {
            other_vars <- c(other_vars, potential_var)
          }
        }

        # Only group if we have multiple variables with the same base
        if (length(other_vars) > 0 && !base %in% seen_bases) {
          seen_bases <- c(seen_bases, base)

          # Add all related variables
          all_vars <- c(var, other_vars)
          for (v in all_vars) {
            results[[length(results) + 1]] <- data.frame(
              var_name = v,
              group_id = paste0("categorical_", base),
              group_type = "categorical",
              group_base = base,
              is_reference = FALSE,
              stringsAsFactors = FALSE
            )
          }
        }
        break
      }
    }
  }

  if (length(results)) {
    unique(do.call(rbind, results))
  } else {
    data.frame(
      var_name = character(0),
      group_id = character(0),
      group_type = character(0),
      group_base = character(0),
      is_reference = logical(0),
      stringsAsFactors = FALSE
    )
  }
}

box::export(
  detect_variable_groups,
  detect_dummy_groups,
  detect_transformation_groups,
  detect_power_groups,
  detect_categorical_groups
)
