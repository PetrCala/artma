#' @title Variable group detection
#' @description
#' Provides functionality to automatically detect variable groups in a dataset.
#' Identifies patterns in data (dummy groups, transformations, etc.).
#'
#' The column-profiling primitives (`empty_group_df`, `group_row`,
#' `bind_group_rows`, `detect_dummy_groups`) live in `data/profile.R` so the
#' data layer can reuse them without importing from `variable/`.

box::use(
  artma / data / profile[
    empty_group_df,
    group_row,
    bind_group_rows,
    detect_dummy_groups
  ]
)

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
#' uses existing group_category values as the group_id/group_base for variables
#' that set it, instead of a detected group name. Structural type detection
#' (dummy, transformation, power, categorical) still runs and populates
#' group_type even for these variables, so setting group_category only
#' overrides the group's display label, not whether it is detected as a
#' structural group.
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
    artma / data / utils[get_reserved_colnames],
    artma / libs / core / validation[validate]
  )

  validate(is.data.frame(df))

  if (is.null(var_names)) {
    var_names <- setdiff(names(df), get_reserved_colnames())
  }

  if (!length(var_names)) {
    return(empty_group_df())
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
      if (is.na(groups$group_id[var_idx])) {
        # Not set from config: adopt the detected group wholesale
        groups$group_id[var_idx] <- all_detected$group_id[i]
        groups$group_type[var_idx] <- all_detected$group_type[i]
        groups$group_base[var_idx] <- all_detected$group_base[i]
        groups$is_reference[var_idx] <- all_detected$is_reference[i]
      } else if (groups$group_type[var_idx] == "singleton") {
        # group_id/group_base came from config (group_category): keep them as
        # the display grouping, but still record the structural type detected
        # from the data so methods like prima_facie_graphs can find the group.
        groups$group_type[var_idx] <- all_detected$group_type[i]
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

        results[[length(results) + 1]] <- group_row(
          var_name = var,
          group_id = paste0("transform_", base),
          group_type = "transformation",
          group_base = base,
          is_reference = FALSE
        )

        # Check if base variable exists (would be the reference)
        if (base %in% var_names) {
          results[[length(results) + 1]] <- group_row(
            var_name = base,
            group_id = paste0("transform_", base),
            group_type = "transformation",
            group_base = base,
            is_reference = TRUE
          )
        }
      }
    }
  }

  bind_group_rows(results)
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

        results[[length(results) + 1]] <- group_row(
          var_name = var,
          group_id = paste0("power_", base),
          group_type = "power",
          group_base = base,
          is_reference = FALSE
        )

        # Check if base variable exists (would be the reference)
        if (base %in% var_names) {
          results[[length(results) + 1]] <- group_row(
            var_name = base,
            group_id = paste0("power_", base),
            group_type = "power",
            group_base = base,
            is_reference = TRUE
          )
        }
      }
    }
  }

  bind_group_rows(results)
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
            results[[length(results) + 1]] <- group_row(
              var_name = v,
              group_id = paste0("categorical_", base),
              group_type = "categorical",
              group_base = base,
              is_reference = FALSE
            )
          }
        }
        break
      }
    }
  }

  unique(bind_group_rows(results))
}

box::export(
  detect_variable_groups,
  detect_dummy_groups,
  detect_transformation_groups,
  detect_power_groups,
  detect_categorical_groups
)
