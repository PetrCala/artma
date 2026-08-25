#' @title Add observation ID column
#' @description Add an observation ID column to the data frame if missing.
#'   Creates a sequential integer ID from 1 to nrow.
#' @param df *\[data.frame\]* The data frame to add the observation ID column to.
#' @return *\[data.frame\]* The data frame with the observation ID column.
#' @keywords internal
add_obs_id_column <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (!"obs_id" %in% colnames(df)) {
    df$obs_id <- seq_len(nrow(df))
    if (get_verbosity() >= 4) {
      cli::cli_inform("Created {.field obs_id} column with sequential integers")
    }
  } else {
    # Validate existing obs_id
    expected_ids <- seq_len(nrow(df))
    invalid_idxs <- which(df$obs_id != expected_ids)
    n_invalid_idxs <- length(invalid_idxs)

    if (n_invalid_idxs > 0) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(c(
          "!" = "Found {n_invalid_idxs} invalid observation IDs in the column {.val obs_id}.",
          "i" = "Resetting them to sequential integers."
        ))
      }
      df$obs_id[invalid_idxs] <- expected_ids[invalid_idxs]
    }
  }

  df
}

#' @title Check whether label values are human-legible
#' @description Study labels count as legible when they are mostly non-numeric
#'   text ("Smith (2009)"), mirroring the value heuristics of
#'   \code{is_likely_study_key}.
#' @param labels *\[character\]* Candidate label values.
#' @return *\[logical\]* TRUE if the values read as human-legible labels.
#' @keywords internal
is_legible_label_vector <- function(labels) {
  labels <- trimws(as.character(labels))
  labels <- labels[!is.na(labels) & nzchar(labels)]

  if (length(labels) == 0) {
    return(FALSE)
  }

  numeric_like_ratio <- mean(grepl("^[-+]?[0-9]+(\\.[0-9]+)?$", labels))
  if (numeric_like_ratio > 0.8) {
    return(FALSE)
  }

  mean(grepl("[A-Za-z]", labels)) >= 0.6
}


#' @title Check that labels group rows exactly like study IDs
#' @description A label column can stand in for the study key only when the
#'   mapping is one-to-one: every study ID carries a single label and no two
#'   studies share one.
#' @param labels *\[character\]* Candidate label values, one per row.
#' @param study_ids *\[integer\]* Normalized study IDs, one per row.
#' @return *\[logical\]* TRUE if labels and IDs induce the same grouping.
#' @keywords internal
labels_align_with_study_ids <- function(labels, study_ids) {
  if (any(is.na(labels) | !nzchar(labels)) || any(is.na(study_ids))) {
    return(FALSE)
  }

  n_ids <- length(unique(study_ids))
  n_labels <- length(unique(labels))
  n_pairs <- length(unique(paste(study_ids, labels, sep = "\r")))

  n_pairs == n_ids && n_labels == n_ids
}


#' @title Find a human-legible source column for study labels
#' @description When the study key column holds numeric IDs, the frame often
#'   still carries the citation-style study names under another column
#'   ("study", "author", ...). Returns the best such column: it must look like
#'   study labels, group rows exactly like the study IDs, and carry a
#'   study-name-like column name (an existing \code{study_label} column always
#'   qualifies). NULL when the current labels are already legible or no
#'   suitable column exists.
#' @param df *\[data.frame\]* The data frame.
#' @param study_ids *\[integer\]* Normalized study IDs, one per row.
#' @param current_labels *\[character\]* Labels derived from the study key.
#' @return *\[character\]* Column name, or NULL.
#' @keywords internal
find_study_label_source <- function(df, study_ids, current_labels) {
  box::use(
    artma / data / column_recognition[MATCH_THRESHOLDS, get_column_patterns, match_column_name],
    artma / data / utils[get_reserved_colnames]
  )

  if (is_legible_label_vector(current_labels)) {
    return(NULL)
  }

  candidates <- setdiff(names(df), setdiff(get_reserved_colnames(), "study_label"))
  if (length(candidates) == 0) {
    return(NULL)
  }

  study_patterns <- get_column_patterns()["study_id"]

  scores <- vapply(candidates, function(col) {
    values <- df[[col]]
    if (!(is.character(values) || is.factor(values))) {
      return(NA_real_)
    }
    labels <- trimws(as.character(values))
    if (!is_legible_label_vector(labels)) {
      return(NA_real_)
    }
    if (!labels_align_with_study_ids(labels, study_ids)) {
      return(NA_real_)
    }
    if (identical(col, "study_label")) {
      # A user-provided study_label column beats any name-matched candidate
      return(2)
    }
    match <- match_column_name(col, study_patterns)
    if (identical(match$match, "study_id") && match$score >= MATCH_THRESHOLDS$optional_confidence) {
      match$score
    } else {
      NA_real_
    }
  }, numeric(1))

  if (all(is.na(scores))) {
    return(NULL)
  }

  candidates[which.max(scores)]
}


#' @title Add study ID column
#' @description Add or normalize the study ID column. Uses the existing
#'   \code{study_id} column (character or integer), preserves its original
#'   labels in \code{study_label}, and overwrites \code{study_id} with
#'   integer IDs derived from factorizing its values. When the study key is
#'   numeric and another column carries human-legible study names that group
#'   rows the same way, \code{study_label} is taken from that column instead.
#' @param df *\[data.frame\]* The data frame with a \code{study_id} column.
#' @return *\[data.frame\]* The data frame with \code{study_id} as integer IDs
#'   and \code{study_label} as the study label values.
#' @keywords internal
add_study_id_column <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  study_src <- df$study_id

  if (!length(study_src) == nrow(df)) {
    cli::cli_abort("The number of study_id values must be equal to the number of rows in the data frame.")
  }

  valid_ids <- as.integer(factor(study_src, levels = unique(study_src)))

  study_labels <- as.character(study_src)
  label_source <- "the original study keys"

  label_col <- find_study_label_source(df, valid_ids, study_labels)
  if (!is.null(label_col)) {
    study_labels <- as.character(df[[label_col]])
    label_source <- sprintf("the column '%s'", label_col)
    if (get_verbosity() >= 3 && !identical(label_col, "study_label")) {
      cli::cli_alert_info(
        "The study keys are numeric; using {.field {label_col}} for {.field study_label} instead."
      )
    }
  }

  if (!"study_label" %in% colnames(df)) {
    df$study_label <- study_labels
    if (get_verbosity() >= 4) {
      cli::cli_inform("Created {.field study_label} column from {label_source}")
    }
  } else if (!identical(as.character(df$study_label), study_labels)) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning(c(
        "!" = "Existing {.val study_label} values differ from {label_source}.",
        "i" = "Overwriting {.val study_label}."
      ))
    }
    df$study_label <- study_labels
  }

  if ("study_id" %in% colnames(df)) {
    invalid_or_missing_ids <- which(is.na(study_src) | trimws(as.character(study_src)) == "")
    if (length(invalid_or_missing_ids) > 0) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(c(
          "!" = "Found {length(invalid_or_missing_ids)} invalid or missing study IDs in the column {.val study_id}. ",
          "i" = "Resetting them to sequential integers."
        ))
      }
    }
  }

  df$study_id <- valid_ids

  df
}

#' @title Add t-statistic column
#' @description Add a t-statistic column to the data frame.
#' @param df *\[data.frame\]* The data frame to add the t-statistic column to.
#' @return *\[data.frame\]* The data frame with the t-statistic column.
#' @keywords internal
add_t_stat_column <- function(df) {
  box::use(
    calc = artma / calc / index
  )

  if ("t_stat" %in% colnames(df)) {
    if (any(is.na(df$t_stat))) {
      n_missing <- sum(is.na(df$t_stat))
      cli::cli_abort(c(
        "!" = "Found {n_missing} missing t-statistics in the column {.val t_stat}.",
        "i" = "Please add these to your data frame, or remove the {.val t_stat} column mapping so they are computed automatically.",
        "i" = "You can remove the mapping by running {.code artma::config_set('t_stat', source_name = NA)}."
      ))
    }
  }

  df$t_stat <- calc$t_stat(effect = df$effect, se = df$se)

  df
}

#' @title Add study size column
#' @description Add a study size column to the data frame.
#' @param df *\[data.frame\]* The data frame to add the study size column to.
#' @return *\[data.frame\]* The data frame with the study size column.
#' @keywords internal
add_study_size_column <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  study_id_col <- df$study_id

  freq_table <- table(study_id_col)
  study_size_col <- unname(as.integer(freq_table[as.character(study_id_col)]))

  if ("study_size" %in% colnames(df)) {
    invalid_or_missing_ids <- which(is.na(df$study_size) | df$study_size != study_size_col)
    if (length(invalid_or_missing_ids) > 0) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(c(
          "!" = "Found {length(invalid_or_missing_ids)} invalid or missing study sizes in the column {.val study_size}.",
          "i" = "Resetting them to the expected values."
        ))
      }
    }
  }

  df$study_size <- study_size_col

  df
}

#' @title Add regression degrees of freedom column
#' @description Add a reg_dof column to the data frame if missing.
#'   Calculates as n_obs - 2 (simple regression assumption).
#' @param df *\[data.frame\]* The data frame to add the reg_dof column to.
#' @return *\[data.frame\]* The data frame with the reg_dof column.
#' @keywords internal
add_reg_dof_column <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (!"reg_dof" %in% colnames(df)) {
    # Calculate reg_dof from n_obs if available
    if ("n_obs" %in% colnames(df)) {
      df$reg_dof <- df$n_obs - 2
      if (get_verbosity() >= 4) {
        cli::cli_inform("Created {.field reg_dof} column from n_obs (n_obs - 2)")
      }

      # Warn about negative or zero degrees of freedom
      negative_dof <- which(df$reg_dof <= 0)
      if (length(negative_dof) > 0) {
        if (get_verbosity() >= 2) {
          cli::cli_alert_warning(c(
            "!" = "Found {length(negative_dof)} rows with non-positive degrees of freedom.",
            "i" = "This occurs when n_obs <= 2. Setting these to NA."
          ))
        }
        df$reg_dof[negative_dof] <- NA_real_
      }
    } else {
      # If n_obs is not available, set to NA
      df$reg_dof <- NA_real_
      if (get_verbosity() >= 4) {
        cli::cli_inform("Created {.field reg_dof} column with NA (n_obs not available)")
      }
    }
  } else {
    # Validate existing reg_dof
    negative_dof <- which(!is.na(df$reg_dof) & df$reg_dof < 0)
    if (length(negative_dof) > 0) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(c(
          "!" = "Found {length(negative_dof)} rows with negative degrees of freedom.",
          "i" = "Setting these to NA."
        ))
      }
      df$reg_dof[negative_dof] <- NA_real_
    }
  }

  df
}


#' @title Add precision column
#' @description Add a precision column to the data frame if missing.
#'   Calculates based on precision_type option: 1/SE or sqrt(DoF).
#' @param df *\[data.frame\]* The data frame to add the precision column to.
#' @return *\[data.frame\]* The data frame with the precision column.
#' @keywords internal
add_precision_column <- function(df) {
  box::use(
    calc = artma / calc / index,
    artma / libs / core / utils[get_verbosity],
    artma / options / typed_accessors[get_precision_type, get_winsorization_level]
  )

  winsorization_level <- get_winsorization_level()
  winsorization_active <- !is.null(winsorization_level) &&
    !is.na(winsorization_level) && winsorization_level != 0

  if ("precision" %in% colnames(df)) {
    if (!is.numeric(df$precision)) {
      cli::cli_abort("The column {.val precision} must be numeric.")
    }

    if (winsorization_active) {
      # se (and thus precision) was winsorized upstream; recompute precision
      # from the winsorized se/reg_dof so it stays consistent with
      # effect/se/t_stat instead of keeping stale pre-winsorization values.
      if (get_verbosity() >= 3) {
        cli::cli_alert_info("Recalculating {.field precision} from winsorized data.")
      }
      df$precision <- calc$precision(se = df$se, reg_dof = df$reg_dof)
    } else if (any(is.na(df$precision))) {
      n_missing <- sum(is.na(df$precision))
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(c(
          "!" = "Found {n_missing} missing precision values.",
          "i" = "Recalculating these based on precision_type option."
        ))
      }
      # Recalculate missing values
      missing_idx <- which(is.na(df$precision))
      calculated_precision <- calc$precision(se = df$se, reg_dof = df$reg_dof)
      df$precision[missing_idx] <- calculated_precision[missing_idx]
    }
  } else {
    # Calculate precision based on option
    precision_type <- get_precision_type()
    df$precision <- calc$precision(se = df$se, reg_dof = df$reg_dof)

    if (get_verbosity() >= 4) {
      cli::cli_inform("Created {.field precision} column using {.val {precision_type}} method")
    }
  }

  df
}

#' @title Update config with computed columns
#' @description Add computed columns to the data config so they can be used
#'   in variable summaries and other analyses.
#' @param df *\[data.frame\]* The data frame with computed columns
#' @keywords internal
update_config_with_computed_columns <- function(df) {
  box::use(
    artma / data_config / read[get_data_config],
    artma / data_config / write[update_data_config],
    artma / data / derivation[pcc_derivation_active],
    artma / data / utils[determine_vector_type],
    artma / const[CONST],
    artma / libs / core / string[make_verbose_name],
    artma / libs / core / utils[get_verbosity]
  )

  # Get current config
  config <- get_data_config()

  # List of computed columns to add. Under the (t, df) route effect and se are
  # derived rather than read, so they join the list and are recorded as
  # computed instead of carrying a source_name.
  computed_columns <- c("obs_id", "study_id", "study_label", "t_stat", "study_size", "reg_dof", "precision")
  if (pcc_derivation_active()) {
    computed_columns <- c(computed_columns, "effect", "se")
  }

  # Add each computed column to config if it exists in df and not in config
  changes <- list()
  for (col_name in computed_columns) {
    if (col_name %in% colnames(df)) {
      config_key <- make.names(col_name)

      if (!config_key %in% names(config)) {
        col_data <- df[[col_name]]
        col_data_type <- tryCatch(
          determine_vector_type(col_data, CONST$DATA_CONFIG$DATA_TYPES),
          error = function(e) "unknown"
        )

        changes[[config_key]] <- list(
          var_name = col_name,
          var_name_verbose = make_verbose_name(col_name),
          data_type = col_data_type,
          variable_summary = !identical(col_name, "study_label"), # Keep labels out of numeric summaries
          effect_sum_stats = FALSE, # Don't split by computed columns
          is_computed = TRUE # Mark as computed for clarity
        )

        if (get_verbosity() >= 4) {
          cli::cli_inform("Added computed column {.field {col_name}} to data config")
        }
      }
    }
  }

  # Update config with changes
  if (length(changes) > 0) {
    suppressMessages(update_data_config(changes))
  }

  invisible(NULL)
}


#' @title Compute optional columns
#' @description Compute optional columns that the user did not provide.
#'   This step is pure: it only derives columns and never writes the data
#'   config. Registering the computed columns in the data config is the job of
#'   the persist phase (`update_config_with_computed_columns`), which runs
#'   whether or not this cached compute step executed.
#'   This includes:
#'   - obs_id: Sequential observation IDs (1 to nrow)
#'   - study_id: Integer study IDs based on study names
#'   - study_label: Original study key labels before integer normalization
#'   - t_stat: T-statistics calculated from effect/se
#'   - study_size: Number of estimates per study
#'   - reg_dof: Regression degrees of freedom (n_obs - 2)
#'   - precision: Precision calculated as 1/SE or sqrt(DoF)
#' @param df *\[data.frame\]* The data frame to compute the optional columns for.
#' @return *\[data.frame\]* The data frame with all optional columns computed.
compute_optional_columns <- function(df) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Computing optional columns for {.val {nrow(df)}} observations...")
  }

  df |>
    add_obs_id_column() |>
    add_study_id_column() |>
    add_t_stat_column() |>
    add_study_size_column() |>
    add_reg_dof_column() |>
    add_precision_column()
}

box::export(compute_optional_columns, update_config_with_computed_columns)
