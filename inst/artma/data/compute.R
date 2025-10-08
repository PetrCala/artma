#' @title Add observation ID column
#' @description Add an observation ID column to the data frame if missing.
#'   Creates a sequential integer ID from 1 to nrow.
#' @param df *\[data.frame\]* The data frame to add the observation ID column to.
#' @return *\[data.frame\]* The data frame with the observation ID column.
#' @keywords internal
add_obs_id_column <- function(df) {
  box::use(artma / libs / utils[get_verbosity])

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

#' @title Add study ID column
#' @description Add a study ID column to the data frame.
#' @param df *\[data.frame\]* The data frame to add the study ID column to.
#' @return *\[data.frame\]* The data frame with the study ID column.
#' @keywords internal
add_study_id_column <- function(df) {
  box::use(artma / libs / utils[get_verbosity])

  study_names <- df$study

  if (!length(study_names) == nrow(df)) {
    cli::cli_abort("The number of study names must be equal to the number of rows in the data frame.")
  }

  valid_ids <- as.integer(factor(study_names, levels = unique(study_names)))

  if ("study_id" %in% colnames(df)) {
    invalid_or_missing_ids <- which(is.na(df$study_id) | df$study_id != valid_ids)
    if (length(invalid_or_missing_ids) > 0) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(c(
          "!" = "Found {length(invalid_or_missing_ids)} invalid or missing study IDs in the column {.val study_id}.",
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
    artma / const[CONST],
    calc = artma / calc / index
  )
  opt_path <- "artma.data.colnames.t_stat"

  if ("t_stat" %in% colnames(df)) {
    if (any(is.na(df$t_stat))) {
      n_missing <- sum(is.na(df$t_stat))
      opt_name <- CONST$STYLES$OPTIONS$NAME(opt_path)
      opt_val <- CONST$STYLES$OPTIONS$VALUE("NA")
      cli::cli_abort(c(
        "!" = "Found {n_missing} missing t-statistics in the column {.val t_stat}.",
        "i" = "Please add these to your data frame or set the option {opt_name} to {opt_val} to compute them automatically.",
        "i" = "You can set the option by running {.code artma::options.modify(user_input = list('{opt_path}' = NA))}."
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
  box::use(artma / libs / utils[get_verbosity])

  study_id_col <- df$study_id

  freq_table <- table(study_id_col)
  study_size_col <- vapply(study_id_col, function(x) freq_table[as.character(x)], FUN.VALUE = integer(1))

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
  box::use(artma / libs / utils[get_verbosity])

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
    artma / libs / utils[get_verbosity]
  )

  if ("precision" %in% colnames(df)) {
    # Validate existing precision column
    if (any(is.na(df$precision))) {
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

    if (!is.numeric(df$precision)) {
      cli::cli_abort("The column {.val precision} must be numeric.")
    }
  } else {
    # Calculate precision based on option
    precision_type <- getOption("artma.calc.precision_type", default = "1/SE")
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
    artma / data / utils[determine_vector_type],
    artma / const[CONST],
    artma / libs / string[make_verbose_name],
    artma / libs / utils[get_verbosity]
  )

  # Get current config
  config <- get_data_config()

  # List of computed columns to add
  computed_columns <- c("obs_id", "study_id", "t_stat", "study_size", "reg_dof", "precision")

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
          variable_summary = TRUE,  # Include in variable summaries
          effect_sum_stats = FALSE,  # Don't split by computed columns
          is_computed = TRUE  # Mark as computed for clarity
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
#' @description Compute optional columns that the user did not provide,
#'   then update the data config to include them.
#'   This includes:
#'   - obs_id: Sequential observation IDs (1 to nrow)
#'   - study_id: Integer study IDs based on study names
#'   - t_stat: T-statistics calculated from effect/se
#'   - study_size: Number of estimates per study
#'   - reg_dof: Regression degrees of freedom (n_obs - 2)
#'   - precision: Precision calculated as 1/SE or sqrt(DoF)
#' @param df *\[data.frame\]* The data frame to compute the optional columns for.
#' @return *\[data.frame\]* The data frame with all optional columns computed.
compute_optional_columns <- function(df) {
  box::use(artma / libs / utils[get_verbosity])
  box::use(magrittr[`%>%`])

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Computing optional columns for {.val {nrow(df)}} observations…")
  }

  df_with_computed <- df %>%
    add_obs_id_column() %>%
    add_study_id_column() %>%
    add_t_stat_column() %>%
    add_study_size_column() %>%
    add_reg_dof_column() %>%
    add_precision_column()

  # Update the data config to include computed columns
  update_config_with_computed_columns(df_with_computed)

  df_with_computed
}

box::export(compute_optional_columns)
