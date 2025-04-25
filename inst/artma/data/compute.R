#' @title Get optional column name
#' @description Get the user-defined name of an optional column.
#' @param colname *\[character\]* The standardized name of the column to get.
#' @return *\[character\]* The user-defined name of the column.
#' @keywords internal
get_optional_colname <- function(colname) {
  usr_colname <- getOption(paste0("artma.data.colnames.", colname))

  if (is.null(usr_colname)) {
    cli::cli_abort("The column name for the {.val {colname}} is not set.")
  }

  if (is.na(usr_colname)) usr_colname <- colname # Default to the standardized name

  usr_colname
}

#' @title Add observation ID column
#' @description Add an observation ID column to the data frame.
#' @param df *\[data.frame\]* The data frame to add the observation ID column to.
#' @return *\[data.frame\]* The data frame with the observation ID column.
#' @keywords internal
add_obs_id_column <- function(df) {
  colname <- get_optional_colname("obs_id")

  if (!colname %in% colnames(df)) {
    df[[colname]] <- seq_len(nrow(df))
  } else {
    invalid_idxs <- which(df[[colname]] != seq_len(nrow(df))) # For validation
    n_invalid_idxs <- length(invalid_idxs)

    if (n_invalid_idxs > 0) {
      cli::cli_alert_warning("Found {n_invalid_idxs} invalid observation IDs. Resetting them to sequential integers.")
      df[[colname]][invalid_idxs] <- seq_len(nrow(df))[invalid_idxs]
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
  box::use(artma / libs / validation[assert])

  colname <- get_optional_colname("study_id")

  study_colname <- getOption("artma.data.colnames.study")
  study_names <- df[[study_colname]]

  if (!length(study_names) == nrow(df)) {
    cli::cli_abort("The number of study names must be equal to the number of rows in the data frame.")
  }

  valid_ids <- as.integer(factor(study_names, levels = unique(study_names)))

  if (colname %in% colnames(df)) {
    invalid_or_missing_ids <- which(is.na(df[[colname]]) | df[[colname]] != valid_ids)
    if (length(invalid_or_missing_ids) > 0) {
      cli::cli_alert_warning("Found {length(invalid_or_missing_ids)} invalid or missing study IDs. Resetting them to sequential integers.")
    }
  }

  df[[colname]] <- valid_ids

  df
}

#' @title Compute optional columns
#' @description Compute optional columns that the user did not provide.
#' @param df *\[data.frame\]* The data frame to compute the optional columns for.
#' @return *\[data.frame\]* The data frame with the optional columns.
compute_optional_columns <- function(df) {
  box::use(magrittr[`%>%`])

  cli::cli_inform("Computing and validating optional columns...")

  df %>%
    add_obs_id_column() %>%
    add_study_id_column()
}

box::export(compute_optional_columns)
