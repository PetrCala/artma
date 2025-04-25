box::use(artma / data / utils[get_usr_colname])

#' @title Add observation ID column
#' @description Add an observation ID column to the data frame.
#' @param df *\[data.frame\]* The data frame to add the observation ID column to.
#' @return *\[data.frame\]* The data frame with the observation ID column.
#' @keywords internal
add_obs_id_column <- function(df) {
  colname <- get_usr_colname("obs_id")

  if (!colname %in% colnames(df)) {
    df[[colname]] <- seq_len(nrow(df))
  } else {
    invalid_idxs <- which(df[[colname]] != seq_len(nrow(df))) # For validation
    n_invalid_idxs <- length(invalid_idxs)

    if (n_invalid_idxs > 0) {
      cli::cli_alert_warning(c(
        "!" = "Found {n_invalid_idxs} invalid observation IDs in the column {.val {colname}}.",
        "i" = "Resetting them to sequential integers."
      ))
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

  colname <- get_usr_colname("study_id")

  study_colname <- getOption("artma.data.colnames.study")
  study_names <- df[[study_colname]]

  if (!length(study_names) == nrow(df)) {
    cli::cli_abort("The number of study names must be equal to the number of rows in the data frame.")
  }

  valid_ids <- as.integer(factor(study_names, levels = unique(study_names)))

  if (colname %in% colnames(df)) {
    invalid_or_missing_ids <- which(is.na(df[[colname]]) | df[[colname]] != valid_ids)
    if (length(invalid_or_missing_ids) > 0) {
      cli::cli_alert_warning(c(
        "!" = "Found {length(invalid_or_missing_ids)} invalid or missing study IDs in the column {.val {colname}}.",
        "i" = "Resetting them to sequential integers."
      ))
    }
  }

  df[[colname]] <- valid_ids

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
  colname <- get_usr_colname("t_stat")
  opt_path <- "artma.data.colnames.t_stat"

  if (colname %in% colnames(df)) {
    if (any(is.na(df[[colname]]))) {
      n_missing <- sum(is.na(df[[colname]]))
      opt_name <- CONST$STYLES$OPTIONS$NAME(opt_path)
      opt_val <- CONST$STYLES$OPTIONS$VALUE("NA")
      cli::cli_abort(c(
        "!" = "Found {n_missing} missing t-statistics in the column {.val {colname}}.",
        "i" = "Please add these to your data frame or set the option {opt_name} to {opt_val} to compute them automatically.",
        "i" = "You can set the option by running {.code artma::options.modify(user_input = list('{opt_path}' = NA))}."
      ))
    }
  }

  df[[colname]] <- calc$t_stat(
    effect = df[[getOption("artma.data.colnames.effect")]],
    se = df[[getOption("artma.data.colnames.se")]]
  )

  df
}

#' @title Add study size column
#' @description Add a study size column to the data frame.
#' @param df *\[data.frame\]* The data frame to add the study size column to.
#' @return *\[data.frame\]* The data frame with the study size column.
#' @keywords internal
add_study_size_column <- function(df) {
  colname <- get_usr_colname("study_size")
  study_id_colname <- get_usr_colname("study_id")

  study_id_col <- df[[study_id_colname]]

  freq_table <- table(study_id_col)
  study_size_col <- vapply(study_id_col, function(x) freq_table[as.character(x)], FUN.VALUE = integer(1))

  if (colname %in% colnames(df)) {
    invalid_or_missing_ids <- which(is.na(df[[colname]]) | df[[colname]] != study_size_col)
    if (length(invalid_or_missing_ids) > 0) {
      cli::cli_alert_warning(c(
        "!" = "Found {length(invalid_or_missing_ids)} invalid or missing study sizes in the column {.val {colname}}.",
        "i" = "Resetting them to the expected values."
      ))
    }
  }

  df[[colname]] <- study_size_col

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
    add_study_id_column() %>%
    add_t_stat_column() %>%
    add_study_size_column()
}

box::export(compute_optional_columns)
