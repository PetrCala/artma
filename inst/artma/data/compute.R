#' @title Add observation ID column
#' @description Add an observation ID column to the data frame.
#' @param df *\[data.frame\]* The data frame to add the observation ID column to.
#' @return *\[data.frame\]* The data frame with the observation ID column.
#' @keywords internal
add_obs_id_column <- function(df) {
  if (!"obs_id" %in% colnames(df)) {
    df$obs_id <- seq_len(nrow(df))
  } else {
    invalid_idxs <- which(df$obs_id != seq_len(nrow(df))) # For validation
    n_invalid_idxs <- length(invalid_idxs)

    if (n_invalid_idxs > 0) {
      cli::cli_alert_warning(c(
        "!" = "Found {n_invalid_idxs} invalid observation IDs in the column {.val obs_id}.",
        "i" = "Resetting them to sequential integers."
      ))
      df$obs_id[invalid_idxs] <- seq_len(nrow(df))[invalid_idxs]
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
  study_names <- df$study

  if (!length(study_names) == nrow(df)) {
    cli::cli_abort("The number of study names must be equal to the number of rows in the data frame.")
  }

  valid_ids <- as.integer(factor(study_names, levels = unique(study_names)))

  if ("study_id" %in% colnames(df)) {
    invalid_or_missing_ids <- which(is.na(df$study_id) | df$study_id != valid_ids)
    if (length(invalid_or_missing_ids) > 0) {
      cli::cli_alert_warning(c(
        "!" = "Found {length(invalid_or_missing_ids)} invalid or missing study IDs in the column {.val {colname}}.",
        "i" = "Resetting them to sequential integers."
      ))
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
      # nolint start: unused_declared_object_linter.
      n_missing <- sum(is.na(df$t_stat))
      opt_name <- CONST$STYLES$OPTIONS$NAME(opt_path)
      opt_val <- CONST$STYLES$OPTIONS$VALUE("NA")
      # nolint end: unused_declared_object_linter.
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
  study_id_col <- df$study_id

  freq_table <- table(study_id_col)
  study_size_col <- vapply(study_id_col, function(x) freq_table[as.character(x)], FUN.VALUE = integer(1))

  if ("study_size" %in% colnames(df)) {
    invalid_or_missing_ids <- which(is.na(df$study_size) | df$study_size != study_size_col)
    if (length(invalid_or_missing_ids) > 0) {
      cli::cli_alert_warning(c(
        "!" = "Found {length(invalid_or_missing_ids)} invalid or missing study sizes in the column {.val study_size}.",
        "i" = "Resetting them to the expected values."
      ))
    }
  }

  df$study_size <- study_size_col

  df
}

# add_reg_dof_column <- function(df) {}

add_precision_column <- function(df) {
  box::use(calc = artma / calc / index)

  if ("precision" %in% colnames(df)) {
    if (any(is.na(df$precision))) {
      cli::cli_abort("The column {.val precision} must not contain missing values.")
    }
    if (!is.numeric(df$precision)) {
      cli::cli_abort("The column {.val precision} must be numeric.")
    }
  } else {
    df$precision <- calc$precision(se = df$se, reg_dof = df$reg_dof)
  }

  df
}

#' @title Compute optional columns
#' @description Compute optional columns that the user did not provide.
#' @param df *\[data.frame\]* The data frame to compute the optional columns for.
#' @return *\[data.frame\]* The data frame with the optional columns.
compute_optional_columns <- function(df) {
  box::use(magrittr[`%>%`])

  cli::cli_inform("Computing and validating optional columns…")

  df %>%
    add_obs_id_column() %>%
    add_study_id_column() %>%
    add_t_stat_column() %>%
    add_study_size_column() %>%
    add_precision_column()
}

box::export(compute_optional_columns)
