#' @title Compute variable summary statistics
#' @description Compute summary statistics for selected variables in a data frame,
#' including mean, median, minimum, maximum, standard deviation, and percentage of missing observations.
#' If a variable contains missing or non-numeric data, the corresponding summary statistics will be omitted.
#' @param df *\[data.frame\]* The input data frame.
#' @param names_verbose *\[logical\]* If `TRUE`, print out the descriptive variable names. If `FALSE`,
#' print out the data frame column names. Defaults to `TRUE`.
#' @return *\[list\]* A list containing a data frame of summary statistics and a vector of variables with missing data.
run <- function(df) {
  box::use(
    artma / const[CONST],
    artma / data_config / read[get_data_config],
    artma / options / index[get_option_group]
  )

  config <- get_data_config()
  opt <- get_option_group("artma.methods.variable_summary_stats")
  verbose <- getOption("artma.verbose", 3)

  variable_stat_names <- CONST$VARIABLE_SUMMARY_STATS$NAMES
  desired_vars <- names(config)[vapply(config, function(x) isTRUE(x$variable_summary), logical(1))]

  # Initialize output data frame
  df_out <- data.frame(matrix(nrow = length(desired_vars), ncol = length(variable_stat_names)))
  colnames(df_out) <- variable_stat_names

  if (length(desired_vars) == 0) {
    if (verbose >= 2) {
      cli::cli_alert_warning("No variables selected to compute summary statistics for.")
    }
    return(list(df_out, c()))
  }

  # Iterate over all desired variables and append summary statistics to the main DF
  missing_data_vars <- c()
  for (var_name in desired_vars) {
    var_data <- as.vector(unlist(subset(df, select = var_name))) # Roundabout way, because types
    var_class <- config[[var_name]]$data_type
    var_name_display <- if (opt$use_verbose_names) config[[var_name]]$var_name_verbose else var_name
    row_idx <- match(var_name, desired_vars) # Append data to this row

    # Missing all data
    if (!any(is.numeric(var_data), na.rm = TRUE) || all(is.na(var_data))) {
      missing_data_vars <- append(missing_data_vars, var_name)
      df_out[row_idx, ] <- c(var_name_display, var_class, rep(NA, length(variable_stat_names) - 2))
      next
    }

    var_mean <- round(base::mean(var_data, na.rm = TRUE), 3)
    var_median <- round(stats::median(var_data, na.rm = TRUE), 3)
    var_sd <- round(stats::sd(var_data, na.rm = TRUE), 3)
    var_min <- round(base::min(var_data, na.rm = TRUE), 3)
    var_max <- round(base::max(var_data, na.rm = TRUE), 3)
    var_obs <- sum(!is.na(var_data) & var_data != 0)
    var_missing <- round((sum(is.na(var_data)) / length(var_data)) * 100, 1)
    var_missing_verbose <- paste0(as.character(var_missing), "%")
    # Aggregate and append to the main DF
    row_data <- c(
      var_name_display,
      var_class,
      var_mean,
      var_median,
      var_min,
      var_max,
      var_sd,
      var_obs,
      var_missing_verbose
    )
    df_out[row_idx, ] <- row_data
  }
  if (verbose >= 3) {
    cli::cat_print(df_out)
  }
  list(df_out, missing_data_vars)
}

box::export(
  run
)
