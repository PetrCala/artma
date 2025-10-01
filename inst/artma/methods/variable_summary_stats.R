#' @title Compute variable summary statistics
#' @description Compute summary statistics for selected variables in a data frame,
#' including mean, median, minimum, maximum, standard deviation, and percentage of missing observations.
#' If a variable contains missing or non-numeric data, the corresponding summary statistics will be omitted.
#' @param df *\[data.frame\]* The input data frame.
#' @param names_verbose *\[logical\]* If `TRUE`, print out the descriptive variable names. If `FALSE`,
#' print out the data frame column names. Defaults to `TRUE`.
#' @return *\[data.frame\]* A data frame of summary statistics.
variable_summary_stats <- function(df) {
  box::use(
    artma / const[CONST],
    artma / libs / validation[assert],
    artma / data_config / read[get_data_config],
    artma / options / index[get_option_group],
    artma / libs / utils[get_verbosity]
  )

  config <- get_data_config()
  opt <- get_option_group("artma.methods.variable_summary_stats")

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Computing variable summary statistics")
  }

  variable_stat_names <- CONST$VARIABLE_SUMMARY_STATS$NAMES
  desired_vars <- names(config)[vapply(config, function(x) isTRUE(x$variable_summary), logical(1))]

  if (length(desired_vars) == 0) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("No variables selected to compute summary statistics for.")
    }
    return(data.frame())
  }

  # Initialize output data frame
  df_out <- data.frame(matrix(nrow = length(desired_vars), ncol = length(variable_stat_names)))
  colnames(df_out) <- variable_stat_names

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

    round_to <- getOption("artma.output.number_of_decimals", 3)
    assert(round_to >= 0, "Number of decimals must be greater than or equal to 0.")

    var_mean <- round(base::mean(var_data, na.rm = TRUE), round_to)
    var_median <- round(stats::median(var_data, na.rm = TRUE), round_to)
    var_sd <- round(stats::sd(var_data, na.rm = TRUE), round_to)
    var_min <- round(base::min(var_data, na.rm = TRUE), round_to)
    var_max <- round(base::max(var_data, na.rm = TRUE), round_to)
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

  if (get_verbosity() >= 3) {
    cli::cli_h3("Variable summary statistics:")
    cli::cat_print(df_out)
  }

  if (length(missing_data_vars) > 0 && get_verbosity() >= 2) {
    cli::cli_alert_warning("Missing data for {.val {length(missing_data_vars)}} variables: {.val {missing_data_vars}}")
  }

  df_out
}

box::use(
  artma / libs / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

run <- cache_cli_runner(
  variable_summary_stats,
  stage = "variable_summary_stats",
  key_builder = function(...) build_data_cache_signature()
)

box::export(run)
