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
    artma / data_config / read[get_data_config],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / resolver[opt_spec, resolve_options],
    artma / libs / core / utils[get_verbosity],
    artma / libs / formatting / results[print_summary_table]
  )

  config <- get_data_config()
  opt <- get_option_group("artma.methods.variable_summary_stats")

  resolved <- resolve_options(opt, list(
    use_verbose_names = opt_spec(default = TRUE, type = "logical"),
    round_to = opt_spec(
      default = 3L, type = "numeric", key = "artma.output.number_of_decimals",
      constraint = function(x) x >= 0,
      constraint_msg = "Number of decimals must be greater than or equal to 0."
    )
  ))
  use_verbose_names <- resolved$use_verbose_names
  round_to <- resolved$round_to

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Computing variable summary statistics")
  }

  variable_stat_names <- CONST$VARIABLE_SUMMARY_STATS$NAMES
  desired_vars <- names(config)[vapply(config, function(x) isTRUE(x$variable_summary), logical(1))]

  if (length(desired_vars) == 0) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("No variables selected to compute summary statistics for.")
    }
    return(new_method_result(tables = list(summary = data.frame())))
  }

  # Initialize output data frame
  df_out <- data.frame(matrix(nrow = length(desired_vars), ncol = length(variable_stat_names)))
  colnames(df_out) <- variable_stat_names

  # Iterate over all desired variables and append summary statistics to the main DF
  missing_data_vars <- c()
  raw_stats <- list()
  for (row_idx in seq_along(desired_vars)) {
    var_name <- desired_vars[[row_idx]]
    var_data <- as.vector(unlist(subset(df, select = var_name))) # Roundabout way, because types
    var_class <- config[[var_name]]$data_type
    var_name_display <- if (isTRUE(use_verbose_names)) config[[var_name]]$var_name_verbose else var_name

    # Missing all data
    if (!any(is.numeric(var_data), na.rm = TRUE) || all(is.na(var_data))) {
      missing_data_vars <- append(missing_data_vars, var_name)
      df_out[row_idx, ] <- c(var_name_display, var_class, rep(NA, length(variable_stat_names) - 2))
      next
    }

    # The unrounded statistics feed the estimates slot; the display row below
    # keeps the rounded, percent-formatted variants.
    stats_raw <- list(
      mean = base::mean(var_data, na.rm = TRUE),
      median = stats::median(var_data, na.rm = TRUE),
      min = base::min(var_data, na.rm = TRUE),
      max = base::max(var_data, na.rm = TRUE),
      sd = stats::sd(var_data, na.rm = TRUE),
      missing_share = sum(is.na(var_data)) / length(var_data),
      obs = sum(!is.na(var_data))
    )
    raw_stats[[length(raw_stats) + 1]] <- c(list(label = var_name_display), stats_raw)

    var_mean <- round(stats_raw$mean, round_to)
    var_median <- round(stats_raw$median, round_to)
    var_sd <- round(stats_raw$sd, round_to)
    var_min <- round(stats_raw$min, round_to)
    var_max <- round(stats_raw$max, round_to)
    var_obs <- stats_raw$obs
    var_missing <- round(stats_raw$missing_share * 100, 1)
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
    print_summary_table(df_out)
  }

  if (length(missing_data_vars) > 0 && get_verbosity() >= 2) {
    cli::cli_alert_warning("Missing data for {.val {length(missing_data_vars)}} variables: {.val {missing_data_vars}}")
  }

  invisible(new_method_result(
    tables = list(summary = df_out),
    estimates = variable_summary_stats_estimates(raw_stats)
  ))
}

#' @title Tidy estimates for the variable summary statistics
#' @description
#' Flatten the summary table into the shared `estimates` schema. The method is
#' descriptive rather than inferential, so the mapping is a judgement call:
#' each row of the display table is one variable and each of its columns one
#' statistic, so in the long schema `model` is the variable and `term` names
#' the statistic (`"mean"`, `"median"`, `"min"`, `"max"`, `"sd"`,
#' `"missing_share"`). `estimate` holds the statistic itself; there is nothing
#' inferential to fill.
#'
#' `missing_share` is a proportion in `[0, 1]`, not the `"12.5%"` string the
#' display table prints: the estimates frame is machine-readable and must parse
#' as a number.
#'
#' Variables whose data are entirely missing or non-numeric produce no rows,
#' matching the all-`NA` row they get in the display table.
#' @param raw_stats *\[list\]* Per-variable unrounded statistics, each a list
#'   with `label` and the statistic elements.
#' @return *\[data.frame\]* A frame in the shared estimates schema.
variable_summary_stats_estimates <- function(raw_stats) {
  box::use(
    artma / modules / runtime_methods[new_estimates]
  )

  if (!is.list(raw_stats) || length(raw_stats) == 0L) {
    return(new_estimates())
  }

  terms <- c("mean", "median", "min", "max", "sd", "missing_share")

  rows <- lapply(raw_stats, function(entry) {
    data.frame(
      method = "variable_summary_stats",
      model = entry$label,
      term = terms,
      estimate = as.numeric(unlist(entry[terms])),
      n_obs = as.integer(entry$obs),
      stringsAsFactors = FALSE
    )
  })

  new_estimates(do.call(rbind, rows))
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  variable_summary_stats,
  stage = "variable_summary_stats",
  description = "Descriptive statistics for each variable flagged for summary"
)

box::export(variable_summary_stats, variable_summary_stats_estimates, run)
