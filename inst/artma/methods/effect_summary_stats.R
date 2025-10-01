#' @title Effect Summary Stats
#' @description
#' The function getEffectSummaryStats() calculates the summary statistics for variables in a given data frame
#'    using the percentage of correct classification (Effect) effect and sample size study_size columns,
#'    and returns a data frame with the results. The function takes as input input_var_list,
#'    a data frame that contains metadata about the variables in  and which variables to calculate
#'    summary statistics for. The summary statistics calculated are the mean, median, weighted mean,
#'    minimum, maximum, standard deviation, and number of observations. For the weighted mean,
#'    the inverse squared sample size is used as weights. The confidence level for the weighted mean
#'    confidence interval can be set using the conf.level parameter, which defaults to 0.95.
#'    If any input data is missing or non-numeric, it is ignored, and the variable is not included in the output.
#'
#' @param df The data frame to summarize
#' @return A list of the summary statistics
#' @param  [data.frame] Main data frame.
#' @param input_var_list [data.frame] Data frame with variable information.
#' @param conf.level [numeric] Confidence level for the confidence intervals. Defaults to 0.95 (95%).
#' @param formal_output [logical] If TRUE, return the table in a form that can be used in LaTeX. Defaults to FALSE.
#'
#' The function returns a list containing a data frame containing the following columns:
#' -Var Name: The name of the variable.
#' -Var Class: The data type of the variable.
#' -Mean: The arithmetic mean of the effect for the variable.
#' -Median: The median of the effect for the variable.
#' -Weighted Mean: The weighted mean of the effect for the variable, using the inverse squared sample size as weights.
#' -WM CI lower: The lower bound of the confidence interval for the weighted mean.
#' -WM CI upper: The upper bound of the confidence interval for the weighted mean.
#' -Min: The minimum effect value for the variable.
#' -Max: The maximum effect value for the variable.
#' -SD: The standard deviation of the effect for the variable.
#' -Obs: The number of observations for the variable.
#' If a variable has missing or non-numeric data, it will not be included in the output.
#' If no variables are included in the output, the function returns an empty data frame.
effect_summary_stats <- function(df) {
  box::use(
    artma / const[CONST],
    artma / data_config / read[get_data_config],
    artma / libs / utils[get_verbosity],
    artma / libs / validation[assert],
    artma / options / index[get_option_group]
  )

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Summarizing the main effect")
  }

  config <- get_data_config()
  opt <- get_option_group("artma.methods.variable_summary_stats")
  round_to <- getOption("artma.output.number_of_decimals", 3)

  conf_level <- opt$conf_level
  formal_output <- opt$formal_output

  assert(
    conf_level >= 0 && conf_level <= 1,
    "Confidence level must be between 0 and 1. Got {.val {conf_level}}"
  )
  assert(
    is.logical(formal_output),
    "Formal output must be a logical value. Got {.val {formal_output}}"
  )
  assert(round_to >= 0, "Number of decimals must be greater than or equal to 0.")

  z <- qnorm((1 - conf_level) / 2, lower.tail = FALSE) # Z value for conf. int. calculation
  effect_data <- with(df, as.vector(effect))
  study_size_data <- with(df, as.vector(study_size))

  effect_stat_names <- CONST$EFFECT_SUMMARY_STATS$NAMES
  desired_vars <- names(config)[vapply(config, function(x) isTRUE(x$effect_summary_stats), logical(1))]

  if (length(desired_vars) == 0) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("No variables selected to compute summary statistics for.")
    }
    return(data.frame())
  }


  df <- data.frame(
    col1 = character(),
    col2 = character(),
    col3 = numeric(),
    col4 = numeric(),
    col5 = numeric(),
    col6 = numeric(),
    col7 = numeric(),
    col8 = numeric(),
    col9 = numeric(),
    col10 = numeric(),
    col11 = numeric(),
    col12 = numeric(),
    col13 = numeric(),
    stringsAsFactors = FALSE
  )
  stopifnot(ncol(df) == length(effect_stat_names))

  # Iterate over all desired variables and append summary statistics to the main DF
  missing_data_vars <- c()
  for (var_name in desired_vars) {
    # Get data for this var
    var_data <- as.vector(unlist(subset(, select = var_name))) # Roundabout way, because types
    var_specs <- input_var_list[input_var_list$var_name == var_name, ] # Specifications for this variable
    var_class <- var_specs$data_type
    var_name_verbose <- var_specs$var_name_verbose
    # row_idx <- match(var_name, desired_vars) # Append data to this row

    # Missing all data
    if (any(
      !any(is.numeric(var_data), na.rm = TRUE), # No numerics
      all(is.na(var_data)), # All NAs
      nrow(var_data) == 0, # Empty data
      all(var_data %in% c(0, NA)) # Only 0s or NAs
    )) {
      if (get_verbosity() >= 4) {
        cli::cli_alert_info("Missing data for {.val {var_name}}")
      }
      missing_data_vars <- append(missing_data_vars, var_name)
      next
    }

    # Get the specifications and subset the data accordingly
    equal_val <- var_specs$equal
    gtlt_val <- var_specs$gtlt
    stopifnot(xor(is.na(equal_val), is.na(gtlt_val))) # Additional validity check - should never occur
    # The specification is EQUAL
    if (!is.na(equal_val)) {
      effect_data_equal <- effect_data[var_data == equal_val]
      study_size_data_equal <- study_size_data[var_data == equal_val] # For W. mean - wonky, but straightforward
      cutoff <- equal_val # For verbose output
    } else { # The specification is gtlt
      if (gtlt_val %in% c("mean", "median")) {
        cutoff <- if (gtlt_val == "mean") base::mean(var_data, na.rm = TRUE) else stats::median(var_data, na.rm = TRUE)
        effect_data_gt <- effect_data[var_data >= cutoff]
        effect_data_lt <- effect_data[var_data < cutoff]
        study_size_data_gt <- study_size_data[var_data >= cutoff]
        study_size_data_lt <- study_size_data[var_data < cutoff]
      } else if (!is.na(gtlt_val)) {
        cutoff <- gtlt_val # For verbose output
        effect_data_gt <- effect_data[var_data >= gtlt_val]
        effect_data_lt <- effect_data[var_data < gtlt_val]
        study_size_data_gt <- study_size_data[var_data >= gtlt_val]
        study_size_data_lt <- study_size_data[var_data < gtlt_val]
      } else {
        cli::cli_abort("Value error")
      }
    }

    # A function for statistics calculation
    get_new_data_row <- function(input_var_name, input_class_name, input_effect_data, input_study_size_data) {
      input_effect_data <- stats::na.omit(input_effect_data)
      input_study_size_data <- stats::na.omit(input_study_size_data)
      # Summary stats computation
      var_mean <- round(base::mean(input_effect_data), round_to)
      var_sd <- round(stats::sd(input_effect_data), round_to)
      var_ci_lower <- round(var_mean - var_sd * z, round_to)
      var_ci_upper <- round(var_mean + var_sd * z, round_to)
      var_weighted_mean <- round(stats::weighted.mean(input_effect_data, w = 1 / input_study_size_data), round_to)
      var_ci_lower_w <- round(var_weighted_mean - var_sd * z, round_to)
      var_ci_upper_w <- round(var_weighted_mean + var_sd * z, round_to)
      var_median <- round(stats::median(input_effect_data), round_to)
      var_min <- round(base::min(input_effect_data), 3)
      var_max <- round(base::max(input_effect_data), 3)
      var_obs <- length(input_effect_data)

      new_row <- data.frame(
        col1 = input_var_name,
        col2 = input_class_name,
        col3 = var_mean,
        col4 = var_ci_lower,
        col5 = var_ci_upper,
        col6 = var_weighted_mean,
        col7 = var_ci_lower_w,
        col8 = var_ci_upper_w,
        col9 = var_median,
        col10 = var_min,
        col11 = var_max,
        col12 = var_sd,
        col13 = var_obs
      )
      return(new_row)
    }
    # EQUAL data
    if (!is.na(equal_val)) {
      equal_cutoff <- if (cutoff == 1) "" else paste0(" = ", round(cutoff, 3)) # None if equal to 1
      new_varname_equal <- paste0(var_name_verbose, equal_cutoff)
      new_row <- get_new_data_row(new_varname_equal, var_class, effect_data_equal, study_size_data_equal)
      df <- rbind(df, new_row)
    } else { # GTLT data
      new_varname_gt <- paste0(var_name_verbose, " >= ", round(as.numeric(cutoff), 3))
      new_varname_lt <- paste0(var_name_verbose, " < ", round(as.numeric(cutoff), 3))
      new_row_gt <- get_new_data_row(new_varname_gt, var_class, effect_data_gt, study_size_data_gt)
      new_row_lt <- get_new_data_row(new_varname_lt, var_class, effect_data_lt, study_size_data_lt)
      df <- rbind(df, new_row_gt)
      df <- rbind(df, new_row_lt)
    }
  }
  # Add a row on top of the data frame with all observations
  first_row <- get_new_data_row("All Data", "any", effect_data, study_size_data)
  df <- rbind(first_row, df)
  # Put the final output together
  colnames(df) <- effect_stat_names
  # Format into a more presentable form
  if (formal_output) {
    cols_to_drop <- c("Var Class", "Median", "Min", "Max", "SD")
    df <- df[, !names(df) %in% cols_to_drop]
  }

  if (get_verbosity() >= 3) {
    cli::cli_h3("Summary statistics:")
    cli::cat_print(df)
  }

  if (length(missing_data_vars) > 0 && get_verbosity() >= 2) {
    cli::cli_alert_warning("Missing data for {.val {length(missing_data_vars)}} variables: {.val {missing_data_vars}}")
  }

  return(df)
}

box::use(
  artma / libs / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

run <- cache_cli_runner(
  effect_summary_stats,
  stage = "effect_summary_stats",
  key_builder = function(...) build_data_cache_signature()
)

box::export(run)
