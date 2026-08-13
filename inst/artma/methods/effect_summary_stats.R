#' @title Effect summary statistics
#' @description
#' Compute summary statistics for the main effect grouped by variables that are
#' flagged in the data configuration. The function supports equality based
#' splits as well as threshold splits (numeric, mean or median based). It
#' returns the arithmetic mean, weighted mean (weighted by the inverse of the
#' number of estimates per study, so every study contributes equally),
#' confidence intervals, and additional distribution statistics.
effect_summary_stats <- function(df) {
  box::use(
    artma / const[CONST],
    artma / data_config / read[get_data_config],
    artma / libs / core / grouping[
      is_group_value_na,
      resolve_group_threshold,
      resolve_variable_groups
    ],
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[validate, validate_columns],
    artma / libs / formatting / results[print_summary_table],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / resolver[opt_spec, resolve_options]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "study_size"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Summarizing the main effect")
  }

  config <- get_data_config()
  opt <- get_option_group("artma.methods.effect_summary_stats")

  resolved <- resolve_options(opt, list(
    conf_level = opt_spec(
      default = 0.95, type = "numeric", scalar = TRUE,
      constraint = function(x) x >= 0 && x <= 1,
      constraint_msg = "Confidence level must be between 0 and 1."
    ),
    formal_output = opt_spec(default = FALSE, type = "logical", scalar = TRUE),
    round_to = opt_spec(
      default = 3L, type = "numeric", key = "artma.output.number_of_decimals",
      constraint = function(x) x >= 0,
      constraint_msg = "Number of decimals must be greater than or equal to 0."
    )
  ))

  conf_level <- resolved$conf_level
  formal_output <- resolved$formal_output
  round_to <- resolved$round_to

  z_value <- stats::qnorm((1 - conf_level) / 2, lower.tail = FALSE)

  effect_values <- df$effect
  study_sizes <- df$study_size

  # Helper -----------------------------------------------------------------

  format_numeric <- function(x) if (is.finite(x)) round(x, round_to) else NA_real_

  compute_unweighted_stats <- function(values) {
    values <- values[is.finite(values)]
    if (!length(values)) {
      return(list(
        mean = NA_real_, sd = NA_real_, ci = c(NA_real_, NA_real_), median = NA_real_,
        min = NA_real_, max = NA_real_, obs = 0L
      ))
    }

    mean_val <- mean(values)
    sd_val <- stats::sd(values)
    se_val <- if (!is.na(sd_val) && length(values) > 1) sd_val / sqrt(length(values)) else NA_real_
    ci <- if (!is.na(se_val)) c(mean_val - z_value * se_val, mean_val + z_value * se_val) else c(NA_real_, NA_real_)

    list(
      mean = mean_val,
      sd = sd_val,
      ci = ci,
      median = stats::median(values),
      min = min(values),
      max = max(values),
      obs = length(values)
    )
  }

  compute_weighted_stats <- function(values, weights) {
    mask <- is.finite(values) & is.finite(weights) & weights > 0
    values <- values[mask]
    weights <- weights[mask]

    if (!length(values)) {
      return(list(mean = NA_real_, ci = c(NA_real_, NA_real_)))
    }

    weights_sum <- sum(weights)
    if (!is.finite(weights_sum) || weights_sum <= 0) {
      return(list(mean = NA_real_, ci = c(NA_real_, NA_real_)))
    }
    norm_weights <- weights / weights_sum
    mean_val <- stats::weighted.mean(values, w = weights)
    # Var(sum(w_i * x_i)) = sum(w_i^2 * Var(x_i)); estimate each Var(x_i) by the
    # squared deviation, so the SE reflects the effective sample size implied by
    # unequal weights rather than the raw observation count.
    variance <- sum(norm_weights^2 * (values - mean_val)^2)
    se_val <- if (length(values) > 1) sqrt(variance) else NA_real_
    ci <- if (!is.na(se_val)) c(mean_val - z_value * se_val, mean_val + z_value * se_val) else c(NA_real_, NA_real_)

    list(mean = mean_val, ci = ci)
  }

  prepare_subset <- function(mask) {
    mask <- mask & is.finite(effect_values) & is.finite(study_sizes)
    list(
      effect = effect_values[mask],
      study_size = study_sizes[mask]
    )
  }

  # Determine which variables to analyse ------------------------------------

  is_effect_var <- function(var_cfg) {
    if (!is.list(var_cfg)) {
      return(FALSE)
    }

    flag <- var_cfg$effect_sum_stats
    legacy_flag <- var_cfg$effect_summary_stats
    equal <- var_cfg$equal
    gltl <- var_cfg$gltl %||% var_cfg$gtlt

    any(c(isTRUE(flag), isTRUE(legacy_flag), !is.na(equal), !is.na(gltl)))
  }

  effect_vars <- names(config)[vapply(config, is_effect_var, logical(1))]

  # If no variables configured, prompt for interactive selection
  if (!length(effect_vars)) {
    # Check if interactive mode is available
    if (interactive()) {
      if (get_verbosity() >= 3) {
        cli::cli_alert_info("No variables configured for effect summary statistics.")
      }

      box::use(
        artma / interactive / effect_summary_stats[
          prompt_effect_summary_var_selection
        ],
        artma / data_config / write[update_data_config]
      )

      # Prompt for variable selection
      updated_config <- prompt_effect_summary_var_selection(df, config)

      # Update the config in options
      update_data_config(updated_config)
      config <- updated_config

      # Re-check for effect vars after update
      effect_vars <- names(config)[vapply(config, is_effect_var, logical(1))]
    }

    # If still no variables (user declined or non-interactive), fall through:
    # the table still gets the pooled "All Data" row computed below, so say
    # that rather than reporting an absence the output then contradicts.
    if (!length(effect_vars) && get_verbosity() >= 3) {
      cli::cli_alert_info(
        "No moderator variables configured; reporting pooled statistics only."
      )
    }
  }

  rows_env <- environment()

  add_row <- function(label, class_name, subset_data) {
    if (!length(subset_data$effect)) {
      return(FALSE)
    }

    # study_size is the number of estimates the study reports; weighting each
    # estimate by its inverse gives every study equal total weight.
    weights <- 1 / subset_data$study_size
    unweighted <- compute_unweighted_stats(subset_data$effect)
    weighted <- compute_weighted_stats(subset_data$effect, weights)

    new_row <- data.frame(
      `Var Name` = label,
      `Var Class` = class_name,
      Mean = format_numeric(unweighted$mean),
      `CI lower` = format_numeric(unweighted$ci[1]),
      `CI upper` = format_numeric(unweighted$ci[2]),
      `Weighted Mean` = format_numeric(weighted$mean),
      `WM CI lower` = format_numeric(weighted$ci[1]),
      `WM CI upper` = format_numeric(weighted$ci[2]),
      Median = format_numeric(unweighted$median),
      Min = format_numeric(unweighted$min),
      Max = format_numeric(unweighted$max),
      SD = format_numeric(unweighted$sd),
      Obs = as.integer(unweighted$obs),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    assign("rows", c(rows_env$rows, list(new_row)), envir = rows_env)
    # The display row is rounded; keep the raw statistics for the estimates slot.
    assign(
      "raw_rows",
      c(rows_env$raw_rows, stats::setNames(
        list(list(label = label, unweighted = unweighted, weighted = weighted)),
        label
      )),
      envir = rows_env
    )

    TRUE
  }

  rows <- list()
  raw_rows <- list()
  missing_vars <- character()

  for (var_name in effect_vars) {
    var_cfg <- config[[var_name]]
    if (is.null(var_cfg) || !var_name %in% names(df)) {
      missing_vars <- c(missing_vars, var_name)
      next
    }

    var_data <- df[[var_name]]
    if (!is.numeric(var_data) && !is.integer(var_data)) {
      missing_vars <- c(missing_vars, var_name)
      next
    }

    var_label <- var_cfg$var_name_verbose %||% var_name
    var_class <- var_cfg$data_type %||% class(var_data)[1]

    equal_val <- var_cfg$equal
    gltl_val <- var_cfg$gltl %||% var_cfg$gtlt
    valid_mask <- !is.na(var_data) & is.finite(effect_values) & is.finite(study_sizes)
    filtered_var_data <- var_data[valid_mask]

    # The threshold basis (filtered_var_data) intentionally differs from the
    # grouping basis (var_data): mean/median thresholds are computed only over
    # rows whose effect and study size are finite.
    groups <- resolve_variable_groups(
      var_label = var_label,
      equal_val = equal_val,
      gltl_val = gltl_val,
      var_values = var_data,
      round_to = round_to,
      threshold_values = filtered_var_data
    )

    added_any <- FALSE
    for (group in groups) {
      subset <- prepare_subset(seq_along(var_data) %in% group$row_idx)
      added_any <- add_row(group$label, var_class, subset) || added_any
    }

    # A gltl split whose threshold cannot resolve to a number marks the
    # variable as missing, as it did before the split logic was extracted.
    if (!is_group_value_na(gltl_val) &&
      is.na(resolve_group_threshold(gltl_val, filtered_var_data))) {
      missing_vars <- c(missing_vars, var_name)
    }

    if (!added_any) {
      subset <- prepare_subset(!is.na(var_data))
      if (!add_row(var_label, var_class, subset)) {
        missing_vars <- c(missing_vars, var_name)
      }
    }
  }

  total_subset <- prepare_subset(rep(TRUE, length(effect_values)))
  add_row("All Data", "any", total_subset)

  out <- do.call(rbind, rows)
  if (!is.null(out) && nrow(out) > 0) {
    all_idx <- which(out$`Var Name` == "All Data")
    if (length(all_idx) && all_idx[1] != 1) {
      out <- rbind(out[all_idx[1], , drop = FALSE], out[-all_idx[1], , drop = FALSE])
      raw_rows <- c(raw_rows[all_idx[1]], raw_rows[-all_idx[1]])
    }
  }
  rownames(out) <- NULL
  colnames(out) <- CONST$EFFECT_SUMMARY_STATS$NAMES

  if (isTRUE(formal_output)) {
    out <- subset(out, select = !names(out) %in% c("Var Class", "Median", "Min", "Max", "SD"))
  }

  if (length(missing_vars) && get_verbosity() >= 2) {
    cli::cli_alert_warning(
      "Missing or non-numeric data for {.val {unique(missing_vars)}}"
    )
  }

  if (get_verbosity() >= 3) {
    cli::cli_h3("Summary statistics:")
    print_summary_table(out)
  }

  invisible(new_method_result(
    tables = list(summary = out),
    estimates = effect_summary_stats_estimates(raw_rows)
  ))
}

#' @title Tidy estimates for the effect summary statistics
#' @description
#' Flatten the summary table into the shared `estimates` schema. The method is
#' descriptive rather than inferential, so the mapping is a judgement call:
#' each row of the display table is one data subset and each of its columns one
#' statistic, so in the long schema `model` is the subset label (`"All Data"`,
#' or a variable group such as `"Study year > 2000"`) and `term` names the
#' statistic (`"mean"`, `"weighted_mean"`, `"median"`, `"min"`, `"max"`,
#' `"sd"`). `estimate` holds the statistic itself, and there is no test
#' statistic or p-value to fill.
#'
#' The two means carry the confidence bounds that belong to them in `conf_low`
#' and `conf_high`; the remaining statistics leave those `NA` rather than
#' borrowing the mean's interval. `n_obs` is the number of finite estimates in
#' the subset, and is repeated on every statistic of that subset.
#' @param raw_rows *\[list\]* Per-subset unrounded statistics, each a list with
#'   `label`, `unweighted`, and `weighted` elements.
#' @return *\[data.frame\]* A frame in the shared estimates schema.
effect_summary_stats_estimates <- function(raw_rows) {
  box::use(
    artma / modules / runtime_methods[new_estimates]
  )

  if (!is.list(raw_rows) || length(raw_rows) == 0L) {
    return(new_estimates())
  }

  rows <- lapply(raw_rows, function(entry) {
    unweighted <- entry$unweighted
    weighted <- entry$weighted

    data.frame(
      method = "effect_summary_stats",
      model = entry$label,
      term = c("mean", "weighted_mean", "median", "min", "max", "sd"),
      estimate = c(
        unweighted$mean, weighted$mean, unweighted$median,
        unweighted$min, unweighted$max, unweighted$sd
      ),
      conf_low = c(unweighted$ci[1], weighted$ci[1], NA_real_, NA_real_, NA_real_, NA_real_),
      conf_high = c(unweighted$ci[2], weighted$ci[2], NA_real_, NA_real_, NA_real_, NA_real_),
      n_obs = as.integer(unweighted$obs),
      stringsAsFactors = FALSE
    )
  })

  new_estimates(do.call(rbind, rows))
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  effect_summary_stats,
  stage = "effect_summary_stats",
  description = "Summary statistics of the main effect, grouped by the flagged variables",
  required_columns = c("effect", "study_size")
)

box::export(effect_summary_stats, effect_summary_stats_estimates, run)
