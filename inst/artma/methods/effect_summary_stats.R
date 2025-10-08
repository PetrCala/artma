#' @title Effect summary statistics
#' @description
#' Compute summary statistics for the main effect grouped by variables that are
#' flagged in the data configuration. The function supports equality based
#' splits as well as threshold splits (numeric, mean or median based). It
#' returns the arithmetic mean, weighted mean (weighted by the inverse squared
#' study size), confidence intervals, and additional distribution statistics.
effect_summary_stats <- function(df) {
  box::use(
    artma / const[CONST],
    artma / data_config / read[get_data_config],
    artma / libs / utils[get_verbosity],
    artma / libs / validation[assert, validate, validate_columns],
    artma / options / index[get_option_group]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "study_size"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Summarizing the main effect")
  }

  config <- get_data_config()
  opt <- get_option_group("artma.methods.effect_summary_stats")
  conf_level <- opt$conf_level %||% 0.95
  formal_output <- opt$formal_output %||% FALSE
  round_to <- getOption("artma.output.number_of_decimals", 3)

  validate(
    length(conf_level) == 1,
    is.numeric(conf_level),
    length(formal_output) == 1,
    is.logical(formal_output),
    is.numeric(round_to)
  )

  assert(conf_level >= 0 && conf_level <= 1, "Confidence level must be between 0 and 1.")
  assert(round_to >= 0, "Number of decimals must be greater than or equal to 0.")

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
    sd_for_ci <- round(sd_val, round_to)
    se_val <- if (!is.na(sd_for_ci) && length(values) > 1) sd_for_ci / sqrt(length(values)) else NA_real_
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
    # Weighted variance without Bessel correction keeps behaviour stable
    variance <- stats::weighted.mean((values - mean_val)^2, w = norm_weights)
    sd_val <- sqrt(variance)
    se_val <- if (!is.na(sd_val) && length(values) > 1) sd_val / sqrt(length(values)) else NA_real_
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

  format_label <- function(base_label, suffix) {
    if (nzchar(suffix)) {
      paste0(base_label, suffix)
    } else {
      base_label
    }
  }

  format_equal_suffix <- function(value) {
    if (is.numeric(value)) {
      return(paste0(" = ", round(value, round_to)))
    }
    if (is.character(value) && nzchar(value)) {
      return(paste0(" = ", value))
    }
    ""
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
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("No variables configured for effect summary statistics.")
    }

    # Check if interactive mode is available
    if (interactive()) {
      box::use(
        artma / libs / effect_summary_stats_interactive[
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

    # If still no variables (user declined or non-interactive), return empty
    if (!length(effect_vars)) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("No variables selected to compute summary statistics for.")
      }
      empty <- data.frame(matrix(nrow = 0, ncol = length(CONST$EFFECT_SUMMARY_STATS$NAMES)), stringsAsFactors = FALSE)
      colnames(empty) <- CONST$EFFECT_SUMMARY_STATS$NAMES
      return(empty)
    }
  }

  add_row <- function(label, class_name, subset_data) {
    if (!length(subset_data$effect)) {
      return(FALSE)
    }

    weights <- 1 / (subset_data$study_size^2)
    unweighted <- compute_unweighted_stats(subset_data$effect)
    weighted <- compute_weighted_stats(subset_data$effect, weights)

    rows[[length(rows) + 1]] <<- data.frame(
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

    TRUE
  }

  rows <- list()
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

    added_any <- FALSE

    if (!is.na(equal_val)) {
      subset <- prepare_subset(!is.na(var_data) & var_data == equal_val)
      label <- format_label(var_label, format_equal_suffix(equal_val))
      added_any <- add_row(label, var_class, subset) || added_any
    }

    if (!is.na(gltl_val)) {
      if (is.character(gltl_val)) {
        gltl_val <- switch(gltl_val,
          mean = if (length(filtered_var_data)) mean(filtered_var_data, na.rm = TRUE) else NA_real_,
          median = if (length(filtered_var_data)) stats::median(filtered_var_data, na.rm = TRUE) else NA_real_,
          suppressWarnings(as.numeric(gltl_val))
        )
      }

      if (is.na(gltl_val)) {
        missing_vars <- c(missing_vars, var_name)
      } else {
        subset_ge <- prepare_subset(!is.na(var_data) & var_data >= gltl_val)
        subset_lt <- prepare_subset(!is.na(var_data) & var_data < gltl_val)

        label_ge <- format_label(var_label, paste0(" >= ", round(gltl_val, round_to)))
        label_lt <- format_label(var_label, paste0(" < ", round(gltl_val, round_to)))

        added_any <- add_row(label_ge, var_class, subset_ge) || added_any
        added_any <- add_row(label_lt, var_class, subset_lt) || added_any
      }
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
    cli::cat_print(out)
  }

  out
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

box::export(effect_summary_stats, run)
