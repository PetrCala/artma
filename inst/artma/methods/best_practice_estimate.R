#' @title Best-Practice Estimate
#' @description
#' Compute best-practice estimates using coefficients from a Bayesian Model
#' Averaging (BMA) model. The method can reuse a previously computed BMA result
#' or run BMA on demand when missing.
#'
#' This file owns option resolution, prompting, printing, and result assembly.
#' The numeric core lives in `econometric/best_practice_estimate.R` and the plot
#' builders in `visualization/best_practice_estimate.R`.
NULL

box::use(
  artma / econometric / best_practice_estimate[
    build_bpe_formula_string,
    build_bpe_row,
    compute_bpe_economic_significance,
    compute_bpe_factor_summary,
    compute_context_values,
    detect_bpe_standardized_predictors,
    find_config_key_for_var,
    format_bpe_override,
    format_bpe_recommendation,
    get_bpe_recommendations,
    get_existing_bpe_overrides,
    is_bpe_override_na,
    parse_bpe_override,
    resolve_bpe_context,
    resolve_bpe_vcov,
    round_if_finite
  ],
  artma / visualization / best_practice_estimate[build_bpe_plots]
)

best_practice_estimate <- function(df, bma_result = NULL) {
  box::use(
    artma / data_config / read[get_data_config],
    artma / libs / core / autonomy[get_autonomy_level],
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[assert, validate, validate_columns],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / resolver[opt_spec, resolve_options],
    artma / visualization / options[get_visualization_options],
    artma / visualization / export[export_named_plots],
    artma / libs / formatting / results[print_summary_table]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "study_id"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Running Best-Practice Estimate")
  }

  opt <- get_option_group("artma.methods.best_practice_estimate")

  resolved <- resolve_options(opt, list(
    conf_level = opt_spec(
      default = 0.95, type = "numeric", scalar = TRUE,
      constraint = function(x) x > 0 && x < 1,
      constraint_msg = "conf_level must be between 0 and 1."
    ),
    include_intercept = opt_spec(default = TRUE, type = "logical", scalar = TRUE),
    include_author_row = opt_spec(default = TRUE, type = "logical", scalar = TRUE),
    include_study_rows = opt_spec(default = TRUE, type = "logical", scalar = TRUE),
    run_bma_if_missing = opt_spec(default = TRUE, type = "logical", scalar = TRUE),
    include_economic_significance = opt_spec(default = TRUE, type = "logical", scalar = TRUE),
    economic_significance_pip_threshold = opt_spec(
      default = NA_real_, type = "numeric", allow_na = TRUE, scalar = TRUE,
      cast = as.numeric,
      constraint = function(x) x >= 0 && x <= 1,
      constraint_msg = "economic_significance_pip_threshold must be between 0 and 1."
    ),
    include_factor_summary = opt_spec(default = TRUE, type = "logical", scalar = TRUE),
    round_to = opt_spec(
      default = 3L, type = "numeric", key = "artma.output.number_of_decimals",
      cast = as.integer, scalar = TRUE
    )
  ))

  conf_level <- resolved$conf_level
  include_intercept <- resolved$include_intercept
  include_author_row <- resolved$include_author_row
  include_study_rows <- resolved$include_study_rows
  run_bma_if_missing <- resolved$run_bma_if_missing
  include_economic_significance <- resolved$include_economic_significance
  economic_significance_pip_threshold <- resolved$economic_significance_pip_threshold
  include_factor_summary <- resolved$include_factor_summary
  round_to <- resolved$round_to

  # Cross-option constraint: at least one row scope must remain enabled. Kept
  # outside the per-option resolver because it spans two options.
  assert(
    include_author_row || include_study_rows,
    "At least one of include_author_row or include_study_rows must be TRUE."
  )

  resolved_bma <- resolve_bma_input_for_bpe(
    df = df,
    bma_result = bma_result,
    run_bma_if_missing = run_bma_if_missing
  )

  bma_model <- resolved_bma$model
  bma_data <- resolved_bma$data
  bma_formula <- resolved_bma$formula

  bma_coef_matrix <- stats::coef(
    bma_model,
    order.by.pip = FALSE,
    exact = TRUE,
    include.constant = TRUE
  )
  coef_post_mean <- as.numeric(bma_coef_matrix[, "Post Mean"])
  names(coef_post_mean) <- rownames(bma_coef_matrix)
  pip_values <- as.numeric(bma_coef_matrix[, "PIP"])
  names(pip_values) <- rownames(bma_coef_matrix)

  predictors <- setdiff(names(coef_post_mean), "(Intercept)")
  missing_predictors <- predictors[!predictors %in% colnames(bma_data)]
  assert(
    length(missing_predictors) == 0,
    paste0(
      "BMA model coefficients reference variables not present in BMA data: ",
      paste(missing_predictors, collapse = ", ")
    )
  )

  config <- get_data_config()
  autonomy_level <- resolve_effective_autonomy_level(get_autonomy_level())
  current_overrides <- get_existing_bpe_overrides(predictors, config)
  recommended_overrides <- get_bpe_recommendations(predictors, config)
  # A recommendation of NA is ambiguous between "we recommend the mean" and "we
  # have no idea": has_recommendation makes the latter an explicit, reportable
  # outcome instead of silently blending into the mean fallback.
  has_recommendation <- stats::setNames(
    vapply(predictors, function(var_name) !is_bpe_override_na(recommended_overrides[[var_name]]), logical(1)),
    predictors
  )

  override_resolution <- resolve_bpe_overrides(
    predictor_names = predictors,
    autonomy_level = autonomy_level,
    current_overrides = current_overrides,
    recommended_overrides = recommended_overrides,
    has_recommendation = has_recommendation
  )
  resolved_overrides <- override_resolution$overrides

  if (isTRUE(override_resolution$persist)) {
    save_bpe_overrides_to_config(
      overrides = resolved_overrides,
      predictor_names = predictors,
      config = config
    )
  }

  context <- resolve_bpe_context(df = df, bma_data = bma_data)
  ols_model <- stats::lm(formula = bma_formula, data = bma_data)
  vcov_matrix <- resolve_bpe_vcov(ols_model = ols_model, cluster_ids = context$study_id)

  # SEs are study-clustered, so t quantiles use G - 1 degrees of freedom (the
  # normal quantile understates critical values at typical study counts).
  n_clusters <- length(unique(context$study_id))
  z_value <- stats::qt((1 + conf_level) / 2, df = max(n_clusters - 1L, 1L))

  # Scaling metadata recorded by get_bma_data(); absent (identity) when the
  # BMA data was never standardized.
  scale_centers <- as.list(attr(bma_data, "bpe_scale_centers") %||% numeric(0))
  scale_scales <- as.list(attr(bma_data, "bpe_scale_scales") %||% numeric(0))
  effect_center <- scale_centers[["effect"]] %||% 0
  effect_scale <- scale_scales[["effect"]] %||% 1

  author_values <- compute_context_values(
    bma_data = bma_data,
    row_idx = seq_len(nrow(bma_data)),
    predictors = predictors,
    overrides = resolved_overrides,
    centers = scale_centers,
    scales = scale_scales
  )

  # Computed unconditionally (regardless of include_author_row/include_study_rows)
  # because plots reuse the author reference point and per-study estimates.
  author_row <- build_bpe_row(
    scope = "author",
    study_id = NA_character_,
    study_label = "Author",
    predictor_values = author_values,
    coef_post_mean = coef_post_mean,
    include_intercept = include_intercept,
    vcov_matrix = vcov_matrix,
    z_value = z_value,
    effect_center = effect_center,
    effect_scale = effect_scale
  )

  # First-appearance level order keeps summary rows in data order; NA study
  # ids drop out of the factor, matching the previous which()-based skip.
  study_index_groups <- split(
    seq_along(context$study_id),
    factor(context$study_id, levels = unique(context$study_id))
  )
  study_rows <- vector("list", length(study_index_groups))
  for (i in seq_along(study_index_groups)) {
    row_idx <- study_index_groups[[i]]

    study_values <- compute_context_values(
      bma_data = bma_data,
      row_idx = row_idx,
      predictors = predictors,
      overrides = resolved_overrides,
      centers = scale_centers,
      scales = scale_scales
    )

    study_rows[[i]] <- build_bpe_row(
      scope = "study",
      study_id = names(study_index_groups)[[i]],
      study_label = as.character(context$study_label[row_idx[[1]]]),
      predictor_values = study_values,
      coef_post_mean = coef_post_mean,
      include_intercept = include_intercept,
      vcov_matrix = vcov_matrix,
      z_value = z_value,
      effect_center = effect_center,
      effect_scale = effect_scale
    )
  }

  rows <- list()
  if (include_author_row) {
    rows[[length(rows) + 1]] <- author_row
  }
  if (include_study_rows) {
    rows <- c(rows, study_rows)
  }

  summary <- do.call(rbind, rows)
  summary$estimate <- round_if_finite(summary$estimate, round_to)
  summary$standard_error <- round_if_finite(summary$standard_error, round_to)
  summary$ci_lower <- round_if_finite(summary$ci_lower, round_to)
  summary$ci_upper <- round_if_finite(summary$ci_upper, round_to)

  override_table <- data.frame(
    variable = predictors,
    override = vapply(
      predictors,
      function(var_name) format_bpe_override(resolved_overrides[[var_name]]),
      character(1)
    ),
    recommended = vapply(
      predictors,
      function(var_name) {
        format_bpe_recommendation(recommended_overrides[[var_name]], has_recommendation[[var_name]])
      },
      character(1)
    ),
    has_recommendation = as.logical(has_recommendation[predictors]),
    # Report the plugged-in values on the raw data scale, not the z-scored one.
    author_value = round_if_finite(
      vapply(predictors, function(var_name) {
        (scale_centers[[var_name]] %||% 0) +
          (scale_scales[[var_name]] %||% 1) * as.numeric(author_values[[var_name]])
      }, numeric(1)),
      round_to
    ),
    stringsAsFactors = FALSE
  )

  formula <- build_bpe_formula_string(
    coef_post_mean = coef_post_mean,
    predictor_values = author_values,
    include_intercept = include_intercept,
    round_to = round_to,
    standardized_predictors = detect_bpe_standardized_predictors(bma_data, predictors)
  )

  tables <- list(summary = summary)

  if (include_economic_significance) {
    tables$economic_significance <- compute_bpe_economic_significance(
      predictors = predictors,
      bma_data = bma_data,
      coef_post_mean = coef_post_mean,
      bpe_reference_estimate = author_row$estimate,
      pip_values = pip_values,
      config = config,
      round_to = round_to,
      pip_threshold = economic_significance_pip_threshold,
      effect_scale = effect_scale
    )
  }

  if (include_factor_summary) {
    tables$summary_by_factor <- compute_bpe_factor_summary(
      predictors = predictors,
      config = config,
      bma_data = bma_data,
      coef_post_mean = coef_post_mean,
      include_intercept = include_intercept,
      vcov_matrix = vcov_matrix,
      z_value = z_value,
      overrides = resolved_overrides,
      round_to = round_to,
      centers = scale_centers,
      scales = scale_scales
    )
  }

  vis <- get_visualization_options()
  plots <- build_bpe_plots(
    study_rows = study_rows,
    author_estimate = author_row$estimate,
    predictors = predictors,
    config = config,
    bma_data = bma_data,
    study_index_groups = study_index_groups,
    round_to = round_to,
    theme_name = vis$theme
  )

  if (isTRUE(vis$export_graphics) && length(plots)) {
    export_named_plots(
      plots = plots,
      base_name = "best_practice_estimate",
      export_path = vis$export_path,
      graph_scale = vis$graph_scale,
      width = 800,
      height = 600
    )
  }

  if (get_verbosity() >= 3) {
    cli::cli_h3("Best-Practice Estimate")
    cli::cli_alert_info("BMA source: {.val {resolved_bma$source}}")
    print_summary_table(summary)
    if (get_verbosity() >= 4) {
      cli::cli_alert_info("Author formula: {formula}")
    }
  }

  invisible(new_method_result(
    tables = tables,
    plots = plots,
    meta = list(
      formula = formula,
      overrides = override_table,
      bma_formula = bma_formula,
      bma_source = resolved_bma$source,
      autonomy_level = autonomy_level
    )
  ))
}

resolve_bma_input_for_bpe <- function(df, bma_result, run_bma_if_missing) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[assert, validate],
    artma / methods / bma[bma, unwrap_bma_result]
  )

  validate(
    is.data.frame(df),
    is.logical(run_bma_if_missing),
    length(run_bma_if_missing) == 1
  )

  normalized <- unwrap_bma_result(bma_result)
  if (is_bma_input_ready(normalized)) {
    normalized$formula <- build_bma_formula_from_data(normalized$data)
    normalized$source <- "provided"
    return(normalized)
  }

  if (!isTRUE(run_bma_if_missing)) {
    cli::cli_abort("Best-practice estimate requires a BMA result and run_bma_if_missing is FALSE.")
  }

  should_run_bma <- TRUE
  if (interactive()) {
    should_run_bma <- prompt_run_bma_for_bpe()
  }

  if (!isTRUE(should_run_bma)) {
    cli::cli_abort("Best-practice estimate aborted because BMA was not run.")
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Running BMA first because BPE needs BMA model inputs.")
  }

  computed <- unwrap_bma_result(bma(df))
  assert(
    is_bma_input_ready(computed),
    if (!is.null(computed$skipped)) {
      sprintf("Best-practice estimate requires BMA, but BMA was skipped: %s.", computed$skipped)
    } else {
      "BMA did not produce a usable model/data bundle for best-practice estimation."
    }
  )

  computed$formula <- build_bma_formula_from_data(computed$data)
  computed$source <- "computed"
  computed
}

is_bma_input_ready <- function(result) {
  is.list(result) &&
    !is.null(result$model) &&
    inherits(result$model, "bma") &&
    is.data.frame(result$data)
}

build_bma_formula_from_data <- function(bma_data) {
  predictors <- setdiff(colnames(bma_data), "effect")
  if (!length(predictors)) {
    return(stats::as.formula("effect ~ 1"))
  }
  stats::reformulate(termlabels = predictors, response = "effect")
}

prompt_run_bma_for_bpe <- function() {
  choices <- c(
    "Yes, run BMA first (recommended)" = "yes",
    "No, stop this run" = "no"
  )

  selected <- climenu::select(
    choices = names(choices),
    prompt = "Best-practice estimate requires BMA. Do you want to run BMA first?",
    selected = 1
  )

  if (rlang::is_empty(selected)) {
    return(FALSE)
  }

  choices[selected][[1]] == "yes"
}

resolve_effective_autonomy_level <- function(level) {
  box::use(artma / const[CONST])
  level %||% CONST$AUTONOMY$DEFAULT
}

resolve_bpe_overrides <- function(predictor_names, autonomy_level, current_overrides, recommended_overrides,
                                  has_recommendation) {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[validate]
  )

  validate(
    is.character(predictor_names),
    is.character(autonomy_level),
    length(autonomy_level) == 1,
    autonomy_level %in% CONST$AUTONOMY$LEVELS,
    is.list(current_overrides),
    is.list(recommended_overrides),
    is.logical(has_recommendation)
  )

  if (autonomy_level == "autonomous") {
    return(list(overrides = recommended_overrides, persist = FALSE))
  }

  # interactive() is the hard gate: non-interactive sessions never prompt.
  # "ask_more" keeps the configured/current overrides rather than silently
  # applying recommendations; "balanced" still defers to recommendations.
  if (!interactive()) {
    if (autonomy_level == "ask_more") {
      return(list(overrides = current_overrides, persist = FALSE))
    }
    return(list(overrides = recommended_overrides, persist = FALSE))
  }

  if (autonomy_level == "balanced") {
    use_recommendations <- prompt_use_bpe_recommendations()
    if (isTRUE(use_recommendations)) {
      return(list(overrides = recommended_overrides, persist = FALSE))
    }
    return(list(overrides = current_overrides, persist = FALSE))
  }

  manual_overrides <- prompt_manual_bpe_overrides(
    predictor_names = predictor_names,
    current_overrides = current_overrides,
    recommended_overrides = recommended_overrides,
    has_recommendation = has_recommendation,
    show_recommendations = TRUE
  )

  list(overrides = manual_overrides, persist = TRUE)
}

prompt_use_bpe_recommendations <- function() {
  choices <- c(
    "Yes, apply recommendations" = "yes",
    "No, keep configured/default values" = "no"
  )

  selected <- climenu::select(
    choices = names(choices),
    prompt = "Use literature-based recommendations for BPE overrides?",
    selected = 1
  )

  if (rlang::is_empty(selected)) {
    return(TRUE)
  }

  choices[selected][[1]] == "yes"
}

prompt_manual_bpe_overrides <- function(predictor_names, current_overrides, recommended_overrides,
                                        has_recommendation, show_recommendations) {
  box::use(artma / libs / core / utils[get_verbosity])

  overrides <- current_overrides

  choice_labels <- vapply(predictor_names, function(var_name) {
    current_label <- format_bpe_override(overrides[[var_name]])
    rec_label <- format_bpe_recommendation(recommended_overrides[[var_name]], has_recommendation[[var_name]])
    if (show_recommendations) {
      sprintf("%s (current: %s, recommended: %s)", var_name, current_label, rec_label)
    } else {
      sprintf("%s (current: %s)", var_name, current_label)
    }
  }, character(1))

  selected_idx <- climenu::checkbox(
    choices = choice_labels,
    prompt = paste(
      "Select variables you want to modify in BPE",
      "(SPACE to select, ENTER to confirm)"
    ),
    return_index = TRUE,
    allow_select_all = TRUE
  )

  if (rlang::is_empty(selected_idx) || !length(selected_idx)) {
    if (get_verbosity() >= 3) {
      cli::cli_alert_info("No variables selected for manual override.")
    }
    return(overrides)
  }

  for (idx in selected_idx) {
    var_name <- predictor_names[idx]
    overrides[[var_name]] <- prompt_single_bpe_override(
      var_name = var_name,
      current_value = overrides[[var_name]],
      recommended_value = recommended_overrides[[var_name]],
      has_recommendation = isTRUE(has_recommendation[[var_name]]),
      show_recommendations = show_recommendations
    )
  }

  overrides
}

prompt_single_bpe_override <- function(var_name, current_value, recommended_value, has_recommendation,
                                       show_recommendations) {
  default_value <- if (show_recommendations && has_recommendation) {
    recommended_value
  } else {
    current_value
  }

  default_label <- format_bpe_override(default_value)
  recommendation_note <- if (show_recommendations) {
    sprintf(" (recommended: %s)", format_bpe_recommendation(recommended_value, has_recommendation))
  } else {
    ""
  }

  cli::cli_text("Variable: {.field {var_name}}{recommendation_note}")
  cli::cli_text("Enter one of: numeric, mean, median, min, max, default")

  for (attempt in seq_len(3)) {
    raw_value <- readline(
      prompt = sprintf("Override value [default: %s]: ", default_label)
    )
    input_value <- trimws(raw_value)

    if (!nzchar(input_value)) {
      return(default_value)
    }

    if (tolower(input_value) %in% c("default", "none", "clear")) {
      return(NA)
    }

    if (tolower(input_value) == "recommended" && show_recommendations) {
      return(recommended_value)
    }

    parsed <- tryCatch(
      parse_bpe_override(input_value, allow_na = TRUE, var_name = var_name),
      error = function(e) NULL
    )
    if (!is.null(parsed)) {
      return(parsed)
    }

    cli::cli_alert_warning("Invalid override for {.field {var_name}}. Please try again.")
  }

  cli::cli_alert_warning("Too many invalid inputs. Keeping previous value for {.field {var_name}}.")
  current_value
}

save_bpe_overrides_to_config <- function(overrides, predictor_names, config) {
  box::use(
    artma / data_config / write[update_data_config],
    artma / libs / core / utils[get_verbosity]
  )

  changes <- list()
  for (var_name in predictor_names) {
    config_key <- find_config_key_for_var(var_name, config)
    if (is.null(config_key)) {
      next
    }
    override <- overrides[[var_name]]
    changes[[config_key]] <- list(
      bpe = if (is_bpe_override_na(override)) NA else override
    )
  }

  if (!length(changes)) {
    return(invisible(FALSE))
  }

  saved <- tryCatch(
    {
      update_data_config(changes)
      TRUE
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning("Could not persist BPE overrides: {e$message}")
      }
      FALSE
    }
  )

  if (saved && get_verbosity() >= 3) {
    cli::cli_alert_success("Saved BPE overrides to data config.")
  }

  invisible(saved)
}

box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  best_practice_estimate,
  stage = "best_practice_estimate",
  depends_on = "bma",
  required_columns = c("effect", "study_id"),
  suggests = "BMS"
)

box::export(
  best_practice_estimate, run, resolve_bma_input_for_bpe
)
