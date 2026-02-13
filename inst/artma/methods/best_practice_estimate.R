#' @title Best-Practice Estimate
#' @description
#' Compute best-practice estimates using coefficients from a Bayesian Model
#' Averaging (BMA) model. The method can reuse a previously computed BMA result
#' or run BMA on demand when missing.
best_practice_estimate <- function(df, bma_result = NULL) {
  box::use(
    artma / data_config / read[get_data_config],
    artma / libs / core / autonomy[get_autonomy_level],
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[assert, validate, validate_columns],
    artma / options / index[get_option_group]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "study_id"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Running Best-Practice Estimate")
  }

  opt <- get_option_group("artma.methods.best_practice_estimate")
  conf_level <- opt$conf_level %||% 0.95
  include_intercept <- opt$include_intercept %||% TRUE
  include_author_row <- opt$include_author_row %||% TRUE
  include_study_rows <- opt$include_study_rows %||% TRUE
  run_bma_if_missing <- opt$run_bma_if_missing %||% TRUE
  round_to <- as.integer(getOption("artma.output.number_of_decimals", 3))

  validate(
    is.numeric(conf_level),
    length(conf_level) == 1,
    is.logical(include_intercept),
    length(include_intercept) == 1,
    is.logical(include_author_row),
    length(include_author_row) == 1,
    is.logical(include_study_rows),
    length(include_study_rows) == 1,
    is.logical(run_bma_if_missing),
    length(run_bma_if_missing) == 1,
    is.numeric(round_to),
    length(round_to) == 1
  )

  assert(conf_level > 0 && conf_level < 1, "conf_level must be between 0 and 1.")
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

  override_resolution <- resolve_bpe_overrides(
    predictor_names = predictors,
    autonomy_level = autonomy_level,
    current_overrides = current_overrides,
    recommended_overrides = recommended_overrides
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
  z_value <- stats::qnorm((1 + conf_level) / 2)

  rows <- list()
  author_values <- compute_context_values(
    bma_data = bma_data,
    row_idx = seq_len(nrow(bma_data)),
    predictors = predictors,
    overrides = resolved_overrides
  )

  if (include_author_row) {
    rows[[length(rows) + 1]] <- build_bpe_row(
      scope = "author",
      study_id = NA_character_,
      study_label = "Author",
      predictor_values = author_values,
      coef_post_mean = coef_post_mean,
      include_intercept = include_intercept,
      vcov_matrix = vcov_matrix,
      z_value = z_value
    )
  }

  if (include_study_rows) {
    unique_ids <- unique(context$study_id)
    for (study_id in unique_ids) {
      row_idx <- which(context$study_id == study_id)
      if (!length(row_idx)) {
        next
      }

      study_values <- compute_context_values(
        bma_data = bma_data,
        row_idx = row_idx,
        predictors = predictors,
        overrides = resolved_overrides
      )

      study_label <- context$study_label[match(study_id, context$study_id)]
      rows[[length(rows) + 1]] <- build_bpe_row(
        scope = "study",
        study_id = as.character(study_id),
        study_label = as.character(study_label),
        predictor_values = study_values,
        coef_post_mean = coef_post_mean,
        include_intercept = include_intercept,
        vcov_matrix = vcov_matrix,
        z_value = z_value
      )
    }
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
    author_value = round_if_finite(as.numeric(author_values[predictors]), round_to),
    stringsAsFactors = FALSE
  )

  result <- list(
    summary = summary,
    formula = build_bpe_formula_string(
      coef_post_mean = coef_post_mean,
      predictor_values = author_values,
      include_intercept = include_intercept,
      round_to = round_to
    ),
    overrides = override_table,
    bma_formula = bma_formula,
    bma_source = resolved_bma$source,
    autonomy_level = autonomy_level
  )
  class(result) <- c("artma_best_practice_estimate", class(result))

  if (get_verbosity() >= 3) {
    cli::cli_h3("Best-Practice Estimate")
    cli::cli_alert_info("BMA source: {.val {result$bma_source}}")
    cli::cat_print(result$summary)
    if (get_verbosity() >= 4) {
      cli::cli_alert_info("Author formula: {result$formula}")
    }
  }

  invisible(result)
}

resolve_bma_input_for_bpe <- function(df, bma_result, run_bma_if_missing) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[assert, validate],
    artma / methods / bma[bma]
  )

  validate(
    is.data.frame(df),
    is.logical(run_bma_if_missing),
    length(run_bma_if_missing) == 1
  )

  normalized <- normalize_bma_result_input(bma_result)
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

  computed <- normalize_bma_result_input(bma(df))
  assert(
    is_bma_input_ready(computed),
    "BMA did not produce a usable model/data bundle for best-practice estimation."
  )

  computed$formula <- build_bma_formula_from_data(computed$data)
  computed$source <- "computed"
  computed
}

normalize_bma_result_input <- function(bma_result) {
  box::use(artma / libs / core / utils[get_verbosity])

  empty <- list(model = NULL, data = NULL, var_list = NULL, params = NULL)

  if (!is.list(bma_result)) {
    return(empty)
  }

  if (!is.null(bma_result$model) || !is.null(bma_result$data) || !is.null(bma_result$var_list)) {
    return(list(
      model = bma_result$model,
      data = bma_result$data,
      var_list = bma_result$var_list,
      params = bma_result$params
    ))
  }

  if (length(bma_result) > 0 && is.list(bma_result[[1]]) &&
    (!is.null(bma_result[[1]]$model) || !is.null(bma_result[[1]]$data) || !is.null(bma_result[[1]]$var_list))) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("Multiple BMA results were provided; using the first one for BPE.")
    }
    return(list(
      model = bma_result[[1]]$model,
      data = bma_result[[1]]$data,
      var_list = bma_result[[1]]$var_list,
      params = bma_result[[1]]$params
    ))
  }

  empty
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
  if (is.null(level)) {
    if (interactive()) {
      return(4L)
    }
    return(5L)
  }
  as.integer(level)
}

resolve_bpe_overrides <- function(predictor_names, autonomy_level, current_overrides, recommended_overrides) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.character(predictor_names),
    is.numeric(autonomy_level),
    is.list(current_overrides),
    is.list(recommended_overrides)
  )

  if (autonomy_level >= 4) {
    return(list(overrides = recommended_overrides, persist = FALSE))
  }

  if (autonomy_level == 3) {
    if (!interactive()) {
      return(list(overrides = recommended_overrides, persist = FALSE))
    }
    use_recommendations <- prompt_use_bpe_recommendations()
    if (isTRUE(use_recommendations)) {
      return(list(overrides = recommended_overrides, persist = FALSE))
    }
    return(list(overrides = current_overrides, persist = FALSE))
  }

  if (!interactive()) {
    if (autonomy_level == 2) {
      return(list(overrides = recommended_overrides, persist = FALSE))
    }
    return(list(overrides = current_overrides, persist = FALSE))
  }

  manual_overrides <- prompt_manual_bpe_overrides(
    predictor_names = predictor_names,
    current_overrides = current_overrides,
    recommended_overrides = recommended_overrides,
    show_recommendations = autonomy_level == 2
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
                                        show_recommendations) {
  box::use(artma / libs / core / utils[get_verbosity])

  overrides <- current_overrides

  choice_labels <- vapply(predictor_names, function(var_name) {
    current_label <- format_bpe_override(overrides[[var_name]])
    rec_label <- format_bpe_override(recommended_overrides[[var_name]])
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
      show_recommendations = show_recommendations
    )
  }

  overrides
}

prompt_single_bpe_override <- function(var_name, current_value, recommended_value, show_recommendations) {
  default_value <- if (show_recommendations && !is_bpe_override_na(recommended_value)) {
    recommended_value
  } else {
    current_value
  }

  default_label <- format_bpe_override(default_value)
  recommendation_note <- if (show_recommendations) {
    sprintf(" (recommended: %s)", format_bpe_override(recommended_value))
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

get_existing_bpe_overrides <- function(predictor_names, config) {
  box::use(artma / libs / core / utils[get_verbosity])

  overrides <- empty_bpe_override_map(predictor_names)

  for (var_name in predictor_names) {
    config_key <- find_config_key_for_var(var_name, config)
    if (is.null(config_key) || !is.list(config[[config_key]])) {
      next
    }

    raw_value <- config[[config_key]]$bpe
    parsed <- tryCatch(
      parse_bpe_override(raw_value, allow_na = TRUE, var_name = var_name),
      error = function(e) {
        if (get_verbosity() >= 2) {
          cli::cli_alert_warning(
            "Ignoring invalid configured BPE override for {.field {var_name}}: {e$message}"
          )
        }
        NA
      }
    )
    overrides[[var_name]] <- parsed
  }

  overrides
}

get_bpe_recommendations <- function(predictor_names, config) {
  recommendations <- empty_bpe_override_map(predictor_names)

  for (var_name in predictor_names) {
    config_key <- find_config_key_for_var(var_name, config)
    config_entry <- if (!is.null(config_key)) config[[config_key]]
    recommendations[[var_name]] <- infer_bpe_recommendation(var_name, config_entry)
  }

  recommendations
}

infer_bpe_recommendation <- function(var_name, config_entry = NULL) {
  label <- normalize_bpe_label(
    paste(
      var_name,
      config_entry$var_name_verbose %||% "",
      config_entry$var_name_description %||% ""
    )
  )

  if (identical(var_name, "se") || grepl("\\bse\\b|standard error", label, perl = TRUE)) {
    return(0)
  }

  if (matches_any(label, c("first lag instrument", "lag instrument first", "first lag iv"))) {
    return(0)
  }
  if (matches_any(label, c("nondurable consumption", "non durable consumption"))) {
    return(1)
  }
  if (matches_any(label, c("food consumption", "food only"))) {
    return(0)
  }
  if (matches_any(label, c("time dumm", "time fixed effect"))) {
    return(1)
  }
  if (matches_any(label, c("nonseparab", "non separab"))) {
    return(1)
  }
  if (matches_any(label, c("return on capital", "capital return"))) {
    return(1)
  }
  if (matches_any(label, c("micro data", "micro study", "micro level"))) {
    return(1)
  }
  if (matches_any(label, c("log linear approximation", "log linear euler"))) {
    return(0)
  }
  if (matches_any(label, c("hall 1988", "hall normalization"))) {
    return(1)
  }
  if (matches_any(label, c("eis", "relative risk aversion"))) {
    return(1)
  }
  if (matches_any(label, c("\\bgmm\\b", "generalized method of moments"))) {
    return(1)
  }
  if (matches_any(label, c("top journal", "top tier journal"))) {
    return(1)
  }
  if (matches_any(label, c("cross sectional", "cross section unit"))) {
    return("max")
  }
  if (matches_any(label, c("years of data", "data period"))) {
    return("max")
  }
  if (matches_any(label, c("average year", "mean year"))) {
    return("max")
  }
  if (matches_any(label, c("citation", "impact factor"))) {
    return("max")
  }

  NA
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

resolve_bpe_context <- function(df, bma_data) {
  if (!"study_id" %in% colnames(df)) {
    synthetic <- seq_len(nrow(bma_data))
    return(list(study_id = synthetic, study_label = as.character(synthetic)))
  }

  aligned_df <- NULL
  shared_cols <- intersect(colnames(bma_data), colnames(df))
  if (length(shared_cols) == ncol(bma_data)) {
    complete_rows <- stats::complete.cases(df[, shared_cols, drop = FALSE])
    if (sum(complete_rows) == nrow(bma_data)) {
      aligned_df <- df[complete_rows, , drop = FALSE]
    }
  }

  if (is.null(aligned_df) && nrow(df) == nrow(bma_data)) {
    aligned_df <- df
  }

  if (is.null(aligned_df)) {
    fallback <- seq_len(nrow(bma_data))
    return(list(study_id = fallback, study_label = as.character(fallback)))
  }

  labels <- if ("study_label" %in% colnames(aligned_df)) {
    as.character(aligned_df$study_label)
  } else {
    as.character(aligned_df$study_id)
  }

  list(
    study_id = aligned_df$study_id,
    study_label = labels
  )
}

resolve_bpe_vcov <- function(ols_model, cluster_ids = NULL) {
  tryCatch(
    {
      if (!is.null(cluster_ids) && length(cluster_ids) == stats::nobs(ols_model)) {
        return(sandwich::vcovCL(ols_model, cluster = cluster_ids, type = "HC0"))
      }
      sandwich::vcovHC(ols_model, type = "HC0")
    },
    error = function(e) {
      stats::vcov(ols_model)
    }
  )
}

build_bpe_row <- function(scope, study_id, study_label, predictor_values, coef_post_mean,
                          include_intercept, vcov_matrix, z_value) {
  intercept <- if (include_intercept && "(Intercept)" %in% names(coef_post_mean)) {
    as.numeric(coef_post_mean["(Intercept)"])
  } else {
    0
  }

  predictor_coefs <- coef_post_mean[names(predictor_values)]
  estimate <- intercept + sum(predictor_coefs * predictor_values, na.rm = TRUE)
  standard_error <- compute_linear_combo_se(
    predictor_values = predictor_values,
    include_intercept = include_intercept,
    vcov_matrix = vcov_matrix
  )

  ci_lower <- if (is.finite(estimate) && is.finite(standard_error)) {
    estimate - z_value * standard_error
  } else {
    NA_real_
  }
  ci_upper <- if (is.finite(estimate) && is.finite(standard_error)) {
    estimate + z_value * standard_error
  } else {
    NA_real_
  }

  data.frame(
    scope = scope,
    study_id = study_id,
    study_label = study_label,
    estimate = estimate,
    standard_error = standard_error,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    stringsAsFactors = FALSE
  )
}

compute_linear_combo_se <- function(predictor_values, include_intercept, vcov_matrix) {
  coef_names <- rownames(vcov_matrix)
  if (is.null(coef_names) || !length(coef_names)) {
    return(NA_real_)
  }

  c_vector <- stats::setNames(rep(0, length(coef_names)), coef_names)
  if (include_intercept && "(Intercept)" %in% coef_names) {
    c_vector["(Intercept)"] <- 1
  }

  for (var_name in names(predictor_values)) {
    if (var_name %in% coef_names && is.finite(predictor_values[[var_name]])) {
      c_vector[[var_name]] <- predictor_values[[var_name]]
    }
  }

  variance <- as.numeric(
    t(c_vector) %*% vcov_matrix[coef_names, coef_names, drop = FALSE] %*% c_vector
  )

  if (!is.finite(variance) || variance < 0) {
    return(NA_real_)
  }

  sqrt(variance)
}

compute_context_values <- function(bma_data, row_idx, predictors, overrides) {
  values <- vapply(predictors, function(var_name) {
    resolve_bpe_value(
      values = bma_data[row_idx, var_name, drop = TRUE],
      override = overrides[[var_name]]
    )
  }, numeric(1))
  values
}

resolve_bpe_value <- function(values, override) {
  numeric_values <- as.numeric(values)
  numeric_values <- numeric_values[is.finite(numeric_values)]

  if (!length(numeric_values)) {
    return(NA_real_)
  }

  if (is_bpe_override_na(override)) {
    return(mean(numeric_values, na.rm = TRUE))
  }

  parsed <- parse_bpe_override(override, allow_na = TRUE)
  if (is_bpe_override_na(parsed)) {
    return(mean(numeric_values, na.rm = TRUE))
  }

  if (is.numeric(parsed) && length(parsed) == 1 && !is.na(parsed)) {
    return(as.numeric(parsed))
  }

  switch(parsed,
    mean = mean(numeric_values, na.rm = TRUE),
    median = stats::median(numeric_values, na.rm = TRUE),
    min = min(numeric_values, na.rm = TRUE),
    max = max(numeric_values, na.rm = TRUE),
    mean(numeric_values, na.rm = TRUE)
  )
}

build_bpe_formula_string <- function(coef_post_mean, predictor_values, include_intercept, round_to) {
  parts <- character(0)

  if (include_intercept && "(Intercept)" %in% names(coef_post_mean)) {
    parts <- c(parts, format(round(coef_post_mean["(Intercept)"], round_to), nsmall = round_to))
  }

  for (var_name in names(predictor_values)) {
    beta <- coef_post_mean[[var_name]]
    value <- predictor_values[[var_name]]
    if (!is.finite(beta) || !is.finite(value)) {
      next
    }
    parts <- c(
      parts,
      sprintf(
        "%s * %s (%s)",
        format(round(beta, round_to), nsmall = round_to),
        format(round(value, round_to), nsmall = round_to),
        var_name
      )
    )
  }

  if (!length(parts)) {
    return("0")
  }

  paste(parts, collapse = " + ")
}

parse_bpe_override <- function(value, allow_na = TRUE, var_name = NULL) {
  allowed_strings <- c("mean", "median", "min", "max")

  fail <- function(msg) {
    if (!is.null(var_name)) {
      cli::cli_abort("Invalid BPE override for {.field {var_name}}: {msg}")
    }
    cli::cli_abort("Invalid BPE override: {msg}")
  }

  if (is.null(value) || length(value) == 0) {
    if (allow_na) {
      return(NA)
    }
    fail("value cannot be empty")
  }

  if (is.numeric(value) && length(value) == 1) {
    if (is.na(value) && allow_na) {
      return(NA)
    }
    if (is.na(value)) {
      fail("numeric value cannot be NA")
    }
    return(as.numeric(value))
  }

  if (!is.character(value) || length(value) != 1) {
    fail("value must be a scalar numeric or character")
  }

  cleaned <- tolower(trimws(value))
  if (!nzchar(cleaned) || cleaned %in% c("na", "none", "default", "null")) {
    if (allow_na) {
      return(NA)
    }
    fail("empty/default-like value is not allowed here")
  }

  if (cleaned %in% allowed_strings) {
    return(cleaned)
  }

  numeric_value <- suppressWarnings(as.numeric(cleaned))
  if (!is.na(numeric_value)) {
    return(numeric_value)
  }

  fail("use numeric, mean, median, min, max, or default")
}

format_bpe_override <- function(value) {
  if (is_bpe_override_na(value)) {
    return("default(mean)")
  }
  if (is.numeric(value)) {
    return(as.character(signif(value, 6)))
  }
  as.character(value)
}

is_bpe_override_na <- function(value) {
  is.null(value) || length(value) == 0 || (length(value) == 1 && is.na(value))
}

round_if_finite <- function(x, digits) {
  out <- as.numeric(x)
  finite <- is.finite(out)
  out[finite] <- round(out[finite], digits)
  out
}

find_config_key_for_var <- function(var_name, config) {
  if (var_name %in% names(config)) {
    return(var_name)
  }

  var_key <- make.names(var_name)
  if (var_key %in% names(config)) {
    return(var_key)
  }

  matches <- names(config)[vapply(config, function(entry) {
    is.list(entry) && is.character(entry$var_name) && identical(entry$var_name, var_name)
  }, logical(1))]

  if (length(matches)) {
    return(matches[1])
  }

  NULL
}

empty_bpe_override_map <- function(var_names) {
  stats::setNames(lapply(var_names, function(x) NA), var_names)
}

normalize_bpe_label <- function(label) {
  cleaned <- gsub("[^a-zA-Z0-9]+", " ", label)
  tolower(trimws(cleaned))
}

matches_any <- function(label, patterns) {
  any(vapply(patterns, function(pattern) {
    grepl(pattern, label, perl = TRUE)
  }, logical(1)))
}


box::use(
  artma / libs / infrastructure / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

run <- cache_cli_runner(
  best_practice_estimate,
  stage = "best_practice_estimate",
  key_builder = function(...) build_data_cache_signature()
)

box::export(best_practice_estimate, run)
