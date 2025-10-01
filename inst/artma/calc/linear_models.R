box::use(
  artma / libs / utils[get_verbosity],
  artma / libs / validation[assert, validate, validate_columns],
  cli[cli_alert_info, cli_alert_warning],
  lmtest[coeftest],
  plm[plm, within_intercept],
  sandwich[vcovHC],
  stats[pnorm, quantile]
)

LINEAR_TERM_MAP <- c(
  effect = "(Intercept)",
  publication_bias = "se"
)

LINEAR_TERM_LABELS <- c(
  effect = "Effect Beyond Bias",
  publication_bias = "Publication Bias"
)

format_numeric_value <- function(value, digits) {
  if (length(value) == 0L || is.na(value) || !is.finite(value)) {
    return(NA_character_)
  }
  formatC(value, format = "f", digits = digits)
}

format_ci_interval <- function(lower, upper, digits) {
  if (is.na(lower) || is.na(upper) || !is.finite(lower) || !is.finite(upper)) {
    return(NA_character_)
  }
  lower_fmt <- format_numeric_value(lower, digits)
  upper_fmt <- format_numeric_value(upper, digits)
  if (is.na(lower_fmt) || is.na(upper_fmt)) {
    return(NA_character_)
  }
  paste0("[", lower_fmt, ", ", upper_fmt, "]")
}

compute_significance_mark <- function(p_value, levels = c("***" = 0.01, "**" = 0.05, "*" = 0.1)) {
  if (is.na(p_value)) {
    return("")
  }
  marks <- names(levels)
  for (mark in marks) {
    if (p_value <= levels[[mark]]) {
      return(mark)
    }
  }
  ""
}

format_estimate_with_mark <- function(value, digits, mark) {
  formatted <- format_numeric_value(value, digits)
  if (is.na(formatted)) {
    return(NA_character_)
  }
  if (is.na(mark) || !nzchar(mark)) {
    return(formatted)
  }
  paste0(formatted, mark)
}

format_std_error <- function(value, digits) {
  formatted <- format_numeric_value(value, digits)
  if (is.na(formatted)) {
    return(NA_character_)
  }
  paste0("(", formatted, ")")
}

default_coefficient_extractor <- function(model, data, vcov_fun) {
  vcov_matrix <- if (is.null(vcov_fun)) NULL else vcov_fun(model, data)
  coefs <- if (is.null(vcov_matrix)) {
    coeftest(model)
  } else {
    coeftest(model, vcov = vcov_matrix)
  }
  data.frame(
    term = rownames(coefs),
    estimate = unname(coefs[, "Estimate"]),
    std_error = unname(coefs[, "Std. Error"]),
    stringsAsFactors = FALSE
  )
}

fixed_effects_coefficient_extractor <- function(model, data, vcov_fun) {
  slope_table <- default_coefficient_extractor(model, data, vcov_fun)
  intercept_val <- within_intercept(model)
  intercept_estimate <- if (length(intercept_val)) as.numeric(intercept_val[[1]]) else NA_real_
  intercept_se <- attr(intercept_val, "se")
  rbind(
    data.frame(
      term = "(Intercept)",
      estimate = intercept_estimate,
      std_error = if (length(intercept_se)) as.numeric(intercept_se[[1]]) else NA_real_,
      stringsAsFactors = FALSE
    ),
    slope_table
  )
}

align_linear_terms <- function(coef_table) {
  aligned <- lapply(names(LINEAR_TERM_MAP), function(term_key) {
    original_term <- LINEAR_TERM_MAP[[term_key]]
    row <- coef_table[coef_table$term == original_term, , drop = FALSE]
    estimate <- if (nrow(row) == 0) NA_real_ else row$estimate[[1]]
    std_error <- if (nrow(row) == 0) NA_real_ else row$std_error[[1]]
    data.frame(
      term = term_key,
      original_term = original_term,
      estimate = estimate,
      std_error = std_error,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, aligned)
}

resample_linear_data <- function(data, cluster_column = NULL) {
  if (!is.null(cluster_column) && cluster_column %in% names(data)) {
    clusters <- unique(data[[cluster_column]])
    clusters <- clusters[!is.na(clusters)]
    if (!length(clusters)) {
      return(data[sample.int(nrow(data), size = nrow(data), replace = TRUE), , drop = FALSE])
    }
    sampled_clusters <- sample(clusters, size = length(clusters), replace = TRUE)
    indices <- unlist(lapply(sampled_clusters, function(cluster_value) {
      which(data[[cluster_column]] == cluster_value)
    }), use.names = FALSE)
    return(data[indices, , drop = FALSE])
  }
  data[sample.int(nrow(data), size = nrow(data), replace = TRUE), , drop = FALSE]
}

quantile_interval <- function(values, conf_level) {
  finite_values <- values[is.finite(values)]
  if (length(finite_values) < 2) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  alpha <- (1 - conf_level) / 2
  qs <- quantile(finite_values, probs = c(alpha, 1 - alpha), names = FALSE, type = 7)
  c(lower = qs[[1]], upper = qs[[2]])
}

compute_bootstrap_intervals <- function(data, spec, aligned_terms, replicates, conf_level, verbosity) {
  if (!isTRUE(spec$supports_bootstrap) || replicates < 1) {
    empty_ci <- lapply(seq_len(nrow(aligned_terms)), function(i) c(lower = NA_real_, upper = NA_real_))
    names(empty_ci) <- aligned_terms$term
    return(empty_ci)
  }

  stats_matrix <- matrix(NA_real_, nrow = nrow(aligned_terms), ncol = replicates)
  rownames(stats_matrix) <- aligned_terms$term

  successful <- 0L
  for (i in seq_len(replicates)) {
    boot_data <- resample_linear_data(data, spec$bootstrap_cluster)
    try_result <- tryCatch({
      model <- spec$fit(boot_data)
      coef_table <- spec$coefficient_extractor(model, boot_data)
      aligned_boot <- align_linear_terms(coef_table)
      stats_matrix[, i] <- aligned_boot$estimate
      successful <<- successful + 1L
      NULL
    }, error = function(err) {
      if (verbosity >= 4) {
        cli_alert_warning(paste0("Bootstrap replication ", i, " for ", spec$label, " failed: ", err$message))
      }
      err
    })
    if (inherits(try_result, "error")) {
      next
    }
  }

  if (successful == 0L && verbosity >= 2) {
    cli_alert_warning("No successful bootstrap replications for {spec$label}. Confidence intervals will be NA.")
  }

  intervals <- lapply(seq_len(nrow(stats_matrix)), function(row_index) {
    quantile_interval(stats_matrix[row_index, ], conf_level)
  })
  names(intervals) <- aligned_terms$term
  intervals
}

build_linear_spec <- function(
    label,
    required_columns,
    numeric_columns,
    fit_fun,
    vcov_fun = NULL,
    coefficient_extractor = NULL,
    supports_bootstrap = TRUE,
    bootstrap_cluster = "study_id",
    weight_column = NULL) {
  force(label)
  force(required_columns)
  force(numeric_columns)
  force(fit_fun)
  force(vcov_fun)
  force(coefficient_extractor)
  force(supports_bootstrap)
  force(bootstrap_cluster)
  force(weight_column)

  extractor <- coefficient_extractor
  if (is.null(extractor)) {
    extractor <- function(model, data) {
      default_coefficient_extractor(model, data, vcov_fun)
    }
  }

  list(
    label = label,
    required_columns = required_columns,
    numeric_columns = numeric_columns,
    fit = fit_fun,
    vcov_fun = vcov_fun,
    coefficient_extractor = extractor,
    supports_bootstrap = supports_bootstrap,
    bootstrap_cluster = bootstrap_cluster,
    weight_column = weight_column
  )
}

linear_model_specs <- function() {
  specs <- list(
    ols = build_linear_spec(
      label = "OLS",
      required_columns = c("effect", "se", "study_id"),
      numeric_columns = c("effect", "se"),
      fit_fun = function(data) stats::lm(effect ~ se, data = data),
      vcov_fun = function(model, data) vcovHC(model, type = "HC0", cluster = data$study_id)
    ),
    fixed_effects = build_linear_spec(
      label = "Fixed Effects",
      required_columns = c("effect", "se", "study_id"),
      numeric_columns = c("effect", "se"),
      fit_fun = function(data) plm(effect ~ se, data = data, model = "within", index = "study_id"),
      vcov_fun = function(model, data) vcovHC(model, type = "HC0", method = "arellano", cluster = "group"),
      coefficient_extractor = function(model, data) {
        fixed_effects_coefficient_extractor(model, data, function(mdl, dt) vcovHC(mdl, type = "HC0", method = "arellano", cluster = "group"))
      }
    ),
    between_effects = build_linear_spec(
      label = "Between Effects",
      required_columns = c("effect", "se", "study_id"),
      numeric_columns = c("effect", "se"),
      fit_fun = function(data) plm(effect ~ se, data = data, model = "between", index = "study_id"),
      vcov_fun = function(model, data) vcovHC(model, type = "HC0", method = "arellano", cluster = "group"),
      supports_bootstrap = FALSE
    ),
    random_effects = build_linear_spec(
      label = "Random Effects",
      required_columns = c("effect", "se", "study_id"),
      numeric_columns = c("effect", "se"),
      fit_fun = function(data) plm(effect ~ se, data = data, model = "random", index = "study_id"),
      vcov_fun = function(model, data) vcovHC(model, type = "HC0", method = "arellano", cluster = "group")
    ),
    study_weighted_ols = build_linear_spec(
      label = "Study Weighted OLS",
      required_columns = c("effect", "se", "study_id", "study_size"),
      numeric_columns = c("effect", "se", "study_size"),
      fit_fun = function(data) stats::lm(effect ~ se, data = data, weights = study_size^2),
      vcov_fun = function(model, data) vcovHC(model, type = "HC0", cluster = data$study_id),
      weight_column = "study_size"
    ),
    precision_weighted_ols = build_linear_spec(
      label = "Precision Weighted OLS",
      required_columns = c("effect", "se", "study_id", "precision"),
      numeric_columns = c("effect", "se", "precision"),
      fit_fun = function(data) stats::lm(effect ~ se, data = data, weights = precision^2),
      vcov_fun = function(model, data) vcovHC(model, type = "HC0", cluster = data$study_id),
      weight_column = "precision"
    )
  )

  for (name in names(specs)) {
    specs[[name]]$name <- name
  }
  specs
}

prepare_linear_model_data <- function(df, specs, verbosity = get_verbosity()) {
  validate(is.data.frame(df))

  required_cols <- unique(unlist(lapply(specs, function(spec) spec$required_columns)))
  validate_columns(df, required_cols)

  numeric_cols <- unique(unlist(lapply(specs, function(spec) spec$numeric_columns)))
  numeric_cols <- numeric_cols[numeric_cols %in% names(df)]

  for (col in numeric_cols) {
    validate(is.numeric(df[[col]]))
  }

  filtered <- df
  if (length(numeric_cols)) {
    finite_mask <- Reduce(`&`, lapply(numeric_cols, function(col) is.finite(filtered[[col]])))
    if (any(!finite_mask)) {
      removed <- sum(!finite_mask)
      filtered <- filtered[finite_mask, , drop = FALSE]
      if (verbosity >= 2) {
        cli_alert_warning("Removed {removed} observations with non-finite numeric values required by linear tests.")
      }
    }
  }

  if ("study_id" %in% required_cols) {
    valid_ids <- !is.na(filtered$study_id)
    if (any(!valid_ids)) {
      removed <- sum(!valid_ids)
      filtered <- filtered[valid_ids, , drop = FALSE]
      if (verbosity >= 2) {
        cli_alert_warning("Removed {removed} observations with missing study identifiers.")
      }
    }
  }

  weight_columns <- unique(unlist(lapply(specs, function(spec) spec$weight_column)))
  weight_columns <- weight_columns[weight_columns %in% names(filtered)]
  for (weight_col in weight_columns) {
    valid_weights <- is.finite(filtered[[weight_col]]) & filtered[[weight_col]] > 0
    if (any(!valid_weights)) {
      removed <- sum(!valid_weights)
      filtered <- filtered[valid_weights, , drop = FALSE]
      if (verbosity >= 2) {
        cli_alert_warning("Removed {removed} observations with non-positive weights in column {weight_col}.")
      }
    }
  }

  assert(nrow(filtered) >= 2, "At least two observations with valid data are required to run linear tests.")

  filtered
}

empty_linear_result <- function(spec, n_obs, conf_level, bootstrap_replications, add_marks, round_to) {
  data.frame(
    method_id = rep(spec$name, length(LINEAR_TERM_MAP)),
    method = rep(spec$label, length(LINEAR_TERM_MAP)),
    term = names(LINEAR_TERM_MAP),
    term_label = unname(LINEAR_TERM_LABELS[names(LINEAR_TERM_MAP)]),
    estimate = NA_real_,
    std_error = NA_real_,
    t_statistic = NA_real_,
    p_value = NA_real_,
    significance_mark = if (add_marks) "" else NA_character_,
    formatted_estimate = NA_character_,
    formatted_std_error = NA_character_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    formatted_ci = NA_character_,
    n_obs = n_obs,
    conf_level = conf_level,
    bootstrap_replications = bootstrap_replications,
    used_bootstrap = FALSE,
    round_to = round_to,
    stringsAsFactors = FALSE
  )
}

run_single_linear_model <- function(
    data,
    spec,
    conf_level,
    bootstrap_replications,
    add_significance_marks,
    round_to,
    significance_levels,
    verbosity,
    emit_progress) {
  label <- spec$label
  if (emit_progress && verbosity >= 3) {
    cli_alert_info("Running {label}")
  }

  if (!is.null(spec$bootstrap_cluster) && spec$bootstrap_cluster %in% names(data)) {
    unique_clusters <- unique(data[[spec$bootstrap_cluster]])
    unique_clusters <- unique_clusters[!is.na(unique_clusters)]
    if (length(unique_clusters) < 2 && spec$name != "ols") {
      if (verbosity >= 2) {
        cli_alert_warning("Skipping {label}: requires at least two unique clusters.")
      }
      return(empty_linear_result(spec, nrow(data), conf_level, bootstrap_replications, add_significance_marks, round_to))
    }
  }

  result <- tryCatch({
    model <- spec$fit(data)
    coef_table <- spec$coefficient_extractor(model, data)
    aligned <- align_linear_terms(coef_table)

    t_stats <- aligned$estimate / aligned$std_error
    t_stats[!is.finite(t_stats)] <- NA_real_
    p_values <- 2 * (1 - pnorm(abs(t_stats)))

    marks <- if (add_significance_marks) {
      vapply(p_values, compute_significance_mark, character(1), levels = significance_levels)
    } else {
      rep(NA_character_, length(p_values))
    }

    ci_list <- compute_bootstrap_intervals(data, spec, aligned, bootstrap_replications, conf_level, verbosity)
    ci_matrix <- do.call(rbind, ci_list)
    if (is.null(ci_matrix)) {
      ci_matrix <- matrix(NA_real_, nrow = nrow(aligned), ncol = 2L)
      rownames(ci_matrix) <- aligned$term
    }

    formatted_ci <- mapply(
      format_ci_interval,
      lower = ci_matrix[, "lower"],
      upper = ci_matrix[, "upper"],
      MoreArgs = list(digits = round_to),
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )

    data.frame(
      method_id = spec$name,
      method = label,
      term = aligned$term,
      term_label = unname(LINEAR_TERM_LABELS[aligned$term]),
      estimate = aligned$estimate,
      std_error = aligned$std_error,
      t_statistic = t_stats,
      p_value = p_values,
      significance_mark = marks,
      formatted_estimate = mapply(format_estimate_with_mark, aligned$estimate, MoreArgs = list(digits = round_to), mark = marks, SIMPLIFY = TRUE),
      formatted_std_error = vapply(aligned$std_error, format_std_error, character(1), digits = round_to),
      ci_lower = ci_matrix[, "lower"],
      ci_upper = ci_matrix[, "upper"],
      formatted_ci = formatted_ci,
      n_obs = nrow(data),
      conf_level = conf_level,
      bootstrap_replications = bootstrap_replications,
      used_bootstrap = isTRUE(spec$supports_bootstrap) && bootstrap_replications > 0,
      round_to = round_to,
      stringsAsFactors = FALSE
    )
  }, error = function(err) {
    if (verbosity >= 2) {
      cli_alert_warning("Failed to run {label}: {err$message}")
    }
    empty_linear_result(spec, nrow(data), conf_level, bootstrap_replications, add_significance_marks, round_to)
  })

  result
}

run_linear_model_suite <- function(
    data,
    specs,
    conf_level,
    bootstrap_replications,
    add_significance_marks,
    round_to,
    significance_levels = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    verbosity = get_verbosity(),
    emit_progress = FALSE) {
  validate(is.data.frame(data))
  validate(is.list(specs))
  validate(is.numeric(conf_level), length(conf_level) == 1)
  validate(is.numeric(bootstrap_replications), length(bootstrap_replications) == 1)
  validate(is.logical(add_significance_marks), length(add_significance_marks) == 1)
  validate(is.numeric(round_to), length(round_to) == 1)

  assert(conf_level > 0 && conf_level < 1, "Confidence level must be between 0 and 1.")
  assert(bootstrap_replications >= 0, "Bootstrap replications must be greater than or equal to 0.")
  assert(round_to >= 0, "Number of decimals must be greater than or equal to 0.")

  bootstrap_replications <- as.integer(round(bootstrap_replications))

  results <- lapply(specs, function(spec) {
    run_single_linear_model(
      data = data,
      spec = spec,
      conf_level = conf_level,
      bootstrap_replications = bootstrap_replications,
      add_significance_marks = add_significance_marks,
      round_to = round_to,
      significance_levels = significance_levels,
      verbosity = verbosity,
      emit_progress = emit_progress
    )
  })

  output <- do.call(rbind, results)
  rownames(output) <- NULL
  output
}

box::export(
  LINEAR_TERM_LABELS,
  linear_model_specs,
  prepare_linear_model_data,
  run_linear_model_suite
)
