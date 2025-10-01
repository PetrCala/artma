#' @title Linear model helpers
#' @description Helper functions used by the linear testing method.
NULL

box::use(
  stats[model.frame]
)

box::use(
  artma / libs / result_formatters[
    significance_mark,
    format_number,
    format_se,
    format_ci
  ]
)

# nocov start: glue/cli heavy formatting helpers ---------------------------

#' Prepare the dataset for a linear model specification.
#'
#' @param df *[data.frame]* Input data.
#' @param spec *[list]* Linear model specification.
#' @return A list containing the filtered data frame, the number of
#'   observations kept and dropped, and the number of clusters.
prepare_linear_data <- function(df, spec) { # nolint: cyclocomp_linter.
  box::use(
    artma / libs / validation[validate]
  )

  validate(is.data.frame(df))

  required_cols <- spec$required_columns
  missing_cols <- setdiff(required_cols, colnames(df))

  if (length(missing_cols) > 0) {
    return(list(
      data = NULL,
      skipped = TRUE,
      reason = cli::format_inline(
        "Missing required columns for {.emph {spec$label}}: {.field {missing_cols}}"
      )
    ))
  }

  data <- df
  original_n <- nrow(data)

  drop_masks <- list()

  numeric_cols <- spec$numeric_columns %||% character()
  for (col in numeric_cols) {
    drop_masks[[col]] <- is.finite(data[[col]])
  }

  cluster_col <- spec$cluster_column
  if (!is.null(cluster_col)) {
    drop_masks[[cluster_col]] <- !is.na(data[[cluster_col]])
    data[[cluster_col]] <- droplevels(factor(data[[cluster_col]]))
  }

  if (!is.null(spec$weight_column)) {
    weight_col <- spec$weight_column
    drop_masks[[weight_col]] <- is.finite(data[[weight_col]]) & data[[weight_col]] > 0
  }

  if (length(drop_masks) > 0) {
    keep_mask <- Reduce(`&`, drop_masks)
    data <- data[keep_mask, , drop = FALSE]
  }

  kept_n <- nrow(data)

  if (kept_n < 2) {
    return(list(
      data = NULL,
      skipped = TRUE,
      reason = cli::format_inline(
        "Not enough observations to fit {.emph {spec$label}}"
      )
    ))
  }

  list(
    data = data,
    skipped = FALSE,
    dropped = original_n - kept_n,
    n_obs = kept_n,
    n_clusters = if (!is.null(cluster_col)) length(unique(data[[cluster_col]])) else NA_integer_
  )
}

#' @title Cluster bootstrap sample
#' @description Resample the data by clusters.
#' @param df *[data.frame]* Data to resample.
#' @param cluster_column *[character]* Column name holding the cluster ids.
#' @return A resampled data frame with replacement at the cluster level.
resample_by_cluster <- function(df, cluster_column) {
  if (is.null(cluster_column) || !cluster_column %in% colnames(df)) {
    rows <- sample.int(nrow(df), nrow(df), replace = TRUE)
    return(df[rows, , drop = FALSE])
  }

  cluster_ids <- df[[cluster_column]]
  cluster_splits <- split(seq_len(nrow(df)), cluster_ids)
  sampled_clusters <- sample(names(cluster_splits), length(cluster_splits), replace = TRUE)
  sampled_indices <- unlist(cluster_splits[sampled_clusters], use.names = FALSE)
  df[sampled_indices, , drop = FALSE]
}

#' @title Compute bootstrap confidence intervals
#' @param spec *[list]* Linear model specification.
#' @param data *[data.frame]* Prepared dataset.
#' @param replications *[integer]* Number of bootstrap replications.
#' @param conf_level *[numeric]* Confidence level for the interval.
#' @return A matrix with rows equal to coefficient terms.
bootstrap_confidence <- function(spec, data, replications, conf_level) {
  if (replications <= 0) {
    return(matrix(NA_real_, nrow = 0, ncol = 2))
  }

  samples <- matrix(NA_real_, nrow = replications, ncol = length(spec$terms))
  colnames(samples) <- spec$terms

  for (i in seq_len(replications)) {
    boot_data <- resample_by_cluster(data, spec$cluster_column)
    fit <- tryCatch(spec$fit(boot_data), error = function(e) NULL)
    if (is.null(fit)) next
    tidy <- tryCatch(spec$tidy(fit, boot_data), error = function(e) NULL)
    if (is.null(tidy)) next
    samples[i, tidy$term] <- tidy$estimate
  }

  ci <- matrix(NA_real_, nrow = length(spec$terms), ncol = 2, dimnames = list(spec$terms, c("lower", "upper")))
  alpha <- (1 - conf_level) / 2

  for (term in spec$terms) {
    term_samples <- samples[, term]
    term_samples <- term_samples[is.finite(term_samples)]
    if (length(term_samples) == 0) {
      next
    }
    ci[term, ] <- stats::quantile(term_samples, probs = c(alpha, 1 - alpha), names = FALSE, type = 7)
  }

  ci
}

tidy_from_coeftest <- function(coef_matrix) {
  stat_col <- intersect(c("t value", "z value"), colnames(coef_matrix))
  if (!length(stat_col)) {
    stat_col <- colnames(coef_matrix)[max(3, ncol(coef_matrix) - 1)]
  }
  stat_col <- stat_col[1]
  p_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), colnames(coef_matrix))
  if (!length(p_col)) {
    p_col <- colnames(coef_matrix)[ncol(coef_matrix)]
  }
  p_col <- p_col[1]

  data.frame(
    term_raw = rownames(coef_matrix),
    estimate = coef_matrix[, "Estimate"],
    std_error = coef_matrix[, "Std. Error"],
    statistic = coef_matrix[, stat_col],
    p_value = coef_matrix[, p_col],
    row.names = NULL,
    check.names = FALSE
  )
}

tidy_lm_model <- function(model, data, cluster_column) {
  vcov <- tryCatch(
    sandwich::vcovCL(model, cluster = data[[cluster_column]], type = "HC1"),
    error = function(e) sandwich::vcovHC(model, type = "HC1")
  )

  coef_matrix <- lmtest::coeftest(model, vcov. = vcov)
  tidy <- tidy_from_coeftest(coef_matrix)

  tidy$term <- c(
    `se` = "publication_bias",
    `(Intercept)` = "effect"
  )[tidy$term_raw]
  tidy <- tidy[!is.na(tidy$term), , drop = FALSE]
  tidy$term_raw <- NULL
  tidy
}

tidy_plm_generic <- function(model, data) {
  vcov <- tryCatch(
    plm::vcovHC(model, type = "HC1", cluster = "group"),
    error = function(e) {
      tryCatch(
        plm::vcovHC(model, type = "HC0"),
        error = function(e2) stats::vcov(model)
      )
    }
  )
  coef_matrix <- lmtest::coeftest(model, vcov = vcov)

  tidy <- tidy_from_coeftest(coef_matrix)

  tidy$term <- c(
    `se` = "publication_bias",
    `(Intercept)` = "effect"
  )[tidy$term_raw]
  tidy <- tidy[!is.na(tidy$term), , drop = FALSE]
  tidy$term_raw <- NULL
  tidy
}

tidy_plm_within <- function(model, data) {
  vcov <- tryCatch(
    plm::vcovHC(model, type = "HC1", cluster = "group"),
    error = function(e) plm::vcovHC(model, type = "HC0")
  )

  coef_matrix <- lmtest::coeftest(model, vcov = vcov)
  tidy <- tidy_from_coeftest(coef_matrix)
  slope <- tidy[tidy$term_raw == "se", , drop = FALSE]
  slope$term_raw <- NULL
  slope$term <- "publication_bias"

  intercept <- tryCatch(
    plm::within_intercept(model, vcov = function(x) vcov),
    error = function(e) NA_real_
  )

  intercept_est <- if (is.numeric(intercept)) intercept[[1]] else NA_real_
  intercept_se <- if (!is.null(attr(intercept, "se"))) attr(intercept, "se") else NA_real_
  intercept_stat <- if (is.finite(intercept_est) && is.finite(intercept_se) && intercept_se != 0) intercept_est / intercept_se else NA_real_
  intercept_p <- if (is.finite(intercept_stat)) 2 * stats::pnorm(abs(intercept_stat), lower.tail = FALSE) else NA_real_

  intercept_row <- data.frame(
    term = "effect",
    estimate = intercept_est,
    std_error = intercept_se,
    statistic = intercept_stat,
    p_value = intercept_p,
    check.names = FALSE
  )

  rbind(intercept_row, slope[c("term", "estimate", "std_error", "statistic", "p_value")])
}

linear_model_specs <- function() { # nolint: function_length_linter.
  list(
    list(
      name = "ols",
      label = "OLS",
      required_columns = c("effect", "se", "study_id"),
      numeric_columns = c("effect", "se"),
      cluster_column = "study_id",
      terms = c("effect", "publication_bias"),
      fit = function(df) stats::lm(effect ~ se, data = df),
      tidy = function(model, data) tidy_lm_model(model, data, "study_id"),
      supports_bootstrap = TRUE
    ),
    list(
      name = "fe",
      label = "Fixed Effects",
      required_columns = c("effect", "se", "study_id"),
      numeric_columns = c("effect", "se"),
      cluster_column = "study_id",
      terms = c("effect", "publication_bias"),
      fit = function(df) plm::plm(effect ~ se, data = df, model = "within", index = "study_id"),
      tidy = tidy_plm_within,
      supports_bootstrap = TRUE
    ),
    list(
      name = "be",
      label = "Between Effects",
      required_columns = c("effect", "se", "study_id"),
      numeric_columns = c("effect", "se"),
      cluster_column = "study_id",
      terms = c("effect", "publication_bias"),
      fit = function(df) plm::plm(effect ~ se, data = df, model = "between", index = "study_id"),
      tidy = tidy_plm_generic,
      supports_bootstrap = FALSE
    ),
    list(
      name = "re",
      label = "Random Effects",
      required_columns = c("effect", "se", "study_id"),
      numeric_columns = c("effect", "se"),
      cluster_column = "study_id",
      terms = c("effect", "publication_bias"),
      fit = function(df) plm::plm(effect ~ se, data = df, model = "random", index = "study_id"),
      tidy = tidy_plm_generic,
      supports_bootstrap = TRUE
    ),
    list(
      name = "ols_study_weighted",
      label = "Study Weighted OLS",
      required_columns = c("effect", "se", "study_id", "study_size"),
      numeric_columns = c("effect", "se", "study_size"),
      cluster_column = "study_id",
      weight_column = "study_size",
      terms = c("effect", "publication_bias"),
      fit = function(df) stats::lm(effect ~ se, data = df, weights = (df$study_size^2)),
      tidy = function(model, data) tidy_lm_model(model, data, "study_id"),
      supports_bootstrap = TRUE
    ),
    list(
      name = "ols_precision_weighted",
      label = "Precision Weighted OLS",
      required_columns = c("effect", "se", "study_id", "precision"),
      numeric_columns = c("effect", "se", "precision"),
      cluster_column = "study_id",
      weight_column = "precision",
      terms = c("effect", "publication_bias"),
      fit = function(df) stats::lm(effect ~ se, data = df, weights = (df$precision^2)),
      tidy = function(model, data) tidy_lm_model(model, data, "study_id"),
      supports_bootstrap = TRUE
    )
  )
}

#' @title Run linear model specifications
#' @param df *[data.frame]* Input dataset.
#' @param options *[list]* Options controlling formatting and bootstrap.
#' @return A list containing the coefficients, formatted summary, and skipped models.
run_linear_models <- function(df, options) { # nolint: function_length_linter.
  specs <- linear_model_specs()
  results <- list()
  skipped <- list()

  for (spec in specs) {
    prepared <- prepare_linear_data(df, spec)
    if (prepared$skipped) {
      skipped[[spec$name]] <- list(label = spec$label, reason = prepared$reason)
      next
    }

    model <- tryCatch(spec$fit(prepared$data), error = function(e) {
      skipped[[spec$name]] <<- list(label = spec$label, reason = e$message)
      NULL
    })

    if (is.null(model)) next

    tidy <- tryCatch(spec$tidy(model, prepared$data), error = function(e) {
      skipped[[spec$name]] <<- list(label = spec$label, reason = e$message)
      NULL
    })

    if (is.null(tidy) || nrow(tidy) == 0) {
      skipped[[spec$name]] <- list(label = spec$label, reason = "Unable to extract coefficients")
      next
    }

    tidy$model <- spec$name
    tidy$model_label <- spec$label
    tidy$n_obs <- prepared$n_obs
    tidy$term_label <- c(
      effect = "Effect Beyond Bias",
      publication_bias = "Publication Bias"
    )[tidy$term]

    tidy$bootstrap_lower <- NA_real_
    tidy$bootstrap_upper <- NA_real_

    if (isTRUE(spec$supports_bootstrap) && options$bootstrap_replications > 0) {
      boot_ci <- bootstrap_confidence(spec, prepared$data, options$bootstrap_replications, options$conf_level)
      tidy$bootstrap_lower <- boot_ci[tidy$term, "lower"]
      tidy$bootstrap_upper <- boot_ci[tidy$term, "upper"]
    }

    results[[length(results) + 1]] <- tidy
  }

  if (length(results) == 0) {
    empty <- data.frame(
      model = character(),
      model_label = character(),
      term = character(),
      term_label = character(),
      estimate = numeric(),
      std_error = numeric(),
      statistic = numeric(),
      p_value = numeric(),
      n_obs = integer(),
      bootstrap_lower = numeric(),
      bootstrap_upper = numeric(),
      stringsAsFactors = FALSE
    )
    return(list(coefficients = empty, summary = empty, skipped = skipped, options = options))
  }

  coefficients <- do.call(rbind, results)

  coefficients$significance <- if (options$add_significance_marks) vapply(coefficients$p_value, significance_mark, character(1)) else ""
  coefficients$estimate_rounded <- round(coefficients$estimate, options$round_to)
  coefficients$std_error_rounded <- round(coefficients$std_error, options$round_to)
  coefficients$estimate_formatted <- paste0(format_number(coefficients$estimate, options$round_to), coefficients$significance)
  coefficients$std_error_formatted <- vapply(coefficients$std_error, format_se, character(1), digits = options$round_to)
  coefficients$bootstrap_formatted <- mapply( # nolint: undesirable_function_linter.
    format_ci,
    coefficients$bootstrap_lower,
    coefficients$bootstrap_upper,
    MoreArgs = list(digits = options$round_to),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  summary <- build_summary_table(coefficients, options$round_to)

  list(
    coefficients = coefficients,
    summary = summary,
    skipped = skipped,
    options = options
  )
}

#' @title Build summary table
#' @param coefficients *[data.frame]* Tidy coefficient output.
#' @param digits *[integer]* Number of decimal places.
#' @return A formatted data frame summarising models.
build_summary_table <- function(coefficients, digits) {
  if (!nrow(coefficients)) {
    return(data.frame())
  }

  models <- unique(coefficients$model)
  row_labels <- c(
    "Publication Bias",
    "(Std. Error)",
    "Bootstrap CI (PB)",
    "Effect Beyond Bias",
    "(Std. Error)",
    "Bootstrap CI (Effect)",
    "Total Observations"
  )

  summary <- data.frame(
    Metric = row_labels,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (model in models) {
    model_rows <- coefficients[coefficients$model == model, , drop = FALSE]
    pb <- model_rows[model_rows$term == "publication_bias", , drop = FALSE]
    eff <- model_rows[model_rows$term == "effect", , drop = FALSE]

    n_obs_value <- unique(model_rows$n_obs)
    n_obs_value <- n_obs_value[!is.na(n_obs_value)]
    if (!length(n_obs_value)) {
      n_obs_value <- NA_real_
    } else {
      n_obs_value <- n_obs_value[1]
    }

    col_values <- c(
      if (nrow(pb)) pb$estimate_formatted else NA_character_,
      if (nrow(pb)) pb$std_error_formatted else NA_character_,
      if (nrow(pb)) pb$bootstrap_formatted else NA_character_,
      if (nrow(eff)) eff$estimate_formatted else NA_character_,
      if (nrow(eff)) eff$std_error_formatted else NA_character_,
      if (nrow(eff)) eff$bootstrap_formatted else NA_character_,
      if (nrow(model_rows)) format_number(n_obs_value, 0) else NA_character_
    )

    column_name <- model_rows$model_label[1]
    summary[[column_name]] <- col_values
  }

  attr(summary, "row.names") <- row_labels
  summary
}

box::export(
  run_linear_models
)

# nocov end -----------------------------------------------------------------
