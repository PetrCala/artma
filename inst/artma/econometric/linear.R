#' @title Linear model helpers
#' @description Helper functions used by the linear testing method.
NULL

# Not referenced directly: model fitters (e.g. plm) evaluate model.frame in the
# formula environment, which is this module's environment.
box::use(
  stats[model.frame]
)

box::use(
  artma / libs / formatting / results[
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
prepare_linear_data <- function(df, spec) {
  box::use(
    artma / libs / core / validation[validate]
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

#' @title Draw bootstrap row indices
#' @description Resample row indices with replacement at the cluster level, or
#'   plain rows when no cluster split is available.
#' @param n_rows *[integer]* Number of rows in the data being resampled.
#' @param cluster_splits *[list, optional]* Row indices split by cluster, as
#'   returned by `split(seq_len(n_rows), cluster)`. `NULL` resamples rows.
#' @return An integer vector of resampled row indices.
resample_cluster_rows <- function(n_rows, cluster_splits = NULL) {
  if (is.null(cluster_splits)) {
    return(sample.int(n_rows, n_rows, replace = TRUE))
  }
  sampled_clusters <- sample(names(cluster_splits), length(cluster_splits), replace = TRUE)
  unlist(cluster_splits[sampled_clusters], use.names = FALSE)
}

# Fast-path bootstrap estimators ---------------------------------------------
#
# The bootstrap only needs point estimates per replication, so specs may
# provide a `boot_estimate(data, rows)` function that computes them directly
# on the resampled rows, skipping model-object construction and vcov work.
# Each fast path must reproduce the estimates of the corresponding
# `spec$fit` + `spec$tidy` refit on `data[rows, ]` to near machine precision;
# this is asserted in tests. `data` is the prepared frame from
# `prepare_linear_data()` (finite columns, factor cluster ids).

boot_estimate_ols <- function(data, rows) {
  fit <- stats::lm.fit(cbind(1, data$se[rows]), data$effect[rows])
  c(effect = fit$coefficients[[1L]], publication_bias = fit$coefficients[[2L]])
}

make_boot_estimate_weighted_ols <- function(weight_column) {
  force(weight_column)
  function(data, rows) {
    weights <- data[[weight_column]][rows]^2
    fit <- stats::lm.wfit(cbind(1, data$se[rows]), data$effect[rows], weights)
    c(effect = fit$coefficients[[1L]], publication_bias = fit$coefficients[[2L]])
  }
}

# Within (fixed effects) estimator. Duplicated resampled clusters keep their
# original id, so they merge into one group during demeaning, exactly as plm
# treats the resampled frame. The overall intercept is what
# plm::within_intercept() returns: mean(y) - slope * mean(x).
boot_estimate_within <- function(data, rows) {
  y <- data$effect[rows]
  x <- data$se[rows]
  group <- data$study_id[rows]
  y_demeaned <- y - stats::ave(y, group)
  x_demeaned <- x - stats::ave(x, group)
  slope <- stats::.lm.fit(cbind(x_demeaned), y_demeaned)$coefficients[[1L]]
  c(effect = mean(y) - slope * mean(x), publication_bias = slope)
}

# Random effects estimator: feasible GLS via the theta (quasi-demeaning)
# transformation with Swamy-Arora variance components, replicating
# plm::plm(model = "random") exactly. With one regressor plus intercept,
# plm's ercomp (method "swar", effect "individual") reduces to:
#   sigma_nu^2  = within SSR / (O - N - 1)
#   sigma_eta^2 = (sum_i T_i e_Bi^2 - (N - 2) sigma_nu^2) / (O - tr(A^-1 B))
# where O is the observation count, N the group count, e_Bi the residuals of
# the T_i-weighted between regression, A = sum_i T_i m_i m_i' and
# B = sum_i T_i^2 m_i m_i' with m_i = (1, xbar_i). plm's balanced-panel
# branch (dfcor = c(2, 2)) is the algebraic special case of these formulas,
# so no separate branch is needed. Negative components clamp to zero, then
# theta_i = 1 - sqrt(sigma_nu^2 / (T_i sigma_eta^2 + sigma_nu^2)) and OLS on
# the quasi-demeaned data yields the GLS estimates.
#
# The guards mirror the conditions under which plm errors, so degenerate
# resamples fail identically on both paths: plm's swar_Between_check rejects
# fewer than three distinct groups, and its within fit rejects a regressor
# with no within-group variation ("empty model").
boot_estimate_re <- function(data, rows) {
  y <- data$effect[rows]
  x <- data$se[rows]
  group <- droplevels(data$study_id[rows])

  n_obs <- length(y)
  n_groups <- nlevels(group)
  if (n_groups < 3L) {
    cli::cli_abort("Swamy-Arora needs at least three distinct clusters")
  }
  if (n_obs - n_groups < 2L) {
    cli::cli_abort("Not enough within-group observations for Swamy-Arora")
  }

  y_mean <- stats::ave(y, group)
  x_mean <- stats::ave(x, group)
  y_demeaned <- y - y_mean
  x_demeaned <- x - x_mean
  if (sum(x_demeaned^2) == 0) {
    cli::cli_abort("No within-group variation in se")
  }

  within_slope <- stats::.lm.fit(cbind(x_demeaned), y_demeaned)$coefficients[[1L]]
  within_ssr <- sum((y_demeaned - within_slope * x_demeaned)^2)
  idios_var <- within_ssr / (n_obs - n_groups - 1)

  group_codes <- as.integer(group)
  sizes_by_obs <- tabulate(group_codes, nbins = n_groups)[group_codes]
  first_in_group <- !duplicated(group_codes)
  group_sizes <- sizes_by_obs[first_in_group]
  group_x <- x_mean[first_in_group]
  group_y <- y_mean[first_in_group]

  between_fit <- stats::lm.wfit(cbind(1, group_x), group_y, group_sizes)
  between_quad <- sum(group_sizes * between_fit$residuals^2)

  m <- cbind(1, group_x)
  a_mat <- crossprod(m * sqrt(group_sizes))
  b_mat <- crossprod(m * group_sizes)
  id_eta_weight <- n_obs - sum(diag(solve(a_mat, b_mat)))
  id_var <- (between_quad - (n_groups - 2) * idios_var) / id_eta_weight

  sigma2 <- c(idios_var, id_var)
  sigma2[sigma2 < 0] <- 0

  theta <- 1 - sqrt(sigma2[[1L]] / (sizes_by_obs * sigma2[[2L]] + sigma2[[1L]]))
  fit <- stats::lm.fit(
    cbind(1 - theta, x - theta * x_mean),
    y - theta * y_mean
  )
  c(effect = fit$coefficients[[1L]], publication_bias = fit$coefficients[[2L]])
}

#' @title Compute bootstrap confidence intervals
#' @description Only point estimates are needed per replication. Specs with a
#'   `boot_estimate` fast path compute them directly on the resampled rows;
#'   the rest refit via `spec$fit` and extract coefficients with the cheap
#'   `boot_coefs` extractor instead of the full `tidy` path. The data is
#'   trimmed to the columns the fit actually touches before resampling.
#' @param spec *[list]* Linear model specification.
#' @param data *[data.frame]* Prepared dataset.
#' @param replications *[integer]* Number of bootstrap replications.
#' @param conf_level *[numeric]* Confidence level for the interval.
#' @return A matrix with rows equal to coefficient terms.
bootstrap_confidence <- function(spec, data, replications, conf_level) {
  if (replications <= 0) {
    return(matrix(NA_real_, nrow = 0, ncol = 2))
  }

  keep_columns <- intersect(
    unique(c(spec$required_columns, spec$weight_column, spec$cluster_column)),
    colnames(data)
  )
  data <- data[, keep_columns, drop = FALSE]

  samples <- matrix(NA_real_, nrow = replications, ncol = length(spec$terms))
  colnames(samples) <- spec$terms

  cluster_column <- spec$cluster_column
  cluster_splits <- if (!is.null(cluster_column) && cluster_column %in% colnames(data)) {
    split(seq_len(nrow(data)), data[[cluster_column]])
  }
  boot_estimate <- spec$boot_estimate

  for (i in seq_len(replications)) {
    rows <- resample_cluster_rows(nrow(data), cluster_splits)
    if (!is.null(boot_estimate)) {
      estimates <- tryCatch(boot_estimate(data, rows), error = function(e) NULL)
      if (is.null(estimates)) next
      samples[i, names(estimates)] <- estimates
    } else {
      boot_data <- data[rows, , drop = FALSE]
      fit <- tryCatch(spec$fit(boot_data), error = function(e) NULL)
      if (is.null(fit)) next
      coefs <- tryCatch(spec$boot_coefs(fit), error = function(e) NULL)
      if (is.null(coefs)) next
      coefs <- coefs[intersect(names(coefs), spec$terms)]
      samples[i, names(coefs)] <- coefs
    }
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

#' Extract bootstrap point estimates from an intercept + `se` slope model.
#'
#' Cheap counterpart of the `tidy` functions for use inside the bootstrap:
#' no clustered vcov, no coeftest, just the coefficients.
#'
#' @param model A fitted `lm` or `plm` model with an explicit intercept.
#' @return A named numeric vector with `effect` and `publication_bias`.
boot_coefs_intercept_slope <- function(model) {
  coefs <- stats::coef(model)
  c(
    effect = unname(coefs["(Intercept)"]),
    publication_bias = unname(coefs["se"])
  )
}

#' Extract bootstrap point estimates from a within (FE) plm model.
#'
#' `plm::within_intercept` is called without a custom vcov: the default is far
#' cheaper than `plm::vcovHC` and only the point estimate is used.
#'
#' @param model A fitted `plm` model with `model = "within"`.
#' @return A named numeric vector with `effect` and `publication_bias`.
boot_coefs_within <- function(model) {
  intercept <- tryCatch(
    plm::within_intercept(model),
    error = function(e) NA_real_
  )
  c(
    effect = if (is.numeric(intercept)) intercept[[1]] else NA_real_,
    publication_bias = unname(stats::coef(model)["se"])
  )
}

# Spec prechecks -------------------------------------------------------------
#
# Each panel spec carries a `precheck(prepared)` function evaluated after
# `prepare_linear_data()`. It returns NULL when the prepared data can support
# the estimator, or a plain-language skip reason. This turns raw plm errors
# (e.g. "empty model", "model not estimable") into actionable messages; the
# tryCatch around `spec$fit` remains as a fallback for anything missed here.

has_within_cluster_variation <- function(data, value_column, cluster_column) {
  groups <- split(data[[value_column]], data[[cluster_column]])
  any(vapply(groups, function(x) length(unique(x)) > 1L, logical(1)))
}

precheck_fe <- function(prepared) {
  if (!has_within_cluster_variation(prepared$data, "se", "study_id")) {
    return(cli::format_inline(
      "Cannot fit {.emph Fixed Effects}: {.field se} does not vary within any {.field study_id} cluster"
    ))
  }
  NULL
}

precheck_be <- function(prepared) {
  if (prepared$n_clusters < 3L) {
    return(cli::format_inline(
      "Not enough clusters to fit {.emph Between Effects}: at least 3 distinct {.field study_id} values are needed, found {prepared$n_clusters}"
    ))
  }
  NULL
}

precheck_re <- function(prepared) {
  if (prepared$n_clusters < 3L) {
    return(cli::format_inline(
      "Not enough clusters to fit {.emph Random Effects}: at least 3 distinct {.field study_id} values are needed, found {prepared$n_clusters}"
    ))
  }
  if (prepared$n_obs <= prepared$n_clusters + 1L) {
    return(cli::format_inline(
      "Not enough observations to fit {.emph Random Effects}: more than {prepared$n_clusters + 1L} rows are needed with {prepared$n_clusters} {.field study_id} clusters, found {prepared$n_obs}"
    ))
  }
  if (!has_within_cluster_variation(prepared$data, "se", "study_id")) {
    return(cli::format_inline(
      "Cannot fit {.emph Random Effects}: {.field se} does not vary within any {.field study_id} cluster"
    ))
  }
  NULL
}

linear_model_specs <- function() {
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
      boot_coefs = boot_coefs_intercept_slope,
      boot_estimate = boot_estimate_ols,
      supports_bootstrap = TRUE
    ),
    list(
      name = "fe",
      label = "Fixed Effects",
      required_columns = c("effect", "se", "study_id"),
      numeric_columns = c("effect", "se"),
      cluster_column = "study_id",
      terms = c("effect", "publication_bias"),
      required_packages = "plm",
      precheck = precheck_fe,
      fit = function(df) plm::plm(effect ~ se, data = df, model = "within", index = "study_id"),
      tidy = tidy_plm_within,
      boot_coefs = boot_coefs_within,
      boot_estimate = boot_estimate_within,
      supports_bootstrap = TRUE
    ),
    list(
      name = "be",
      label = "Between Effects",
      required_columns = c("effect", "se", "study_id"),
      numeric_columns = c("effect", "se"),
      cluster_column = "study_id",
      terms = c("effect", "publication_bias"),
      required_packages = "plm",
      precheck = precheck_be,
      fit = function(df) plm::plm(effect ~ se, data = df, model = "between", index = "study_id"),
      tidy = tidy_plm_generic,
      boot_coefs = boot_coefs_intercept_slope,
      supports_bootstrap = FALSE
    ),
    list(
      name = "re",
      label = "Random Effects",
      required_columns = c("effect", "se", "study_id"),
      numeric_columns = c("effect", "se"),
      cluster_column = "study_id",
      terms = c("effect", "publication_bias"),
      required_packages = "plm",
      precheck = precheck_re,
      fit = function(df) plm::plm(effect ~ se, data = df, model = "random", index = "study_id"),
      tidy = tidy_plm_generic,
      boot_coefs = boot_coefs_intercept_slope,
      boot_estimate = boot_estimate_re,
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
      boot_coefs = boot_coefs_intercept_slope,
      boot_estimate = make_boot_estimate_weighted_ols("study_size"),
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
      boot_coefs = boot_coefs_intercept_slope,
      boot_estimate = make_boot_estimate_weighted_ols("precision"),
      supports_bootstrap = TRUE
    )
  )
}

#' @title Run linear model specifications
#' @param df *[data.frame]* Input dataset.
#' @param options *[list]* Options controlling formatting and bootstrap.
#' @param is_pkg_available *[function, optional]* Predicate that reports whether
#'   a package namespace is available. Defaults to `requireNamespace`; injectable
#'   so tests can simulate a missing package.
#' @return A list containing the coefficients, formatted summary, and skipped models.
run_linear_models <- function(df, options, is_pkg_available = NULL) {
  if (is.null(is_pkg_available)) {
    is_pkg_available <- function(pkg) requireNamespace(pkg, quietly = TRUE)
  }

  specs <- linear_model_specs()
  results <- list()
  skipped <- list()

  for (spec in specs) {
    required_pkgs <- spec$required_packages %||% character()
    missing_pkgs <- required_pkgs[!vapply(required_pkgs, is_pkg_available, logical(1))]
    if (length(missing_pkgs) > 0) {
      install_hint <- paste0("install.packages(", deparse(missing_pkgs), ")")
      skipped[[spec$name]] <- list(
        label = spec$label,
        reason = cli::format_inline(
          "Package {.pkg {missing_pkgs}} is required for {.emph {spec$label}}. Install with: {install_hint}"
        )
      )
      next
    }

    prepared <- prepare_linear_data(df, spec)
    if (prepared$skipped) {
      skipped[[spec$name]] <- list(label = spec$label, reason = prepared$reason)
      next
    }

    if (!is.null(spec$precheck)) {
      precheck_reason <- spec$precheck(prepared)
      if (!is.null(precheck_reason)) {
        skipped[[spec$name]] <- list(label = spec$label, reason = precheck_reason)
        next
      }
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

  coefficients$significance <- if (options$add_significance_marks) significance_mark(coefficients$p_value) else ""
  coefficients$estimate_rounded <- round(coefficients$estimate, options$round_to)
  coefficients$std_error_rounded <- round(coefficients$std_error, options$round_to)
  coefficients$estimate_formatted <- paste0(format_number(coefficients$estimate, options$round_to), coefficients$significance)
  coefficients$std_error_formatted <- format_se(coefficients$std_error, options$round_to)
  coefficients$bootstrap_formatted <- format_ci(
    coefficients$bootstrap_lower,
    coefficients$bootstrap_upper,
    digits = options$round_to
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
  run_linear_models,
  linear_model_specs,
  resample_cluster_rows
)

# nocov end -----------------------------------------------------------------
