#' @title BMA Inclusion Plot
#' @description
#' A ggplot2 replacement for `BMS:::image.bma`, the grid showing which
#' regressors enter the best models and with which sign.
#'
#' The upstream function is not usable as-is. It lays its columns out on
#' `cumsum()` of the *unnormalised* posterior model weights, which BMS returns
#' spanning tens of orders of magnitude (1e41 down to 1e-6 is ordinary). Past
#' roughly sixty models that cumulative sum saturates in double precision and
#' stops strictly increasing, at which point `graphics::image()` rejects it with
#' "increasing 'x' and 'y' values expected". A nine-regressor model with the
#' default 500 retained models is enough to trigger it, so the existing
#' "at least 4 regressors" guard does not help and the caller's `tryCatch`
#' turns the failure into a warning and no plot.
#'
#' Normalising the weights before accumulating removes the failure, and
#' truncating to the models that carry the posterior mass makes the result
#' legible: 500 columns across a page is not readable whether or not it renders.

#' Assemble the plotting data for a BMA inclusion grid
#'
#' @description
#' Returns one row per (regressor, model) cell, with the model's horizontal
#' extent given by its share of posterior mass.
#'
#' @param bma_model *\[bma\]* A fitted BMS model
#' @param max_models *\[integer\]* Hard cap on the number of models shown
#' @param coverage *\[numeric\]* Stop once the retained models carry this share
#'   of the posterior mass across the retained set
#'
#' @return *\[list\]* With `cells`, `n_models_shown`, `n_models_total` and
#'   `mass_shown`, or `NULL` when the model carries nothing to draw
#' @keywords internal
build_bma_inclusion_data <- function(bma_model, max_models = 40L, coverage = 0.99) {
  box::use(artma / libs / core / validation[validate])

  validate(
    inherits(bma_model, "bma"),
    is.numeric(max_models), max_models > 0,
    is.numeric(coverage), coverage > 0, coverage <= 1
  )

  betas <- bma_model$topmod$betas()
  reg_names <- bma_model$reg.names

  if (is.null(betas) || !length(betas) || !length(reg_names)) {
    return(NULL)
  }
  betas <- as.matrix(betas)
  if (nrow(betas) != length(reg_names) || ncol(betas) < 1) {
    return(NULL)
  }

  weights <- bma_model$topmod$lik()
  # `lik()` returns log marginal likelihoods (plus the log model prior). Shifting
  # by the maximum before exponentiating is what keeps the weights finite: the
  # raw likelihoods overflow, which is the same span that breaks the upstream
  # cumulative layout.
  weights <- exp(weights - max(weights, na.rm = TRUE))
  weights[!is.finite(weights)] <- 0
  total <- sum(weights)
  if (!is.finite(total) || total <= 0) {
    return(NULL)
  }
  weights <- weights / total

  order_by_weight <- order(weights, decreasing = TRUE)
  betas <- betas[, order_by_weight, drop = FALSE]
  weights <- weights[order_by_weight]

  n_models_total <- length(weights)
  keep_n <- min(
    as.integer(max_models),
    max(1L, which(cumsum(weights) >= coverage)[1] %||% n_models_total),
    n_models_total
  )

  betas <- betas[, seq_len(keep_n), drop = FALSE]
  weights <- weights[seq_len(keep_n)]
  mass_shown <- sum(weights)

  # Renormalised over the retained models so the grid spans the full width.
  widths <- weights / sum(weights)
  right <- cumsum(widths)
  left <- right - widths

  pip_order <- resolve_pip_order(bma_model, reg_names)
  betas <- betas[pip_order, , drop = FALSE]
  ordered_names <- reg_names[pip_order]

  n_regressors <- length(ordered_names)
  sign_of <- function(value) {
    if (!is.finite(value) || value == 0) {
      "Excluded"
    } else if (value > 0) {
      "Positive"
    } else {
      "Negative"
    }
  }

  cells <- expand.grid(
    row = seq_len(n_regressors),
    model = seq_len(keep_n),
    KEEP.OUT.ATTRS = FALSE
  )
  cells$xmin <- left[cells$model]
  cells$xmax <- right[cells$model]
  # Highest PIP at the top: row 1 occupies the topmost band.
  cells$ymin <- n_regressors - cells$row
  cells$ymax <- cells$ymin + 1
  cells$inclusion <- vapply(
    seq_len(nrow(cells)),
    function(i) sign_of(betas[cells$row[i], cells$model[i]]),
    character(1)
  )

  list(
    cells = cells,
    regressors = ordered_names,
    n_models_shown = keep_n,
    n_models_total = n_models_total,
    mass_shown = mass_shown
  )
}


#' Order regressors by posterior inclusion probability, descending
#'
#' @param bma_model *\[bma\]* A fitted BMS model
#' @param reg_names *\[character\]* Regressor names in model order
#' @return *\[integer\]* Row indices into the beta matrix
#' @keywords internal
resolve_pip_order <- function(bma_model, reg_names) {
  coefficients <- tryCatch(
    stats::coef(bma_model, order.by.pip = FALSE, exact = TRUE, include.constant = FALSE),
    error = function(e) NULL
  )

  if (is.null(coefficients) || !"PIP" %in% colnames(coefficients)) {
    return(seq_along(reg_names))
  }

  pip <- coefficients[, "PIP"]
  if (length(pip) != length(reg_names)) {
    return(seq_along(reg_names))
  }

  order(pip, decreasing = TRUE)
}


#' Build the BMA inclusion plot
#'
#' @param bma_model *\[bma\]* A fitted BMS model
#' @param theme_name *\[character\]* Color theme name
#' @param max_models *\[integer\]* Hard cap on the number of models shown
#'
#' @return *\[ggplot or NULL\]* The plot, or `NULL` when there is nothing to draw
#' @keywords internal
create_bma_inclusion_plot <- function(bma_model, theme_name, max_models = 40L) {
  box::use(
    artma / libs / core / log[log_info],
    artma / visualization / colors[get_tokens, get_neutral],
    artma / visualization / theme[get_theme]
  )

  plot_data <- build_bma_inclusion_data(bma_model, max_models = max_models)
  if (is.null(plot_data)) {
    return(NULL)
  }

  tokens <- get_tokens(theme_name)
  neutral <- get_neutral()

  cells <- plot_data$cells
  regressors <- plot_data$regressors
  n_regressors <- length(regressors)

  fill_values <- c(
    "Positive" = tokens$accent,
    "Negative" = tokens$contrast,
    "Excluded" = neutral$surface_alt
  )

  # Truncation is reported to the console rather than captioned on the figure.
  # It still must not pass in silence: the grid shows a fraction of the
  # retained models, and nothing in the image itself says so.
  if (plot_data$n_models_shown < plot_data$n_models_total) {
    log_info(paste0(
      "BMA inclusion plot: showing ", plot_data$n_models_shown, " of ",
      plot_data$n_models_total, " retained models, carrying ",
      sprintf("%.1f%%", 100 * plot_data$mass_shown), " of their posterior mass."
    ))
  }

  ggplot2::ggplot(cells) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax,
        fill = .data$inclusion
      ),
      colour = "white",
      linewidth = 0.2
    ) +
    ggplot2::scale_fill_manual(
      values = fill_values,
      breaks = c("Positive", "Negative", "Excluded"),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_len(n_regressors) - 0.5,
      labels = rev(regressors),
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(round(100 * x), "%"),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = "Cumulative posterior model probability",
      y = NULL
    ) +
    get_theme(theme_name) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}


box::export(build_bma_inclusion_data, create_bma_inclusion_plot)
