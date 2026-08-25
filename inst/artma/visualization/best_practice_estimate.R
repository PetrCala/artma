#' @title Best-practice estimate plot builders
#' @description
#' Builds the sorted per-study best-practice-estimate scatter plot and the
#' per-factor density plots. The numeric core lives in
#' `econometric/best_practice_estimate.R`; the wrapper that assembles the
#' method result lives in `methods/best_practice_estimate.R`.
NULL

box::use(
  artma / econometric / best_practice_estimate[find_config_key_for_var, is_bpe_factor_var],
  artma / libs / core / grouping[resolve_variable_groups]
)

#' @title Build Best-Practice Estimate Plots
#' @description
#' Builds the sorted "miracle" scatter plot of per-study best-practice
#' estimates (with the author's own estimate highlighted) and, for every
#' variable flagged for BPE grouping in the data config, a per-factor density
#' plot of the per-study estimates split by factor level.
#' @keywords internal
build_bpe_plots <- function(study_rows, author_estimate, predictors, config, bma_data,
                            study_index_groups, round_to, theme_name) {
  plots <- list()

  study_estimates <- if (length(study_rows)) do.call(rbind, study_rows) else NULL

  min_points_for_scatter <- 4L
  if (!is.null(study_estimates) && nrow(study_estimates) >= min_points_for_scatter) {
    plots$bpe_scatter <- create_bpe_scatter_plot(
      study_estimates = study_estimates,
      author_estimate = author_estimate,
      theme_name = theme_name
    )
  }

  if (!is.null(study_estimates) && nrow(study_estimates) > 0) {
    density_plots <- build_bpe_density_plots(
      predictors = predictors,
      config = config,
      bma_data = bma_data,
      study_index_groups = study_index_groups,
      study_estimates = study_estimates,
      round_to = round_to,
      theme_name = theme_name
    )
    plots <- c(plots, density_plots)
  }

  plots
}

#' @title Create the Sorted BPE Scatter Plot
#' @description
#' Studies sorted by their best-practice estimate, a spline smoother through
#' the sorted points, and a dashed reference line at the author's own
#' best-practice estimate.
#' @keywords internal
create_bpe_scatter_plot <- function(study_estimates, author_estimate, theme_name) {
  box::use(
    artma / visualization / colors[get_tokens, get_vline_color, get_neutral],
    artma / visualization / theme[get_theme]
  )

  has_author_reference <- is.finite(author_estimate)

  plot_df <- study_estimates[order(study_estimates$estimate), , drop = FALSE]
  plot_df$rank <- seq_len(nrow(plot_df))
  plot_df$relative_to_author <- "Study"
  if (has_author_reference) {
    plot_df$relative_to_author <- "Below author's BPE"
    plot_df$relative_to_author[
      is.finite(plot_df$estimate) & plot_df$estimate >= author_estimate
    ] <- "At or above author's BPE"
  }

  tokens <- get_tokens(theme_name)
  vline_color <- get_vline_color(theme_name)
  neutral <- get_neutral()
  plot_theme <- get_theme(theme_name)

  # A diverging pair, not two arbitrary neighbours out of a qualitative ramp:
  # the split is "below the author's estimate" against "at or above it", so the
  # two sides should read as opposites. The author's own reference line takes a
  # third hue, since it is a reference rather than one of the categories.
  point_colors <- c(
    "Study" = tokens$accent,
    "Below author's BPE" = tokens$highlight,
    "At or above author's BPE" = tokens$accent
  )

  p <- ggplot2::ggplot(
    data = plot_df,
    ggplot2::aes(x = .data$rank, y = .data$estimate, color = .data$relative_to_author)
  ) +
    # Neutral, behind the points: the intervals give the estimates their spread
    # but it is the estimates that carry the comparison.
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
      width = 0, alpha = 0.5, colour = neutral$axis, linewidth = 0.3
    ) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_color_manual(
      values = point_colors,
      name = NULL,
      drop = TRUE
    ) +
    ggplot2::labs(
      title = NULL,
      x = "Studies (sorted by best-practice estimate)",
      y = "Best-practice estimate"
    ) +
    plot_theme +
    ggplot2::theme(legend.position = "bottom")

  if (has_author_reference) {
    p <- p + ggplot2::geom_hline(
      yintercept = author_estimate, linetype = "dashed", color = vline_color, linewidth = 0.5
    )
  }

  spline_points <- plot_df[is.finite(plot_df$rank) & is.finite(plot_df$estimate), , drop = FALSE]
  min_points_for_spline <- 4L
  if (nrow(spline_points) >= min_points_for_spline) {
    spline_fit <- tryCatch(
      stats::smooth.spline(x = spline_points$rank, y = spline_points$estimate),
      error = function(e) NULL
    )
    if (!is.null(spline_fit)) {
      spline_df <- data.frame(rank = spline_fit$x, estimate = spline_fit$y)
      # Neutral and thin. In the theme's contrast colour at linewidth 1 it was
      # indistinguishable from the author's reference line, which means
      # something entirely different.
      p <- p + ggplot2::geom_line(
        data = spline_df,
        mapping = ggplot2::aes(x = .data$rank, y = .data$estimate),
        inherit.aes = FALSE,
        color = neutral$ink_soft,
        linewidth = 0.6
      )
    }
  }

  p
}

#' @title Build Per-Factor BPE Density Plots
#' @description
#' For every predictor flagged via `bpe_sum_stats`/`bpe_equal`/`bpe_gltl` in
#' the data config, split studies into groups by that variable's study-level
#' value and plot the density of their best-practice estimates per group.
#' @keywords internal
build_bpe_density_plots <- function(predictors, config, bma_data, study_index_groups,
                                    study_estimates, round_to, theme_name) {
  plots <- list()

  for (var_name in predictors) {
    config_key <- find_config_key_for_var(var_name, config)
    if (is.null(config_key)) {
      next
    }

    var_cfg <- config[[config_key]]
    if (!is_bpe_factor_var(var_cfg)) {
      next
    }

    var_label <- var_cfg$var_name_verbose %||% var_name
    study_level_values <- resolve_bpe_study_level_values(bma_data, study_index_groups, var_name)

    groups <- resolve_variable_groups(
      var_label = var_label,
      equal_val = var_cfg$bpe_equal,
      gltl_val = var_cfg$bpe_gltl,
      var_values = study_level_values,
      round_to = round_to,
      auto_levels = TRUE
    )

    plot_df <- build_bpe_density_plot_data(
      groups = groups,
      study_ids = names(study_level_values),
      study_estimates = study_estimates
    )

    if (is.null(plot_df) || length(unique(plot_df$group)) < 2) {
      next
    }

    plots[[paste0("bpe_density_", var_name)]] <- create_bpe_density_plot(
      var_label = var_label,
      group_estimates = plot_df,
      theme_name = theme_name
    )
  }

  plots
}

#' @title Study-Level Values for a BPE Grouping Variable
#' @description
#' Averages a predictor's observation-level values within each study,
#' returning a named numeric vector keyed by study id. `study_index_groups`
#' is the precomputed split of row indices by study id.
#' @keywords internal
resolve_bpe_study_level_values <- function(bma_data, study_index_groups, var_name) {
  var_values <- as.numeric(bma_data[[var_name]])
  vapply(
    study_index_groups,
    function(row_idx) mean(var_values[row_idx], na.rm = TRUE),
    numeric(1)
  )
}

#' @title Assemble Per-Group Density Plot Data
#' @description
#' Maps study-level group membership (indices into `study_ids`) back to each
#' study's best-practice estimate, producing a long-format data frame with
#' `estimate` and `group` columns.
#' @keywords internal
build_bpe_density_plot_data <- function(groups, study_ids, study_estimates) {
  if (!length(groups)) {
    return(NULL)
  }

  rows <- lapply(groups, function(group) {
    if (!length(group$row_idx)) {
      return(NULL)
    }
    matched_ids <- study_ids[group$row_idx]
    estimates <- study_estimates$estimate[as.character(study_estimates$study_id) %in% matched_ids]
    estimates <- estimates[is.finite(estimates)]
    if (length(estimates) < 2) {
      return(NULL)
    }
    data.frame(estimate = estimates, group = group$label, stringsAsFactors = FALSE)
  })

  do.call(rbind, Filter(Negate(is.null), rows))
}

#' @title Create a Per-Factor BPE Density Plot
#' @keywords internal
create_bpe_density_plot <- function(var_label, group_estimates, theme_name) {
  box::use(
    artma / visualization / colors[get_colors],
    artma / visualization / theme[get_theme]
  )

  palette <- get_colors(theme_name, "bpe", submethod = "density")
  plot_theme <- get_theme(theme_name)

  ggplot2::ggplot(
    data = group_estimates,
    ggplot2::aes(x = .data$estimate, color = .data$group, fill = .data$group)
  ) +
    ggplot2::geom_density(alpha = 0.12, linewidth = 0.8) +
    ggplot2::scale_color_brewer(palette = palette, name = var_label) +
    ggplot2::scale_fill_brewer(palette = palette, name = var_label) +
    ggplot2::labs(x = "Best-practice estimate", y = "Density") +
    plot_theme +
    ggplot2::theme(legend.position = "bottom")
}

box::export(
  build_bpe_plots
)
