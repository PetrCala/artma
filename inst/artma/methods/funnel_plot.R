#' @title Funnel Plot Visualization
#' @description
#' Create a funnel plot showing effect estimates against their precision.
#' Useful for detecting publication bias in meta-analysis.
#' Supports outlier filtering, study median aggregation, and optional export.
funnel_plot <- function(df) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[validate, validate_columns],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / resolver[opt_spec, resolve_options],
    artma / visualization / options[get_visualization_options],
    artma / visualization / export[export_named_plots, preview_plot]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "precision"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Creating funnel plot visualization")
  }

  opt <- get_option_group("artma.methods.funnel_plot")
  vis <- get_visualization_options()

  resolved <- resolve_options(opt, list(
    effect_proximity = opt_spec(
      default = 0.2, type = "numeric",
      constraint = function(x) x >= 0 && x <= 1,
      constraint_msg = "effect_proximity must be between 0 and 1"
    ),
    maximum_precision = opt_spec(
      default = 0.2, type = "numeric",
      constraint = function(x) x >= 0 && x <= 1,
      constraint_msg = "maximum_precision must be between 0 and 1"
    ),
    precision_to_log = opt_spec(default = FALSE, type = "logical"),
    use_study_medians = opt_spec(default = FALSE, type = "logical"),
    add_zero = opt_spec(default = TRUE, type = "logical"),
    show_contours = opt_spec(default = TRUE, type = "logical")
  ))

  effect_proximity <- resolved$effect_proximity
  maximum_precision <- resolved$maximum_precision
  precision_to_log <- resolved$precision_to_log
  use_study_medians <- resolved$use_study_medians
  add_zero <- resolved$add_zero
  show_contours <- resolved$show_contours
  theme_name <- vis$theme
  export_graphics <- vis$export_graphics
  export_path <- vis$export_path
  graph_scale <- vis$graph_scale

  if (use_study_medians) {
    validate_columns(df, c("study_id"))
  }

  filter_result <- filter_outliers(
    df = df,
    effect_proximity = effect_proximity,
    maximum_precision = maximum_precision
  )

  filtered_df <- filter_result$data
  n_outliers <- filter_result$n_outliers
  n_total <- nrow(df)
  pct_filtered <- if (n_total > 0) round(100 * n_outliers / n_total) else 0L

  if (nrow(filtered_df) == 0) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("All {n_total} observations filtered as outliers. Skipping funnel plot.")
    }
    return(invisible(new_method_result(
      plots = list(funnel_plot = NULL),
      meta = list(
        n_points = 0L,
        n_outliers_removed = n_outliers,
        pct_filtered = pct_filtered,
        used_study_medians = use_study_medians
      ),
      class = "artma_funnel_plot"
    )))
  }

  if (n_outliers > 0 && get_verbosity() >= 2) {
    cli::cli_alert_info(
      "{n_outliers} of {n_total} observations ({pct_filtered}%) filtered as outliers"
    )
  }

  plot_data <- if (use_study_medians) {
    aggregate_study_medians(filtered_df)
  } else {
    filtered_df[, c("effect", "precision"), drop = FALSE]
  }

  tick_info <- generate_funnel_ticks(
    bounds = c(min(plot_data$effect), max(plot_data$effect)),
    mean_effect = mean(plot_data$effect),
    add_zero = add_zero
  )

  subtitle_text <- if (n_outliers > 0) {
    sprintf("%d of %d observations filtered as outliers (%d%%)", n_outliers, n_total, pct_filtered)
  } else {
    NULL
  }

  plot <- create_funnel_plot(
    df = plot_data,
    tick_info = tick_info,
    theme_name = theme_name,
    precision_to_log = precision_to_log,
    use_study_medians = use_study_medians,
    show_contours = show_contours,
    subtitle_text = subtitle_text
  )

  if (get_verbosity() >= 3) {
    cli::cli_h3("Funnel Plot: Effect vs Precision")
    preview_plot(plot)
  }

  if (export_graphics) {
    export_named_plots(
      plots = list(effect_precision = plot),
      base_name = "funnel_plot",
      export_path = export_path,
      graph_scale = graph_scale,
      width = 800,
      height = 736
    )
  }

  invisible(new_method_result(
    plots = list(funnel_plot = plot),
    meta = list(
      n_points = nrow(plot_data),
      n_outliers_removed = n_outliers,
      pct_filtered = pct_filtered,
      used_study_medians = use_study_medians
    ),
    class = "artma_funnel_plot"
  ))
}


#' Filter outliers based on effect proximity and precision thresholds
#'
#' @description
#' Identifies and removes outliers where effects are far from the mean
#' AND precision is high (suggesting potentially problematic estimates).
#'
#' @param df *\[data.frame\]* Data frame with effect and precision columns
#' @param effect_proximity *\[numeric\]* Fraction of max effect range for cutoff (0-1)
#' @param maximum_precision *\[numeric\]* Fraction of max precision for cutoff (0-1)
#'
#' @return *\[list\]* With elements: data (filtered df), n_outliers (count removed)
#' @keywords internal
filter_outliers <- function(df, effect_proximity, maximum_precision) {
  box::use(artma / libs / core / utils[get_verbosity])

  effect <- df$effect
  precision <- df$precision

  max_effect <- max(effect, na.rm = TRUE)
  max_precision <- max(precision, na.rm = TRUE)
  effect_mean <- mean(effect, na.rm = TRUE)

  effect_cutoff <- max_effect * effect_proximity
  precision_cutoff <- max_precision * maximum_precision

  effect_outside_bounds <- (effect < effect_mean - effect_cutoff) |
    (effect > effect_mean + effect_cutoff)
  precision_high <- precision >= precision_cutoff

  is_outlier <- effect_outside_bounds & precision_high

  n_outliers <- sum(is_outlier, na.rm = TRUE)

  if (n_outliers > 0 && get_verbosity() >= 4) {
    cli::cli_alert_info("Funnel plot outlier filtering:")
    cli::cli_bullets(c(
      "*" = "Effect cutoff: mean +/- {round(effect_cutoff, 2)}",
      "*" = "Precision cutoff: >= {round(precision_cutoff, 2)}",
      "*" = "Outliers identified: {n_outliers}"
    ))
  }

  list(
    data = df[!is_outlier, , drop = FALSE],
    n_outliers = n_outliers
  )
}


#' Aggregate data to study medians
#'
#' @description
#' Reduces multiple observations per study to a single point using the median
#' effect. The precision value is taken from the observation closest to the median.
#'
#' @param df *\[data.frame\]* Data with study_id, effect, precision columns
#'
#' @return *\[data.frame\]* One row per study with effect and precision
#' @keywords internal
aggregate_study_medians <- function(df) {
  box::use(artma / libs / core / validation[validate])

  validate("study_id" %in% names(df))

  studies <- unique(df$study_id)
  study_rows <- unname(split(seq_len(nrow(df)), match(df$study_id, studies)))

  result <- lapply(study_rows, function(rows) {
    study_effects <- df$effect[rows]
    median_effect <- stats::median(study_effects, na.rm = TRUE)

    closest_idx <- which.min(abs(study_effects - median_effect))
    median_precision <- df$precision[rows][closest_idx]

    data.frame(
      effect = median_effect,
      precision = median_precision,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, result)
}


#' Generate intelligent x-axis ticks for funnel plot
#'
#' @description
#' Creates tick positions that include bounds, mean, and optionally zero, plus
#' intermediate ticks at sensible intervals. Every candidate then goes through
#' `thin_ticks()`, which drops whichever ticks would print on top of each other:
#' the mean outranks zero, zero outranks the data bounds, and the regular grid
#' gives way to all of them. Without that pass a data bound sitting just short
#' of a grid tick (e.g. a maximum of 369.5 next to a tick at 350) produces
#' overlapping labels.
#'
#' @param bounds *\[numeric(2)\]* Lower and upper bounds of effect values
#' @param mean_effect *\[numeric\]* Mean effect value to highlight
#' @param add_zero *\[logical\]* Whether to include zero if in range
#'
#' @return *\[list\]* With elements: ticks (numeric), mean_effect (numeric)
#' @keywords internal
generate_funnel_ticks <- function(bounds, mean_effect, add_zero) {
  box::use(
    artma / visualization / ticks[
      generate_regular_ticks,
      resolve_tick_interval,
      thin_ticks,
      tick_min_separation
    ]
  )

  lower_bound <- bounds[1]
  upper_bound <- bounds[2]
  range_size <- upper_bound - lower_bound

  interval <- resolve_tick_interval(range_size)
  min_distance <- tick_min_separation(range_size)

  # Priorities feed thin_ticks(): the mean is the point of the plot, zero is
  # the null effect, the bounds show the data extent, and the grid is filler.
  ticks <- c(mean_effect, lower_bound, upper_bound)
  priority <- c(4, 2, 2)

  if (add_zero && lower_bound < 0 && upper_bound > 0) {
    ticks <- c(ticks, 0)
    priority <- c(priority, 3)
  }

  regular_ticks <- generate_regular_ticks(
    lower = lower_bound,
    upper = upper_bound,
    interval = interval,
    edge_distance = min_distance,
    special_values = mean_effect,
    special_distance = min_distance,
    upper_inclusive = FALSE
  )
  ticks <- c(ticks, regular_ticks)
  priority <- c(priority, rep(1, length(regular_ticks)))

  kept <- thin_ticks(ticks, min_distance = min_distance, priority = priority)

  list(
    ticks = ticks[kept],
    mean_effect = mean_effect
  )
}


#' Build 95% pseudo-confidence contours for a funnel plot
#'
#' @description
#' The contours trace `mean_effect +/- 1.96 * SE` across the observed precision
#' range: the region an unbiased literature's estimates should fall inside, and
#' the reference against which funnel asymmetry is read.
#'
#' They are only meaningful when precision *is* `1 / SE`. Under the `DoF`
#' precision type precision is `sqrt(reg_dof)`, which carries no standard-error
#' relationship, so the contours would be drawn from a quantity that does not
#' define them. This returns `NULL` in that case and the plot simply omits them.
#'
#' @param precision *\[numeric\]* Observed precision values, untransformed
#' @param mean_effect *\[numeric\]* Center of the contours
#' @param critical_value *\[numeric\]* Multiplier for the contour width
#' @param n_points *\[integer\]* Resolution of each contour line
#'
#' @return *\[data.frame or NULL\]* Long-format contour points with `effect`,
#'   `precision` and `side` columns
#' @keywords internal
build_funnel_contours <- function(precision, mean_effect, critical_value = 1.96, n_points = 128L) {
  box::use(artma / options / typed_accessors[get_precision_type])

  if (!identical(get_precision_type(), "1/SE")) {
    return(NULL)
  }

  finite_precision <- precision[is.finite(precision) & precision > 0]
  if (length(finite_precision) < 2) {
    return(NULL)
  }

  grid <- seq(min(finite_precision), max(finite_precision), length.out = n_points)
  half_width <- critical_value / grid

  rbind(
    data.frame(effect = mean_effect - half_width, precision = grid, side = "lower"),
    data.frame(effect = mean_effect + half_width, precision = grid, side = "upper")
  )
}


#' Create a single funnel plot
#'
#' @description
#' Builds the ggplot2 funnel plot object with proper theming.
#'
#' @param df *\[data.frame\]* Data with effect and precision columns
#' @param tick_info *\[list\]* From generate_funnel_ticks()
#' @param theme_name *\[character\]* Theme name
#' @param precision_to_log *\[logical\]* Whether to log-transform precision
#' @param use_study_medians *\[logical\]* For axis label text
#' @param show_contours *\[logical\]* Whether to draw 95% pseudo-confidence contours
#' @param subtitle_text *\[character, optional\]* Subtitle noting filtered observations
#'
#' @return *\[ggplot\]* The plot object
#' @keywords internal
create_funnel_plot <- function(df, tick_info, theme_name, precision_to_log, use_study_medians,
                               show_contours = TRUE, subtitle_text = NULL) {
  box::use(
    artma / visualization / colors[get_colors, get_vline_color, get_neutral],
    artma / visualization / theme[get_theme],
    artma / visualization / ticks[format_tick_labels]
  )

  point_color <- get_colors(theme_name, "funnel_plot")
  vline_color <- get_vline_color(theme_name)
  neutral <- get_neutral()
  plot_theme <- get_theme(theme_name)

  mean_effect <- tick_info$mean_effect

  contours <- if (show_contours) {
    build_funnel_contours(df$precision, mean_effect)
  } else {
    NULL
  }

  plot_df <- df
  if (precision_to_log) {
    plot_df$precision <- log(plot_df$precision)
    if (!is.null(contours)) {
      contours$precision <- log(contours$precision)
    }
  }

  x_label <- if (use_study_medians) {
    "Estimate of the effect - study median values"
  } else {
    "Estimate of the effect - all observations"
  }

  y_label <- if (precision_to_log) {
    "log(Precision of the effect)"
  } else {
    "Precision of the effect"
  }

  tick_labels <- format_tick_labels(tick_info$ticks)

  # Reference lines carry a mapped label rather than a fixed colour, so ggplot2
  # builds a legend for them. The previous version tinted the matching x-axis
  # tick instead, which left the reader to infer what a colored number meant.
  mean_label <- "Mean effect"
  zero_label <- "No effect"
  contour_label <- "95% pseudo-CI"

  reference_lines <- data.frame(
    xintercept = mean_effect,
    label = mean_label,
    stringsAsFactors = FALSE
  )

  effect_range <- range(plot_df$effect, na.rm = TRUE)
  if (effect_range[1] < 0 && effect_range[2] > 0) {
    reference_lines <- rbind(
      reference_lines,
      data.frame(xintercept = 0, label = zero_label, stringsAsFactors = FALSE)
    )
  }

  # Overplotting is the normal case here: a meta-analysis routinely carries
  # thousands of estimates, and at full opacity the dense core reads as one
  # solid mass. Fade the points as the count grows, with a floor that keeps a
  # lone estimate visible.
  n_points <- nrow(plot_df)
  point_alpha <- max(0.25, min(0.8, 250 / max(n_points, 1)))

  p <- ggplot2::ggplot(
    data = plot_df,
    ggplot2::aes(x = .data$effect, y = .data$precision)
  )

  if (!is.null(contours)) {
    p <- p + ggplot2::geom_line(
      data = contours,
      mapping = ggplot2::aes(
        x = .data$effect, y = .data$precision,
        group = .data$side, colour = contour_label, linetype = contour_label
      ),
      inherit.aes = FALSE,
      linewidth = 0.5,
      key_glyph = "path"
    )
  }

  p <- p +
    ggplot2::geom_point(color = point_color, size = 1.4, alpha = point_alpha) +
    ggplot2::geom_vline(
      data = reference_lines,
      mapping = ggplot2::aes(
        xintercept = .data$xintercept,
        colour = .data$label,
        linetype = .data$label
      ),
      linewidth = 0.5,
      # Both layers feed one colour scale, so they must contribute the same
      # key glyph or the legend mixes vertical and horizontal rules.
      key_glyph = "path"
    ) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(
        c(vline_color, neutral$muted, neutral$axis),
        c(mean_label, zero_label, contour_label)
      ),
      breaks = c(mean_label, zero_label, contour_label),
      limits = c(mean_label, zero_label, contour_label),
      drop = TRUE
    ) +
    ggplot2::scale_linetype_manual(
      values = stats::setNames(
        c("dashed", "solid", "dotted"),
        c(mean_label, zero_label, contour_label)
      ),
      breaks = c(mean_label, zero_label, contour_label),
      limits = c(mean_label, zero_label, contour_label),
      drop = TRUE
    ) +
    ggplot2::labs(
      title = NULL,
      subtitle = subtitle_text,
      x = x_label,
      y = y_label,
      colour = NULL,
      linetype = NULL
    ) +
    ggplot2::scale_x_continuous(
      breaks = tick_info$ticks,
      labels = tick_labels
    ) +
    # The contours reach mean +/- 1.96/precision, which at the lowest observed
    # precision lies far outside the estimates; without clipping they would
    # drag the x-axis with them. Clip the panel to the data instead and let the
    # contour lines run off its edges.
    ggplot2::coord_cartesian(xlim = effect_range) +
    plot_theme +
    ggplot2::theme(legend.position = "bottom")

  p
}


box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  funnel_plot,
  stage = "funnel_plot",
  description = "Funnel plot of the effect against precision, for spotting asymmetry",
  required_columns = c("effect", "precision")
)

box::export(funnel_plot, run)
