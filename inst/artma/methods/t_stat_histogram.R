#' @title T-Statistic Distribution Histogram
#' @description
#' Create histograms of t-statistic distributions from a meta-analysis dataset.
#' Visualizes the density of t-statistics with vertical reference lines at
#' critical values (e.g., +/-1.96 for 5% significance), an optional mean line,
#' and an optional density curve overlay.
#' By default produces two plots: a full-range histogram and a close-up view
#' with tighter bounds for detailed inspection.
t_stat_histogram <- function(df) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[assert, validate, validate_columns],
    artma / modules / runtime_methods[new_method_result],
    artma / options / index[get_option_group],
    artma / options / resolver[opt_spec, resolve_options],
    artma / visualization / options[get_visualization_options],
    artma / visualization / export[export_named_plots, preview_plot]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("t_stat"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Creating t-statistic distribution histogram")
  }

  opt <- get_option_group("artma.methods.t_stat_histogram")
  vis <- get_visualization_options()

  resolved <- resolve_options(opt, list(
    lower_cutoff = opt_spec(default = -120, type = "numeric"),
    upper_cutoff = opt_spec(default = 120, type = "numeric"),
    critical_values = opt_spec(
      default = 1.96, type = "numeric",
      constraint = function(x) all(x > 0),
      constraint_msg = "critical_values must be positive (symmetric +/- applied automatically)"
    ),
    n_bins = opt_spec(
      default = 80L, type = "numeric", cast = as.integer,
      constraint = function(x) x > 0,
      constraint_msg = "n_bins must be positive"
    ),
    show_mean_line = opt_spec(default = TRUE, type = "logical"),
    show_density_curve = opt_spec(default = TRUE, type = "logical"),
    min_tick_distance = opt_spec(
      default = 0.5, type = "numeric",
      constraint = function(x) x > 0,
      constraint_msg = "min_tick_distance must be positive"
    ),
    close_up_enabled = opt_spec(default = TRUE, type = "logical"),
    close_up_lower = opt_spec(default = -10, type = "numeric"),
    close_up_upper = opt_spec(default = 10, type = "numeric"),
    close_up_min_tick_distance = opt_spec(default = 0.3, type = "numeric")
  ))

  lower_cutoff <- resolved$lower_cutoff
  upper_cutoff <- resolved$upper_cutoff
  critical_values <- resolved$critical_values
  n_bins <- resolved$n_bins
  show_mean_line <- resolved$show_mean_line
  show_density_curve <- resolved$show_density_curve
  min_tick_distance <- resolved$min_tick_distance
  close_up_enabled <- resolved$close_up_enabled
  close_up_lower <- resolved$close_up_lower
  close_up_upper <- resolved$close_up_upper
  close_up_min_tick_distance <- resolved$close_up_min_tick_distance
  theme_name <- vis$theme
  export_graphics <- vis$export_graphics
  export_path <- vis$export_path
  graph_scale <- vis$graph_scale

  assert(
    lower_cutoff < upper_cutoff,
    "lower_cutoff must be less than upper_cutoff"
  )

  if (close_up_enabled) {
    assert(
      close_up_lower < close_up_upper,
      "close_up_lower must be less than close_up_upper"
    )
    assert(
      close_up_min_tick_distance > 0,
      "close_up_min_tick_distance must be positive"
    )
  }

  # Build the main (full-range) plot
  main_result <- build_histogram(
    t_values = df$t_stat,
    lower_cutoff = lower_cutoff,
    upper_cutoff = upper_cutoff,
    critical_values = critical_values,
    n_bins = n_bins,
    show_mean_line = show_mean_line,
    show_density_curve = show_density_curve,
    min_tick_distance = min_tick_distance,
    theme_name = theme_name
  )

  verbosity <- get_verbosity()

  if (verbosity >= 3) {
    cli::cli_h3("T-Statistic Distribution")
    if (main_result$n_outliers > 0) {
      cli::cli_alert_info(paste0(
        main_result$n_outliers,
        " observation(s) outside [",
        lower_cutoff, ", ", upper_cutoff, "] excluded"
      ))
    }
    preview_plot(main_result$plot)
  }

  # Build the close-up plot (if enabled)
  close_up_result <- NULL

  if (close_up_enabled) {
    close_up_result <- build_histogram(
      t_values = df$t_stat,
      lower_cutoff = close_up_lower,
      upper_cutoff = close_up_upper,
      critical_values = critical_values,
      n_bins = n_bins,
      show_mean_line = show_mean_line,
      show_density_curve = show_density_curve,
      min_tick_distance = close_up_min_tick_distance,
      theme_name = theme_name
    )

    if (verbosity >= 3 && !is.null(close_up_result$plot)) {
      cli::cli_h3(paste0(
        "T-Statistic Distribution (Close-up: [",
        close_up_lower, ", ", close_up_upper, "])"
      ))
      preview_plot(close_up_result$plot)
    }
  }

  # Export if enabled
  if (export_graphics) {
    export_named_plots(
      plots = list(
        full_range = main_result$plot,
        close_up = if (!is.null(close_up_result)) close_up_result$plot
      ),
      base_name = "t_stat_histogram",
      export_path = export_path,
      graph_scale = graph_scale,
      width = 800,
      height = 600
    )
  }

  close_up_plot <- if (!is.null(close_up_result)) {
    close_up_result$plot
  }
  close_up_outliers <- if (!is.null(close_up_result)) {
    close_up_result$n_outliers
  } else {
    0L
  }

  invisible(new_method_result(
    plots = list(
      plot_main = main_result$plot,
      plot_close_up = close_up_plot
    ),
    meta = list(
      n_observations = nrow(df),
      n_outliers_main = main_result$n_outliers,
      n_outliers_close_up = close_up_outliers,
      mean_t_stat = mean(df$t_stat, na.rm = TRUE),
      close_up_enabled = close_up_enabled
    ),
    class = "artma_t_stat_histogram"
  ))
}


#' Filter t-statistic values by cutoff bounds
#'
#' @param t_values *\[numeric\]* Vector of t-statistic values
#' @param lower_cutoff *\[numeric\]* Lower bound
#' @param upper_cutoff *\[numeric\]* Upper bound
#'
#' @return *\[list\]* With elements: filtered (numeric), n_outliers (integer)
#' @keywords internal
filter_by_cutoff <- function(t_values, lower_cutoff, upper_cutoff) {
  in_range <- t_values >= lower_cutoff & t_values <= upper_cutoff
  in_range[is.na(in_range)] <- FALSE

  list(
    filtered = t_values[in_range],
    n_outliers = as.integer(sum(!in_range, na.rm = TRUE))
  )
}


#' Compute effective data bounds
#'
#' @description
#' Returns the tighter of the actual data range vs the cutoff bounds.
#'
#' @param filtered_values *\[numeric\]* Filtered t-statistic values
#' @param lower_cutoff *\[numeric\]* Lower cutoff bound
#' @param upper_cutoff *\[numeric\]* Upper cutoff bound
#'
#' @return *\[numeric(2)\]* Lower and upper effective bounds
#' @keywords internal
compute_data_bounds <- function(filtered_values,
                                lower_cutoff,
                                upper_cutoff) {
  data_min <- min(filtered_values, na.rm = TRUE)
  data_max <- max(filtered_values, na.rm = TRUE)

  c(
    max(data_min, lower_cutoff),
    min(data_max, upper_cutoff)
  )
}


#' Generate intelligent x-axis ticks for t-statistic histogram
#'
#' @description
#' Creates tick positions that include bounds, mean, and critical t-stat
#' values. Regular ticks maintain minimum distance from critical values
#' and the mean to prevent overlap, and a final `thin_ticks()` pass resolves
#' the collisions the generator cannot see: a data bound landing next to a
#' critical value or the mean. Critical values outrank the mean, which
#' outranks the bounds, which outrank the regular grid.
#'
#' @param bounds *\[numeric(2)\]* Lower and upper data bounds
#' @param mean_value *\[numeric\]* Mean t-statistic
#' @param critical_values *\[numeric\]* Positive critical values
#' @param min_tick_distance *\[numeric\]* Min distance between ticks
#'
#' @return *\[list\]* With: ticks, mean_value, critical_ticks
#' @keywords internal
generate_histogram_ticks <- function(bounds,
                                     mean_value,
                                     critical_values,
                                     min_tick_distance) {
  box::use(
    artma / visualization / ticks[
      generate_regular_ticks,
      resolve_tick_interval,
      thin_ticks,
      tick_min_separation
    ]
  )

  lower <- bounds[1]
  upper <- bounds[2]
  range_size <- upper - lower

  # Symmetric critical value ticks within bounds
  crit_ticks <- sort(unique(c(-critical_values, critical_values)))
  crit_ticks <- crit_ticks[crit_ticks >= lower & crit_ticks <= upper]

  # All special ticks that regular ticks must avoid
  special_ticks <- unique(c(crit_ticks, mean_value))

  # Determine interval for regular ticks
  interval <- resolve_tick_interval(range_size)

  # Generate regular ticks
  regular_ticks <- generate_regular_ticks(
    lower = lower,
    upper = upper,
    interval = interval,
    edge_distance = min_tick_distance / 2,
    special_values = special_ticks,
    special_distance = min_tick_distance,
    upper_inclusive = TRUE
  )

  # Assembled in descending priority so thin_ticks() drops the filler first.
  # The separation floor also respects the data range: a user-set
  # min_tick_distance of 0.5 over a range of 240 would still overlap.
  all_ticks <- c(crit_ticks, mean_value, lower, upper, regular_ticks)
  priority <- c(
    rep(4, length(crit_ticks)),
    3,
    2, 2,
    rep(1, length(regular_ticks))
  )

  min_distance <- max(min_tick_distance, tick_min_separation(range_size))
  kept <- thin_ticks(all_ticks, min_distance = min_distance, priority = priority)

  list(
    ticks = all_ticks[kept],
    mean_value = mean_value,
    critical_ticks = crit_ticks
  )
}


#' Build a single t-statistic histogram
#'
#' @param t_values *\[numeric\]* All t-statistic values (pre-filtering)
#' @param lower_cutoff *\[numeric\]* Lower bound for filtering
#' @param upper_cutoff *\[numeric\]* Upper bound for filtering
#' @param critical_values *\[numeric\]* Positive critical values
#' @param n_bins *\[integer\]* Number of histogram bins
#' @param show_mean_line *\[logical\]* Show mean reference line
#' @param show_density_curve *\[logical\]* Overlay density curve
#' @param min_tick_distance *\[numeric\]* Min distance between ticks
#' @param theme_name *\[character\]* Theme name
#'
#' @return *\[list\]* With: plot (ggplot), n_outliers (integer),
#'   mean_t (numeric)
#' @keywords internal
build_histogram <- function(t_values,
                            lower_cutoff,
                            upper_cutoff,
                            critical_values,
                            n_bins,
                            show_mean_line,
                            show_density_curve,
                            min_tick_distance,
                            theme_name) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / visualization / colors[get_colors],
    artma / visualization / theme[get_theme],
    artma / visualization / ticks[format_tick_labels]
  )

  filter_result <- filter_by_cutoff(t_values, lower_cutoff, upper_cutoff)
  filtered <- filter_result$filtered

  if (length(filtered) == 0) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning(
        "All observations filtered. Skipping histogram."
      )
    }
    return(list(
      plot = NULL,
      n_outliers = filter_result$n_outliers,
      mean_t = NA_real_
    ))
  }

  bounds <- compute_data_bounds(filtered, lower_cutoff, upper_cutoff)
  mean_t <- mean(filtered, na.rm = TRUE)

  tick_info <- generate_histogram_ticks(
    bounds = bounds,
    mean_value = mean_t,
    critical_values = critical_values,
    min_tick_distance = min_tick_distance
  )

  fill_color <- get_colors(theme_name, "t_stat_histogram", "main")
  density_color <- get_colors(theme_name, "t_stat_histogram", "density")
  critical_color <- get_colors(
    theme_name, "t_stat_histogram", "critical"
  )
  mean_color <- get_colors(theme_name, "t_stat_histogram", "mean")
  plot_theme <- get_theme(theme_name)

  tick_labels <- format_tick_labels(tick_info$ticks)

  plot_df <- data.frame(t_stat = filtered)

  critical_ticks <- tick_info$critical_ticks
  mean_label <- "Mean t-statistic"

  p <- ggplot2::ggplot(
    data = plot_df,
    ggplot2::aes(x = .data$t_stat)
  )

  # The region beyond the loosest critical value is where a t-statistic reads
  # as significant, and it is the part of the distribution p-hacking distorts.
  # Shading it states that directly; a pair of bare rules left the reader to
  # work out which side of the line mattered.
  if (length(critical_ticks) > 0) {
    shade_from <- min(abs(critical_ticks))
    shade_df <- data.frame(
      xmin = c(-Inf, shade_from),
      xmax = c(-shade_from, Inf),
      band = "Significant"
    )
    p <- p + ggplot2::geom_rect(
      data = shade_df,
      mapping = ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, fill = .data$band),
      ymin = -Inf, ymax = Inf,
      inherit.aes = FALSE,
      alpha = 0.08
    ) +
      ggplot2::scale_fill_manual(
        values = stats::setNames(critical_color, "Significant"),
        labels = paste0("Significant (|t| > ", format_tick_labels(shade_from), ")"),
        name = NULL
      )
  }

  p <- p +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = n_bins,
      fill = fill_color,
      colour = NA
    )

  # Add density curve
  if (show_density_curve) {
    p <- p + ggplot2::geom_density(
      colour = density_color,
      fill = density_color,
      alpha = 0.12,
      linewidth = 0.7
    )
  }

  # The critical values are the visible edge of the shaded band, so they need
  # no legend entry of their own; one that repeated the threshold would just
  # duplicate the band's label.
  if (length(critical_ticks) > 0) {
    p <- p + ggplot2::geom_vline(
      xintercept = critical_ticks,
      colour = critical_color,
      linetype = "dotted",
      linewidth = 0.5
    )
  }

  # The mean does carry a mapped label, so ggplot2 names it in a legend rather
  # than tinting the matching axis tick and leaving the colour unexplained.
  if (show_mean_line) {
    p <- p +
      ggplot2::geom_vline(
        data = data.frame(xintercept = mean_t, label = mean_label, stringsAsFactors = FALSE),
        mapping = ggplot2::aes(
          xintercept = .data$xintercept,
          colour = .data$label,
          linetype = .data$label
        ),
        linewidth = 0.5,
        key_glyph = "path"
      ) +
      ggplot2::scale_colour_manual(values = stats::setNames(mean_color, mean_label), name = NULL) +
      ggplot2::scale_linetype_manual(values = stats::setNames("dashed", mean_label), name = NULL)
  }

  p <- p +
    ggplot2::labs(
      title = NULL,
      x = "T-statistic",
      y = "Density"
    ) +
    ggplot2::scale_x_continuous(
      breaks = tick_info$ticks,
      labels = tick_labels
    ) +
    plot_theme +
    ggplot2::theme(legend.position = "bottom")

  list(
    plot = p,
    n_outliers = filter_result$n_outliers,
    mean_t = mean_t
  )
}


box::use(
  artma / modules / runtime_methods[register_runtime_method]
)

run <- register_runtime_method(
  t_stat_histogram,
  stage = "t_stat_histogram",
  description = "Histograms of the t-statistic distribution with significance lines",
  required_columns = "t_stat"
)

box::export(t_stat_histogram, run)
