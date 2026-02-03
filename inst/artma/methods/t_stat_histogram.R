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
    artma / options / index[get_option_group],
    artma / visualization / options[get_visualization_options],
    artma / visualization / export[
      save_plot,
      build_export_filename,
      ensure_export_dir
    ]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("t_stat"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Creating t-statistic distribution histogram")
  }

  opt <- get_option_group("artma.methods.t_stat_histogram")
  vis <- get_visualization_options()

  lower_cutoff <- opt$lower_cutoff %||% -120
  upper_cutoff <- opt$upper_cutoff %||% 120
  critical_values <- opt$critical_values %||% 1.96
  n_bins <- opt$n_bins %||% 80L
  show_mean_line <- opt$show_mean_line %||% TRUE
  show_density_curve <- opt$show_density_curve %||% TRUE
  min_tick_distance <- opt$min_tick_distance %||% 0.5
  close_up_enabled <- opt$close_up_enabled %||% TRUE
  close_up_lower <- opt$close_up_lower %||% -10
  close_up_upper <- opt$close_up_upper %||% 10
  close_up_min_tick_distance <- opt$close_up_min_tick_distance %||% 0.3
  theme_name <- vis$theme
  export_graphics <- vis$export_graphics
  export_path <- vis$export_path
  graph_scale <- vis$graph_scale

  validate(
    is.numeric(lower_cutoff),
    is.numeric(upper_cutoff),
    is.numeric(critical_values),
    is.numeric(n_bins),
    is.logical(show_mean_line),
    is.logical(show_density_curve),
    is.numeric(min_tick_distance),
    is.logical(close_up_enabled),
    is.numeric(close_up_lower),
    is.numeric(close_up_upper),
    is.numeric(close_up_min_tick_distance)
  )

  n_bins <- as.integer(n_bins)
  assert(
    lower_cutoff < upper_cutoff,
    "lower_cutoff must be less than upper_cutoff"
  )
  assert(n_bins > 0, "n_bins must be positive")
  assert(
    all(critical_values > 0),
    "critical_values must be positive (symmetric +/- applied automatically)"
  )
  assert(min_tick_distance > 0, "min_tick_distance must be positive")

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
    suppressWarnings(print(main_result$plot))
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
      suppressWarnings(print(close_up_result$plot))
    }
  }

  # Export if enabled
  if (export_graphics) {
    export_t_stat_histograms(
      main_plot = main_result$plot,
      close_up_plot = if (!is.null(close_up_result)) {
        close_up_result$plot
      },
      export_path = export_path,
      graph_scale = graph_scale
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

  result <- list(
    plot_main = main_result$plot,
    plot_close_up = close_up_plot,
    n_observations = nrow(df),
    n_outliers_main = main_result$n_outliers,
    n_outliers_close_up = close_up_outliers,
    mean_t_stat = mean(df$t_stat, na.rm = TRUE),
    close_up_enabled = close_up_enabled
  )
  class(result) <- c("artma_t_stat_histogram", class(result))
  invisible(result)
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


#' Determine tick interval for histogram based on data range
#'
#' @param range_size *\[numeric\]* Range of data
#' @return *\[numeric\]* Tick interval
#' @keywords internal
determine_histogram_tick_interval <- function(range_size) {
  if (range_size <= 5) return(1)
  if (range_size <= 20) return(2)
  if (range_size <= 50) return(5)
  if (range_size <= 100) return(10)
  if (range_size <= 200) return(20)
  50
}


#' Generate intelligent x-axis ticks for t-statistic histogram
#'
#' @description
#' Creates tick positions that include bounds, mean, and critical t-stat
#' values. Regular ticks maintain minimum distance from critical values
#' and the mean to prevent overlap. Colors: critical values get a
#' contrasting color, mean gets the vline accent color, others black.
#'
#' @param bounds *\[numeric(2)\]* Lower and upper data bounds
#' @param mean_value *\[numeric\]* Mean t-statistic
#' @param critical_values *\[numeric\]* Positive critical values
#' @param min_tick_distance *\[numeric\]* Min distance between ticks
#' @param theme_name *\[character\]* Theme name for color resolution
#'
#' @return *\[list\]* With: ticks, tick_colors, mean_value, critical_ticks
#' @keywords internal
generate_histogram_ticks <- function(bounds,
                                     mean_value,
                                     critical_values,
                                     min_tick_distance,
                                     theme_name) {
  box::use(
    artma / visualization / colors[get_colors, get_vline_color]
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
  interval <- determine_histogram_tick_interval(range_size)

  # Generate regular ticks
  start_tick <- ceiling(lower / interval) * interval
  regular_ticks <- numeric(0)
  current <- start_tick
  while (current <= upper) {
    far_from_special <- all(
      abs(current - special_ticks) >= min_tick_distance
    )
    far_from_lower <- abs(current - lower) >= min_tick_distance / 2
    far_from_upper <- abs(current - upper) >= min_tick_distance / 2

    if (far_from_special && far_from_lower && far_from_upper) {
      regular_ticks <- c(regular_ticks, current)
    }
    current <- current + interval
  }

  # Combine all ticks
  all_ticks <- sort(unique(c(
    lower, upper, crit_ticks, mean_value, regular_ticks
  )))

  # Assign colors
  critical_color <- get_colors(
    theme_name, "t_stat_histogram", "critical"
  )
  mean_color <- get_colors(
    theme_name, "t_stat_histogram", "mean"
  )

  tick_colors <- rep("black", length(all_ticks))

  for (cv in crit_ticks) {
    idx <- which(abs(all_ticks - cv) < 1e-10)
    if (length(idx) > 0) {
      tick_colors[idx] <- critical_color
    }
  }

  mean_idx <- which(abs(all_ticks - mean_value) < 1e-10)
  if (length(mean_idx) > 0) {
    tick_colors[mean_idx] <- mean_color
  }

  list(
    ticks = all_ticks,
    tick_colors = tick_colors,
    mean_value = mean_value,
    critical_ticks = crit_ticks
  )
}


#' Format tick values as integers or decimals
#'
#' @param x *\[numeric\]* Tick values
#' @return *\[character\]* Formatted labels
#' @keywords internal
format_tick_labels <- function(x) {
  vapply(x, function(val) {
    if (is.na(val)) return(NA_character_)
    if (val == floor(val)) {
      as.character(as.integer(val))
    } else {
      format(round(val, 2), nsmall = 0, trim = TRUE)
    }
  }, FUN.VALUE = character(1))
}


#' Format tick labels with HTML color spans
#'
#' @description
#' Wraps tick labels in HTML span elements with color styling for use
#' with ggtext::element_markdown().
#'
#' @param ticks *\[numeric\]* Tick values
#' @param colors *\[character\]* Color for each tick
#'
#' @return *\[character\]* HTML-formatted labels
#' @keywords internal
format_colored_tick_labels <- function(ticks, colors) {
  labels <- format_tick_labels(ticks)

  vapply(seq_along(labels), function(i) {
    if (is.na(labels[i])) return(NA_character_)
    sprintf("<span style='color:%s'>%s</span>", colors[i], labels[i])
  }, FUN.VALUE = character(1))
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
    artma / visualization / theme[get_theme]
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
    min_tick_distance = min_tick_distance,
    theme_name = theme_name
  )

  fill_color <- get_colors(theme_name, "t_stat_histogram", "main")
  density_color <- get_colors(theme_name, "t_stat_histogram", "density")
  critical_color <- get_colors(
    theme_name, "t_stat_histogram", "critical"
  )
  mean_color <- get_colors(theme_name, "t_stat_histogram", "mean")
  plot_theme <- get_theme(theme_name)

  tick_labels <- format_colored_tick_labels(
    tick_info$ticks, tick_info$tick_colors
  )

  plot_df <- data.frame(t_stat = filtered)

  p <- ggplot2::ggplot(
    data = plot_df,
    ggplot2::aes(x = .data$t_stat)
  ) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = n_bins,
      fill = fill_color,
      color = "black",
      linewidth = 0.1
    ) +
    ggplot2::labs(
      title = NULL,
      x = "T-statistic",
      y = "Density"
    ) +
    ggplot2::scale_x_continuous(
      breaks = tick_info$ticks,
      labels = tick_labels
    ) +
    plot_theme

  # Add critical value lines
  for (cv in tick_info$critical_ticks) {
    p <- p + ggplot2::geom_vline(
      xintercept = cv,
      color = critical_color,
      linewidth = 0.5
    )
  }

  # Add mean line (dashed)
  if (show_mean_line) {
    p <- p + ggplot2::geom_vline(
      xintercept = mean_t,
      color = mean_color,
      linetype = "dashed",
      linewidth = 0.7
    )
  }

  # Add density curve
  if (show_density_curve) {
    p <- p + ggplot2::geom_density(
      color = density_color,
      alpha = 0.2,
      linewidth = 1
    )
  }

  list(
    plot = p,
    n_outliers = filter_result$n_outliers,
    mean_t = mean_t
  )
}


#' Export t-statistic histograms to files
#'
#' @param main_plot *\[ggplot\]* Main histogram
#' @param close_up_plot *\[ggplot, NULL\]* Close-up histogram
#' @param export_path *\[character\]* Directory
#' @param graph_scale *\[numeric\]* Scale factor
#'
#' @return NULL (invisibly)
#' @keywords internal
export_t_stat_histograms <- function(main_plot,
                                     close_up_plot,
                                     export_path,
                                     graph_scale) {
  box::use(
    artma / visualization / export[
      save_plot,
      build_export_filename,
      ensure_export_dir
    ]
  )

  ensure_export_dir(export_path)

  if (!is.null(main_plot)) {
    filename <- build_export_filename(
      "t_stat_histogram", "full_range"
    )
    save_plot(
      plot = main_plot,
      path = file.path(export_path, filename),
      width = 800,
      height = 600,
      scale = graph_scale
    )
  }

  if (!is.null(close_up_plot)) {
    filename <- build_export_filename(
      "t_stat_histogram", "close_up"
    )
    save_plot(
      plot = close_up_plot,
      path = file.path(export_path, filename),
      width = 800,
      height = 600,
      scale = graph_scale
    )
  }

  invisible(NULL)
}


box::use(
  artma / libs / infrastructure / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

run <- cache_cli_runner(
  t_stat_histogram,
  stage = "t_stat_histogram",
  key_builder = function(...) build_data_cache_signature()
)

box::export(t_stat_histogram, run)
