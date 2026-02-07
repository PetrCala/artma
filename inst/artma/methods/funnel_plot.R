#' @title Funnel Plot Visualization
#' @description
#' Create a funnel plot showing effect estimates against their precision.
#' Useful for detecting publication bias in meta-analysis.
#' Supports outlier filtering, study median aggregation, and optional export.
funnel_plot <- function(df) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[assert, validate, validate_columns],
    artma / options / index[get_option_group],
    artma / visualization / options[get_visualization_options],
    artma / visualization / export[save_plot, build_export_filename, ensure_export_dir]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect", "precision"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Creating funnel plot visualization")
  }

  opt <- get_option_group("artma.methods.funnel_plot")
  vis <- get_visualization_options()

  effect_proximity <- opt$effect_proximity %||% 0.2
  maximum_precision <- opt$maximum_precision %||% 0.2
  precision_to_log <- opt$precision_to_log %||% FALSE
  use_study_medians <- opt$use_study_medians %||% FALSE
  add_zero <- opt$add_zero %||% TRUE
  theme_name <- vis$theme
  export_graphics <- vis$export_graphics
  export_path <- vis$export_path
  graph_scale <- vis$graph_scale

  validate(
    is.numeric(effect_proximity),
    is.numeric(maximum_precision),
    is.logical(precision_to_log),
    is.logical(use_study_medians),
    is.logical(add_zero)
  )

  assert(
    effect_proximity >= 0 && effect_proximity <= 1,
    "effect_proximity must be between 0 and 1"
  )
  assert(
    maximum_precision >= 0 && maximum_precision <= 1,
    "maximum_precision must be between 0 and 1"
  )

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

  if (nrow(filtered_df) == 0) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("All observations filtered as outliers. Skipping funnel plot.")
    }
    return(invisible(list(
      plot = NULL,
      n_points = 0L,
      n_outliers_removed = n_outliers,
      used_study_medians = use_study_medians
    )))
  }

  plot_data <- if (use_study_medians) {
    aggregate_study_medians(filtered_df)
  } else {
    filtered_df[, c("effect", "precision"), drop = FALSE]
  }

  tick_info <- generate_funnel_ticks(
    bounds = c(min(plot_data$effect), max(plot_data$effect)),
    mean_effect = mean(plot_data$effect),
    add_zero = add_zero,
    theme_name = theme_name
  )

  plot <- create_funnel_plot(
    df = plot_data,
    tick_info = tick_info,
    theme_name = theme_name,
    precision_to_log = precision_to_log,
    use_study_medians = use_study_medians
  )

  if (get_verbosity() >= 3) {
    cli::cli_h3("Funnel Plot: Effect vs Precision")
    if (n_outliers > 0) {
      cli::cli_alert_info("{n_outliers} outlier{?s} removed")
    }
    suppressWarnings(print(plot)) # nolint: undesirable_function_linter.
  }

  if (export_graphics) {
    export_funnel_plot(
      plot = plot,
      export_path = export_path,
      graph_scale = graph_scale
    )
  }

  result <- list(
    plot = plot,
    n_points = nrow(plot_data),
    n_outliers_removed = n_outliers,
    used_study_medians = use_study_medians
  )
  class(result) <- c("artma_funnel_plot", class(result))
  invisible(result)
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

  result <- lapply(studies, function(sid) {
    study_data <- df[df$study_id == sid, , drop = FALSE]
    median_effect <- stats::median(study_data$effect, na.rm = TRUE)

    closest_idx <- which.min(abs(study_data$effect - median_effect))
    median_precision <- study_data$precision[closest_idx]

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
#' Creates tick positions that include bounds, mean, and optionally zero.
#' Adds intermediate ticks at sensible intervals while maintaining readability.
#'
#' @param bounds *\[numeric(2)\]* Lower and upper bounds of effect values
#' @param mean_effect *\[numeric\]* Mean effect value to highlight
#' @param add_zero *\[logical\]* Whether to include zero if in range
#' @param theme_name *\[character\]* Theme for determining tick colors
#'
#' @return *\[list\]* With elements: ticks (numeric), tick_colors (character)
#' @keywords internal
generate_funnel_ticks <- function(bounds, mean_effect, add_zero, theme_name) {
  box::use(artma / visualization / colors[get_vline_color])

  lower_bound <- bounds[1]
  upper_bound <- bounds[2]
  range_size <- upper_bound - lower_bound

  ticks <- c(lower_bound, upper_bound)

  if (add_zero && !(0 %in% ticks) && lower_bound < 0 && upper_bound > 0) {
    ticks <- c(ticks, 0)
  }

  interval <- determine_tick_interval(range_size)
  min_distance <- interval / 5

  start_tick <- ceiling(lower_bound / interval) * interval

  current_tick <- start_tick
  while (current_tick < upper_bound) {
    far_from_lower <- abs(current_tick - lower_bound) >= min_distance
    far_from_upper <- abs(current_tick - upper_bound) >= min_distance
    not_duplicate <- !(current_tick %in% ticks)

    if (far_from_lower && far_from_upper && not_duplicate) {
      ticks <- c(ticks, current_tick)
    }
    current_tick <- current_tick + interval
  }

  ticks <- sort(unique(c(ticks, mean_effect)))

  tick_colors <- rep("black", length(ticks))
  mean_idx <- which(ticks == mean_effect)
  if (length(mean_idx) > 0) {
    tick_colors[mean_idx] <- get_vline_color(theme_name)
  }

  list(
    ticks = ticks,
    tick_colors = tick_colors,
    mean_effect = mean_effect
  )
}


#' Determine appropriate tick interval based on data range
#'
#' @param range_size *\[numeric\]* The range of data values
#' @return *\[numeric\]* Appropriate tick interval
#' @keywords internal
determine_tick_interval <- function(range_size) {
  if (range_size <= 1) {
    return(0.2)
  }
  if (range_size <= 5) {
    return(1)
  }
  if (range_size <= 20) {
    return(5)
  }
  if (range_size <= 50) {
    return(10)
  }
  if (range_size <= 100) {
    return(20)
  }
  50
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
#'
#' @return *\[ggplot\]* The plot object
#' @keywords internal
create_funnel_plot <- function(df, tick_info, theme_name, precision_to_log, use_study_medians) {
  box::use(
    artma / visualization / colors[get_colors, get_vline_color],
    artma / visualization / theme[get_theme]
  )

  point_color <- get_colors(theme_name, "funnel_plot")
  vline_color <- get_vline_color(theme_name)
  plot_theme <- get_theme(theme_name)

  plot_df <- df
  if (precision_to_log) {
    plot_df$precision <- log(plot_df$precision)
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

  mean_effect <- tick_info$mean_effect

  tick_labels <- format_colored_tick_labels(
    tick_info$ticks,
    tick_info$tick_colors
  )

  p <- ggplot2::ggplot(
    data = plot_df,
    ggplot2::aes(x = .data$effect, y = .data$precision)
  ) +
    ggplot2::geom_point(color = point_color, size = 2) +
    ggplot2::geom_vline(
      xintercept = mean_effect,
      color = vline_color,
      linewidth = 0.5
    ) +
    ggplot2::labs(
      title = NULL,
      x = x_label,
      y = y_label
    ) +
    ggplot2::scale_x_continuous(
      breaks = tick_info$ticks,
      labels = tick_labels
    ) +
    plot_theme

  p
}


#' Format tick values as integers or decimals appropriately
#'
#' @description
#' Displays integer values without decimal points and floats with
#' minimal necessary precision for cleaner axis labels.
#'
#' @param x *\[numeric\]* Tick values
#' @return *\[character\]* Formatted labels
#' @keywords internal
format_tick_labels <- function(x) {
  mapply(x, function(val) { # nolint: undesirable_function_linter.
    if (is.na(val)) {
      return(NA_character_)
    }
    if (val == floor(val)) {
      as.character(as.integer(val))
    } else {
      format(round(val, 2), nsmall = 0, trim = TRUE)
    }
  })
}


#' Format tick labels with HTML color spans
#'
#' @description
#' Wraps tick labels in HTML span elements with color styling for use with
#' ggtext::element_markdown(). This enables per-tick coloring (e.g., highlighting
#' the mean tick in a different color).
#'
#' @param ticks *\[numeric\]* Tick values
#' @param colors *\[character\]* Color for each tick (same length as ticks)
#'
#' @return *\[character\]* HTML-formatted labels
#' @keywords internal
format_colored_tick_labels <- function(ticks, colors) {
  labels <- format_tick_labels(ticks)

  mapply(function(label, color) { # nolint: undesirable_function_linter.
    if (is.na(label)) {
      return(NA_character_)
    }
    sprintf("<span style='color:%s'>%s</span>", color, label)
  }, labels, colors, USE.NAMES = FALSE)
}


#' Export funnel plot to file
#'
#' @param plot *\[ggplot\]* The plot to export
#' @param export_path *\[character\]* Directory path
#' @param graph_scale *\[numeric\]* Scale factor
#'
#' @return NULL (invisibly)
#' @keywords internal
export_funnel_plot <- function(plot, export_path, graph_scale) {
  box::use(
    artma / visualization / export[save_plot, build_export_filename, ensure_export_dir]
  )

  ensure_export_dir(export_path)

  filename <- build_export_filename("funnel_plot", "effect_precision")
  full_path <- file.path(export_path, filename)

  save_plot(
    plot = plot,
    path = full_path,
    width = 800,
    height = 736,
    scale = graph_scale
  )

  invisible(NULL)
}


box::use(
  artma / libs / infrastructure / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

run <- cache_cli_runner(
  funnel_plot,
  stage = "funnel_plot",
  key_builder = function(...) build_data_cache_signature()
)

box::export(funnel_plot, run)
