#' @title Box Plot Visualization
#' @description
#' Create box plots of effect values grouped by a categorical variable.
#' Supports automatic splitting for large datasets (many unique factor values)
#' and optional export to PNG files.
box_plot <- function(df) {
  box::use(
    artma / data_config / read[get_data_config],
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[assert, validate, validate_columns],
    artma / libs / core / autonomy[should_prompt_user],
    artma / options / index[get_option_group],
    artma / visualization / options[get_visualization_options],
    artma / visualization / export[save_plot, build_export_filename, ensure_export_dir]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Creating box plot visualization")
  }

  config <- get_data_config()
  opt <- get_option_group("artma.methods.box_plot")
  vis <- get_visualization_options()

  factor_by <- opt$factor_by %||% NA_character_
  max_per_plot <- opt$max_boxes_per_plot %||% 60L
  theme_name <- vis$theme
  show_mean_line <- opt$show_mean_line %||% TRUE
  effect_label <- opt$effect_label %||% "effect"
  export_graphics <- vis$export_graphics
  export_path <- vis$export_path
  graph_scale <- vis$graph_scale

  validate(
    is.numeric(max_per_plot),
    is.logical(show_mean_line),
    is.character(effect_label)
  )

  assert(max_per_plot > 0, "max_boxes_per_plot must be positive")

  factor_by <- resolve_factor_by(df, config, factor_by)

  if (is.null(factor_by) || is.na(factor_by)) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("No suitable grouping variable found. Skipping box plot.")
    }
    return(invisible(list(
      plots = list(),
      factor_by = NULL,
      n_groups = 0L
    )))
  }

  if (!factor_by %in% names(df)) {
    cli::cli_abort("Column {.field {factor_by}} not found in data")
  }

  result <- create_box_plots(
    df = df,
    factor_by = factor_by,
    max_per_plot = max_per_plot,
    theme_name = theme_name,
    show_mean_line = show_mean_line,
    effect_label = effect_label
  )

  if (get_verbosity() >= 3) {
    cli::cli_h3("Box Plot: Effect by {factor_by}")
    n_plots <- length(result$plots)
    for (i in seq_along(result$plots)) {
      if (n_plots > 1) {
        cli::cli_alert_info("Displaying plot {i}/{n_plots}")
      }
      suppressWarnings(print(result$plots[[i]])) # nolint: undesirable_function_linter.
    }
  }

  if (export_graphics) {
    export_box_plots(
      plots = result$plots,
      factor_by = factor_by,
      export_path = export_path,
      graph_scale = graph_scale
    )
  }

  # Add class for clean printing (print method defined in R/print.R)
  class(result) <- c("artma_box_plot", class(result))
  invisible(result)
}


#' Resolve factor_by variable
#'
#' @description
#' Determines which variable to use for grouping, using options > interactive > auto-detect.
#'
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* Data configuration
#' @param factor_by_option *\[character\]* Option value (may be NA)
#'
#' @return *\[character\]* Variable name or NA if none found
#' @keywords internal
resolve_factor_by <- function(df, config, factor_by_option) {
  box::use(
    artma / libs / core / autonomy[should_prompt_user],
    artma / libs / core / utils[get_verbosity]
  )

  if (!is.na(factor_by_option) && factor_by_option %in% names(df)) {
    return(factor_by_option)
  }

  candidates <- detect_factor_candidates(df, config)

  if (length(candidates) == 0) {
    return(NA_character_)
  }

  # For box plots, study labels/IDs are almost always the right choice, so we only prompt
  # at low autonomy levels (1-2). At medium and above, auto-select.
  if (should_prompt_user(required_level = 3) && length(candidates) > 1) {
    box::use(artma / interactive / box_plot[prompt_factor_by_selection])
    return(prompt_factor_by_selection(df, config, candidates))
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_info("Auto-selected grouping variable: {.field {candidates[1]}}")
  }
  candidates[1]
}


#' Detect suitable factor candidates
#'
#' @description
#' Identifies variables that would be good candidates for grouping box plots.
#' Prioritizes "study_label" (or "study_id" fallback) as the default grouping
#' variable since box plots in
#' meta-analysis typically show effect distribution across studies.
#'
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* Data configuration
#'
#' @return *\[character\]* Vector of candidate variable names
#' @keywords internal
detect_factor_candidates <- function(df, config) {
  # For box plots, include study identifiers as candidates (unlike other methods),
  # but exclude numeric identifiers and computed columns
  excluded_names <- c(
    "effect", "se", "t_stat", "study_size",
    "n_obs", "sample_size", "dof", "reg_dof", "precision", "obs_id"
  )

  var_names <- setdiff(names(df), excluded_names)

  if (length(var_names) == 0) {
    return(character(0))
  }

  candidates <- character(0)

  # Study identifier columns are always valid grouping variables; downstream splits by max_boxes_per_plot
  max_levels_default <- 200L
  max_levels_numeric <- 30L
  max_levels_study <- 10000L

  for (var in var_names) {
    col <- df[[var]]
    is_study_var <- var %in% c("study_label", "study_id")

    if (is.factor(col)) {
      n_levels <- nlevels(col)
      cap <- if (is_study_var) max_levels_study else max_levels_default
      if (n_levels >= 2 && n_levels <= cap) {
        candidates <- c(candidates, var)
      }
      next
    }

    if (is.character(col)) {
      n_unique <- length(unique(stats::na.omit(col)))
      cap <- if (is_study_var) max_levels_study else max_levels_default
      if (n_unique >= 2 && n_unique <= cap) {
        candidates <- c(candidates, var)
      }
      next
    }

    if (is.numeric(col) || is.integer(col)) {
      unique_vals <- unique(stats::na.omit(col))
      n_unique <- length(unique_vals)
      cap <- if (is_study_var) max_levels_study else max_levels_numeric
      if (n_unique >= 2 && n_unique <= cap && all(unique_vals == floor(unique_vals))) {
        candidates <- c(candidates, var)
      }
    }
  }

  # Prioritize study labels as the default grouping variable
  # since box plots in meta-analysis typically show effect distribution across studies
  study_vars <- intersect(c("study_label", "study_id"), candidates)
  if (length(study_vars) > 0) {
    other <- setdiff(candidates, study_vars)
    candidates <- c(study_vars, other)
  }

  # Also prioritize variables marked in config
  if (!is.null(config)) {
    prioritized <- character(0)
    for (var in candidates) {
      if (var %in% names(config)) {
        var_cfg <- config[[var]]
        if (isTRUE(var_cfg$box_plot) || isTRUE(var_cfg$is_grouping_var)) {
          prioritized <- c(prioritized, var)
        }
      }
    }
    if (length(prioritized) > 0) {
      other <- setdiff(candidates, prioritized)
      candidates <- c(prioritized, other)
    }
  }

  candidates
}


#' Create box plots
#'
#' @description
#' Creates one or more box plots, splitting data if there are too many unique factor values.
#' When splitting is needed, plots are balanced so each has a similar number of boxes.
#'
#' @param df *\[data.frame\]* The data frame
#' @param factor_by *\[character\]* Variable to group by
#' @param max_per_plot *\[integer\]* Maximum boxes per plot
#' @param theme_name *\[character\]* Theme name
#' @param show_mean_line *\[logical\]* Whether to show mean line
#' @param effect_label *\[character\]* Label for effect axis
#'
#' @return *\[list\]* List with plots, factor_by, and n_groups
#' @keywords internal
create_box_plots <- function(df, factor_by, max_per_plot, theme_name, show_mean_line, effect_label) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.data.frame(df),
    is.character(factor_by),
    factor_by %in% names(df)
  )

  factor_values <- as.character(df[[factor_by]])
  unique_factors <- sort(unique(stats::na.omit(factor_values)))
  n_factors <- length(unique_factors)

  if (n_factors <= max_per_plot) {
    plot <- create_single_box_plot(
      df = df,
      factor_by = factor_by,
      theme_name = theme_name,
      show_mean_line = show_mean_line,
      effect_label = effect_label
    )
    return(list(
      plots = list(plot),
      factor_by = factor_by,
      n_groups = n_factors
    ))
  }

  # Balance factors across plots so each plot has a similar number of boxes
  n_plots <- ceiling(n_factors / max_per_plot)
  chunk_size <- ceiling(n_factors / n_plots)
  factor_chunks <- split(unique_factors, ceiling(seq_along(unique_factors) / chunk_size))

  datasets <- lapply(factor_chunks, function(chunk_factors) {
    subset_mask <- factor_values %in% chunk_factors
    df[subset_mask, , drop = FALSE]
  })

  plots <- lapply(datasets, function(subset_df) {
    create_single_box_plot(
      df = subset_df,
      factor_by = factor_by,
      theme_name = theme_name,
      show_mean_line = show_mean_line,
      effect_label = effect_label
    )
  })

  list(
    plots = plots,
    factor_by = factor_by,
    n_groups = n_factors
  )
}


#' Create a single box plot
#'
#' @description
#' Creates a single ggplot2 box plot.
#'
#' @param df *\[data.frame\]* The data frame
#' @param factor_by *\[character\]* Variable to group by
#' @param theme_name *\[character\]* Theme name
#' @param show_mean_line *\[logical\]* Whether to show mean line
#' @param effect_label *\[character\]* Label for effect axis
#'
#' @return *\[ggplot\]* A ggplot2 object
#' @keywords internal
create_single_box_plot <- function(df, factor_by, theme_name, show_mean_line, effect_label) {
  box::use(
    artma / visualization / colors[get_colors, get_vline_color],
    artma / visualization / theme[get_theme]
  )

  colors <- get_colors(theme_name, "box_plot")
  vline_color <- get_vline_color(theme_name)
  plot_theme <- get_theme(theme_name)

  factor_values <- as.character(df[[factor_by]])
  factor_levels <- rev(sort(unique(stats::na.omit(factor_values))))
  factor_by_verbose <- gsub("_", " ", factor_by)

  plot_df <- df
  plot_df[[".factor"]] <- factor(factor_values, levels = factor_levels)

  p <- ggplot2::ggplot(
    data = plot_df,
    ggplot2::aes(y = .data$effect, x = .data$.factor)
  ) +
    ggplot2::geom_boxplot(
      outlier.colour = colors$outlier,
      outlier.shape = 21,
      outlier.fill = colors$outlier,
      fill = colors$fill,
      color = colors$border
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = NULL,
      y = paste("Effect of", tolower(effect_label)),
      x = paste("Grouped by", factor_by_verbose)
    ) +
    plot_theme

  if (show_mean_line) {
    mean_effect <- mean(df$effect, na.rm = TRUE)
    p <- p + ggplot2::geom_hline(
      yintercept = mean_effect,
      color = vline_color,
      linewidth = 0.85
    )
  }

  p
}


#' Export box plots to files
#'
#' @param plots *\[list\]* List of ggplot objects
#' @param factor_by *\[character\]* Factor variable name
#' @param export_path *\[character\]* Directory path
#' @param graph_scale *\[numeric\]* Scale factor
#'
#' @return NULL (invisibly)
#' @keywords internal
export_box_plots <- function(plots, factor_by, export_path, graph_scale) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / visualization / export[save_plot, build_export_filename, ensure_export_dir]
  )

  ensure_export_dir(export_path)
  n_plots <- length(plots)
  use_indexing <- n_plots > 1

  for (i in seq_along(plots)) {
    index <- if (use_indexing) i else NULL
    filename <- build_export_filename("box_plot", factor_by, index = index)
    full_path <- file.path(export_path, filename)

    save_plot(
      plot = plots[[i]],
      path = full_path,
      width = 800,
      height = 1100,
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
  box_plot,
  stage = "box_plot",
  key_builder = function(...) build_data_cache_signature()
)

box::export(box_plot, run)
