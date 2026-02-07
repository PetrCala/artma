#' @title Prima Facie Graphs
#' @description
#' Create density or histogram plots of effect distributions grouped by variable
#' categories. These "prima facie" visualizations help inspect whether effect
#' sizes differ across subgroups (e.g., published vs. unpublished, country groups).
prima_facie_graphs <- function(df) {
  box::use(
    artma / data_config / read[get_data_config],
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[assert, validate, validate_columns],
    artma / libs / core / autonomy[should_prompt_user],
    artma / options / index[get_option_group],
    artma / variable / detection[detect_variable_groups],
    artma / visualization / options[get_visualization_options],
    artma / visualization / export[save_plot, build_export_filename, ensure_export_dir]
  )

  validate(is.data.frame(df))
  validate_columns(df, c("effect"))

  if (get_verbosity() >= 4) {
    cli::cli_alert_info("Creating prima facie graph visualizations")
  }

  config <- get_data_config()
  opt <- get_option_group("artma.methods.prima_facie_graphs")
  vis <- get_visualization_options()

  graph_type <- opt$type %||% "automatic"
  hide_outliers <- opt$hide_outliers %||% TRUE
  n_bins <- opt$n_bins %||% 80L
  legend_font_size <- opt$legend_font_size %||% 14
  theme_name <- vis$theme
  export_graphics <- vis$export_graphics
  export_path <- vis$export_path
  graph_scale <- vis$graph_scale

  validate(
    is.character(graph_type),
    is.logical(hide_outliers),
    is.numeric(n_bins), n_bins > 0,
    is.numeric(legend_font_size)
  )
  assert(
    graph_type %in% c("density", "histogram", "automatic"),
    "type must be one of: density, histogram, automatic"
  )

  groups <- resolve_groups(df, config, opt)

  if (length(groups) == 0) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning(
        "No suitable variable groups found for prima facie graphs. Skipping."
      )
    }
    return(invisible(list(plots = list(), groups = character(0))))
  }

  plots <- list()
  group_names <- character(0)

  for (group in groups) {
    group_id <- group$group_id
    group_vars <- group$var_names
    group_base <- group$group_base

    group_col <- build_group_column(df, group_vars, config)

    if (is.null(group_col) || length(unique(group_col)) < 2) {
      if (get_verbosity() >= 4) {
        cli::cli_alert_info("Skipping group {.field {group_base}}: fewer than 2 unique values")
      }
      next
    }

    plot <- create_prima_facie_plot(
      df = df,
      group_col = group_col,
      group_name = group_base,
      graph_type = graph_type,
      hide_outliers = hide_outliers,
      n_bins = n_bins,
      legend_font_size = legend_font_size,
      theme_name = theme_name
    )

    plot_name <- paste0("prima_facie_", group_base)
    plots[[plot_name]] <- plot
    group_names <- c(group_names, group_base)
  }

  if (length(plots) == 0) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("No prima facie graphs could be created.")
    }
    return(invisible(list(plots = list(), groups = character(0))))
  }

  if (get_verbosity() >= 3) {
    cli::cli_h3("Prima Facie Graphs")
    for (i in seq_along(plots)) {
      if (length(plots) > 1) {
        cli::cli_alert_info(
          "Displaying plot {i}/{length(plots)}: {.field {names(plots)[i]}}"
        )
      }
      suppressWarnings(print(plots[[i]])) # nolint: undesirable_function_linter.
    }
  }

  if (export_graphics) {
    export_prima_facie_plots(
      plots = plots,
      group_names = group_names,
      export_path = export_path,
      graph_scale = graph_scale
    )
  }

  result <- list(plots = plots, groups = group_names)
  class(result) <- c("artma_prima_facie_graphs", class(result))
  invisible(result)
}


#' Resolve which variable groups to plot
#'
#' @description
#' Determines which variable groups to create prima facie graphs for,
#' using options > interactive selection > auto-detection.
#'
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* Data configuration
#' @param opt *\[list\]* Method-specific options
#'
#' @return *\[list\]* List of group descriptors, each with group_id, var_names, group_base
#' @keywords internal
resolve_groups <- function(df, config, opt) {
  box::use(
    artma / libs / core / autonomy[should_prompt_user],
    artma / libs / core / utils[get_verbosity],
    artma / variable / detection[detect_variable_groups]
  )

  groups_option <- opt$groups %||% NA_character_

  all_groups <- detect_variable_groups(df, config = config)

  # Filter to meaningful group types for prima facie visualization
  suitable_types <- c("dummy", "categorical")
  suitable <- all_groups[all_groups$group_type %in% suitable_types, , drop = FALSE]

  if (nrow(suitable) == 0) {
    return(list())
  }

  # Build group descriptors from detection results
  unique_group_ids <- unique(suitable$group_id)
  group_descriptors <- lapply(unique_group_ids, function(gid) {
    members <- suitable[suitable$group_id == gid, , drop = FALSE]
    list(
      group_id = gid,
      var_names = members$var_name,
      group_base = members$group_base[1],
      group_type = members$group_type[1]
    )
  })

  # If user specified groups, filter to those
  if (!identical(groups_option, NA_character_) && !is.na(groups_option)) {
    requested <- trimws(strsplit(groups_option, ",")[[1]])
    group_descriptors <- Filter(
      function(g) g$group_id %in% requested || g$group_base %in% requested,
      group_descriptors
    )
    return(group_descriptors)
  }

  # Interactive selection at low autonomy levels
  if (should_prompt_user(required_level = 3) && length(group_descriptors) > 1) {
    box::use(artma / interactive / prima_facie_graphs[prompt_group_selection])
    selected_ids <- prompt_group_selection(df, config, group_descriptors)
    group_descriptors <- Filter(
      function(g) g$group_id %in% selected_ids,
      group_descriptors
    )
    return(group_descriptors)
  }

  if (get_verbosity() >= 3) {
    n <- length(group_descriptors)
    cli::cli_alert_info(
      "Auto-detected {n} variable group{?s} for prima facie graphs"
    )
  }

  group_descriptors
}


#' Build a categorical group column from related variables
#'
#' @description
#' Creates a single categorical vector from a set of related variables,
#' suitable for use as a grouping factor in plots.
#'
#' @param df *\[data.frame\]* The data frame
#' @param group_vars *\[character\]* Variable names in this group
#' @param config *\[list\]* Data configuration (for verbose names)
#'
#' @return *\[character\]* A character vector of group labels (same length as nrow(df))
#' @keywords internal
build_group_column <- function(df, group_vars, config) {
  box::use(artma / libs / core / validation[validate])

  validate(is.data.frame(df), is.character(group_vars), length(group_vars) > 0)

  # Filter to variables that exist in df
  group_vars <- intersect(group_vars, names(df))
  if (length(group_vars) == 0) return(NULL)

  get_verbose <- function(var_name) {
    if (!is.null(config) && var_name %in% names(config)) {
      vn <- config[[var_name]]$var_name_verbose
      if (!is.null(vn) && nzchar(vn)) return(vn)
    }
    gsub("_", " ", var_name)
  }

  if (length(group_vars) > 1) {
    # Multiple columns (dummy group): pick the column name with max value per row
    var_data <- df[, group_vars, drop = FALSE]
    if (!all(vapply(var_data, is.numeric, logical(1)))) return(NULL)
    verbose_names <- vapply(group_vars, get_verbose, character(1))
    colnames(var_data) <- verbose_names
    group_col <- apply(var_data, 1, function(x) names(x)[which.max(x)])
    return(group_col)
  }

  # Single variable
  col <- df[[group_vars]]
  verbose_name <- get_verbose(group_vars)

  if (is.factor(col) || is.character(col)) {
    return(as.character(col))
  }

  if (is.numeric(col) || is.integer(col)) {
    unique_vals <- unique(stats::na.omit(col))
    if (length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))) {
      return(paste(verbose_name, "=", as.integer(col)))
    }
    med <- stats::median(col, na.rm = TRUE)
    direction <- vapply(col, function(x) if (x >= med) ">=" else "<", character(1))
    return(paste(verbose_name, direction, round(med, 2)))
  }

  NULL
}


#' Create a single prima facie plot
#'
#' @param df *\[data.frame\]* The data frame (must contain "effect" column)
#' @param group_col *\[character\]* Group labels vector (same length as nrow(df))
#' @param group_name *\[character\]* Base name of the group (for title/labels)
#' @param graph_type *\[character\]* One of "density", "histogram", "automatic"
#' @param hide_outliers *\[logical\]* Whether to trim outlier bins
#' @param n_bins *\[integer\]* Number of bins
#' @param legend_font_size *\[numeric\]* Legend text size
#' @param theme_name *\[character\]* Theme name
#'
#' @return *\[ggplot\]* A ggplot2 object
#' @keywords internal
create_prima_facie_plot <- function(df, group_col, group_name, graph_type,
                                    hide_outliers, n_bins, legend_font_size,
                                    theme_name) {
  box::use(
    artma / visualization / colors[get_colors],
    artma / visualization / theme[get_theme]
  )

  plot_df <- data.frame(
    effect = df$effect,
    group = group_col,
    stringsAsFactors = FALSE
  )
  plot_df <- plot_df[!is.na(plot_df$effect) & !is.na(plot_df$group), , drop = FALSE]

  bin_width <- diff(range(plot_df$effect)) / n_bins

  if (hide_outliers && bin_width > 0) {
    x_min <- min(plot_df$effect) + bin_width
    x_max <- max(plot_df$effect) - bin_width
    plot_df <- plot_df[plot_df$effect > x_min & plot_df$effect < x_max, , drop = FALSE]
  }

  # Determine effective graph type
  effective_type <- graph_type
  if (graph_type == "automatic") {
    n_groups <- length(unique(plot_df$group))
    effective_type <- if (n_groups > 3) "density" else "histogram"
  }

  palette <- get_colors(theme_name, "prima_facie", submethod = effective_type)
  plot_theme <- get_theme(theme_name)

  group_label <- gsub("_", " ", group_name)

  if (effective_type == "histogram") {
    p <- ggplot2::ggplot(
      data = plot_df,
      ggplot2::aes(x = .data$effect, y = ggplot2::after_stat(density), fill = .data$group)
    ) +
      ggplot2::geom_histogram(binwidth = bin_width) +
      ggplot2::scale_fill_brewer(palette = palette, name = group_label)
  } else {
    p <- ggplot2::ggplot(
      data = plot_df,
      ggplot2::aes(x = .data$effect, y = ggplot2::after_stat(density), color = .data$group)
    ) +
      ggplot2::geom_density(alpha = 0.2, linewidth = 1) +
      ggplot2::scale_color_brewer(palette = palette, name = group_label)
  }

  p <- p +
    ggplot2::labs(x = "Effect", y = "Density") +
    plot_theme +
    ggplot2::theme(
      legend.position = c(0.98, 0.98),
      legend.justification = c(1, 1),
      legend.direction = "vertical",
      legend.text = ggplot2::element_text(size = legend_font_size),
      legend.background = ggplot2::element_rect(fill = "white", color = "grey80")
    )

  p
}


#' Export prima facie plots to files
#'
#' @param plots *\[list\]* Named list of ggplot objects
#' @param group_names *\[character\]* Vector of group base names
#' @param export_path *\[character\]* Directory path
#' @param graph_scale *\[numeric\]* Scale factor
#'
#' @return NULL (invisibly)
#' @keywords internal
export_prima_facie_plots <- function(plots, group_names, export_path, graph_scale) {
  box::use(
    artma / visualization / export[save_plot, build_export_filename, ensure_export_dir]
  )

  ensure_export_dir(export_path)

  for (i in seq_along(plots)) {
    filename <- build_export_filename("prima_facie", group_names[i])
    full_path <- file.path(export_path, filename)

    save_plot(
      plot = plots[[i]],
      path = full_path,
      width = 800,
      height = 666,
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
  prima_facie_graphs,
  stage = "prima_facie_graphs",
  key_builder = function(...) build_data_cache_signature()
)

box::export(prima_facie_graphs, run)
