#' Print method for box_plot results
#'
#' @param x An artma_box_plot object
#' @param ... Additional arguments (ignored)
#' @return x invisibly
#' @export
print.artma_box_plot <- function(x, ...) {
  n_plots <- length(x$plots)
  plot_word <- if (n_plots == 1) "plot" else "plots"

  cli::cli_text(
    "<box_plot result: {n_plots} {plot_word}, {x$meta$n_groups} groups, grouped by {.field {x$meta$factor_by}}>"
  )
  cli::cli_text("Access plots via {.code $plots[[1]]}, {.code $plots[[2]]}, etc.")

  invisible(x)
}


#' Print method for funnel_plot results
#'
#' @param x An artma_funnel_plot object
#' @param ... Additional arguments (ignored)
#' @return x invisibly
#' @export
print.artma_funnel_plot <- function(x, ...) {
  median_info <- if (isTRUE(x$meta$used_study_medians)) " (study medians)" else ""

  cli::cli_text(
    "<funnel_plot result: {x$meta$n_points} points{median_info}, {x$meta$n_outliers_removed} outliers removed ({x$meta$pct_filtered}%)>"
  )
  cli::cli_text("Access plot via {.code $plots$funnel_plot}")

  invisible(x)
}


#' Print method for t_stat_histogram results
#'
#' @param x An artma_t_stat_histogram object
#' @param ... Additional arguments (ignored)
#' @return x invisibly
#' @export
print.artma_t_stat_histogram <- function(x, ...) {
  close_up_info <- if (isTRUE(x$meta$close_up_enabled)) " + close-up" else ""
  mean_info <- round(x$meta$mean_t_stat, 3)

  cli::cli_text(paste0(
    "<t_stat_histogram result: {x$meta$n_observations} observations, ",
    "mean t = {mean_info}{close_up_info}>"
  ))
  cli::cli_text(
    "Access plots via {.code $plots$plot_main} and {.code $plots$plot_close_up}"
  )

  invisible(x)
}
