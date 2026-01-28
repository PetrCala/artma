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
    "<box_plot result: {n_plots} {plot_word}, {x$n_groups} groups, grouped by {.field {x$factor_by}}>"
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
  median_info <- if (x$used_study_medians) " (study medians)" else ""

  cli::cli_text(
    "<funnel_plot result: {x$n_points} points{median_info}, {x$n_outliers_removed} outliers removed>"
  )
  cli::cli_text("Access plot via {.code $plot}")

  invisible(x)
}
