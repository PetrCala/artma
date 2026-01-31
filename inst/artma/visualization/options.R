#' @title Visualization Options
#' @description
#' Centralized access to visualization options (theme, export settings).

#' Get visualization options
#'
#' @description
#' Retrieves the centralized visualization options from the global options namespace.
#' All visualization methods should use this function instead of reading theme/export
#' options from their own method-specific option groups.
#'
#' @return *\[list\]* A list with elements: theme, export_graphics, export_path, graph_scale
#'
#' @examples
#' \dontrun{
#' vis <- get_visualization_options()
#' vis$theme          # "blue"
#' vis$export_graphics # FALSE
#' vis$export_path    # "./results/graphic"
#' vis$graph_scale    # 2
#' }
get_visualization_options <- function() {
  box::use(artma / options / index[get_option_group])
  box::use(artma / visualization / colors[validate_theme, VALID_THEMES])
  box::use(artma / libs / core / validation[assert])

  opt <- get_option_group("artma.visualization")

  theme <- opt$theme %||% "blue"
  export_graphics <- opt$export_graphics %||% FALSE
  export_path <- opt$export_path %||% "./results/graphic"
  graph_scale <- opt$graph_scale %||% 2

  assert(
    theme %in% VALID_THEMES,
    paste0("theme must be one of: ", paste(VALID_THEMES, collapse = ", "))
  )
  assert(is.logical(export_graphics), "export_graphics must be logical")
  assert(is.character(export_path), "export_path must be a character string")
  assert(is.numeric(graph_scale) && graph_scale > 0, "graph_scale must be a positive number")

  list(
    theme = theme,
    export_graphics = export_graphics,
    export_path = export_path,
    graph_scale = graph_scale
  )
}


box::export(get_visualization_options)
