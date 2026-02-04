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
#' vis$theme # "blue"
#' vis$export_graphics # FALSE
#' vis$export_path # "graphics"
#' vis$graph_scale # 2
#' }
get_visualization_options <- function() {
  box::use(artma / options / index[get_option_group])
  box::use(artma / visualization / colors[validate_theme, VALID_THEMES])
  box::use(artma / libs / core / validation[assert])

  opt <- get_option_group("artma.visualization")

  theme <- opt$theme %||% "blue"
  export_graphics <- opt$export_graphics %||% TRUE
  export_path <- opt$export_path %||% "graphics"
  graph_scale <- opt$graph_scale %||% 2

  assert(
    theme %in% VALID_THEMES,
    paste0("theme must be one of: ", paste(VALID_THEMES, collapse = ", "))
  )
  assert(is.logical(export_graphics), "export_graphics must be logical")
  assert(is.character(export_path), "export_path must be a character string")
  assert(is.numeric(graph_scale) && graph_scale > 0, "graph_scale must be a positive number")

  # Resolve export_path relative to the unified output directory
  save_results <- getOption("artma.output.save_results", TRUE)
  if (isTRUE(save_results)) {
    box::use(artma / output / export[resolve_output_dir])
    output_dir <- resolve_output_dir()
    export_path <- file.path(output_dir, export_path)
  }

  list(
    theme = theme,
    export_graphics = export_graphics,
    export_path = export_path,
    graph_scale = graph_scale
  )
}


#' Set visualization options
#'
#' @description
#' Sets one or more visualization options in the R options namespace.
#' Only provided (non-NULL) arguments are changed; others are left as-is.
#'
#' @param theme *\[character, optional\]* Theme name
#' @param export_graphics *\[logical, optional\]* Whether to export plots
#' @param export_path *\[character, optional\]* Directory for exported plots
#' @param graph_scale *\[numeric, optional\]* Scale factor for exports
#'
#' @return *\[list\]* Previous values (invisibly)
#' @keywords internal
set_visualization_option <- function(theme = NULL, export_graphics = NULL,
                                     export_path = NULL, graph_scale = NULL) {
  box::use(artma / visualization / colors[validate_theme, VALID_THEMES])
  box::use(artma / libs / core / validation[assert])

  previous <- get_visualization_options()

  if (!is.null(theme)) {
    assert(
      theme %in% VALID_THEMES,
      paste0("theme must be one of: ", paste(VALID_THEMES, collapse = ", "))
    )
    options("artma.visualization.theme" = theme)
  }

  if (!is.null(export_graphics)) {
    assert(is.logical(export_graphics), "export_graphics must be logical")
    options("artma.visualization.export_graphics" = export_graphics)
  }

  if (!is.null(export_path)) {
    assert(is.character(export_path), "export_path must be a character string")
    options("artma.visualization.export_path" = export_path)
  }

  if (!is.null(graph_scale)) {
    assert(is.numeric(graph_scale) && graph_scale > 0, "graph_scale must be a positive number")
    options("artma.visualization.graph_scale" = graph_scale)
  }

  invisible(previous)
}


#' Get valid theme names
#'
#' @description
#' Returns the character vector of valid theme names.
#'
#' @return *\[character\]* Vector of valid theme names
#' @keywords internal
get_valid_themes <- function() {
  box::use(artma / visualization / colors[VALID_THEMES])
  VALID_THEMES
}


box::export(get_visualization_options, set_visualization_option, get_valid_themes)
