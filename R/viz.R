#' @title Get Visualization Settings
#' @description Get the current visualization settings.
#'   Returns all settings as a list, or a single setting by name.
#' @param option *\[character, optional\]* Name of a specific option to retrieve.
#'   One of: `"theme"`, `"export_graphics"`, `"export_path"`, `"graph_scale"`.
#'   If NULL (default), returns all options as a named list.
#' @return A named list of all visualization settings, or a single setting value.
#' @export
#' @examples
#' \dontrun{
#' # Get all visualization settings
#' viz.get()
#'
#' # Get just the current theme
#' viz.get("theme")
#'
#' # Get export path
#' viz.get("export_path")
#' }
viz.get <- function(option = NULL) {
  box::use(artma / visualization / options[get_visualization_options])
  box::use(artma / libs / core / validation[assert])

  opts <- get_visualization_options()

  if (is.null(option)) {
    return(opts)
  }

  valid_names <- c("theme", "export_graphics", "export_path", "graph_scale")
  assert(
    option %in% valid_names,
    paste0("Unknown option '", option, "'. Must be one of: ", paste(valid_names, collapse = ", "))
  )

  opts[[option]]
}

#' @title Set Visualization Settings
#' @description Set visualization options for the current session.
#'   Only provided arguments are changed; others remain unchanged.
#' @param theme *\[character, optional\]* Color theme. Use `viz.themes()` to see available themes.
#' @param export_graphics *\[logical, optional\]* If TRUE, export plots to files.
#' @param export_path *\[character, optional\]* Directory path for exported plots.
#' @param graph_scale *\[numeric, optional\]* Scaling factor for exported graphics.
#'   Values > 1 increase resolution.
#' @return Previous settings (invisibly), enabling easy restoration.
#' @export
#' @examples
#' \dontrun{
#' # Change theme
#' viz.set(theme = "purple")
#'
#' # Enable export with custom path
#' viz.set(export_graphics = TRUE, export_path = "./output/plots")
#'
#' # Save and restore settings
#' prev <- viz.set(theme = "red")
#' # ... do work ...
#' do.call(viz.set, prev)
#' }
viz.set <- function(theme = NULL, export_graphics = NULL,
                    export_path = NULL, graph_scale = NULL) {
  box::use(
    artma / visualization / options[
      set_visualization_option,
      get_valid_themes
    ],
    artma / libs / core / utils[get_verbosity]
  )

  no_args <- is.null(theme) && is.null(export_graphics) &&
    is.null(export_path) && is.null(graph_scale)

  if (no_args && interactive()) {
    themes <- get_valid_themes()
    theme <- climenu::select(
      choices = themes,
      prompt = "Select a theme:"
    )
    if (is.null(theme)) {
      cli::cli_alert_info("Theme selection cancelled.")
      return(invisible(NULL))
    }
  }

  previous <- set_visualization_option(
    theme = theme,
    export_graphics = export_graphics,
    export_path = export_path,
    graph_scale = graph_scale
  )

  if (get_verbosity() >= 3) {
    changed <- c()
    if (!is.null(theme)) changed <- c(changed, paste0("theme = ", theme))
    if (!is.null(export_graphics)) changed <- c(changed, paste0("export_graphics = ", export_graphics))
    if (!is.null(export_path)) changed <- c(changed, paste0("export_path = ", export_path))
    if (!is.null(graph_scale)) changed <- c(changed, paste0("graph_scale = ", graph_scale))

    if (length(changed) > 0) {
      cli::cli_alert_success("Visualization updated: {paste(changed, collapse = ', ')}")
    }
  }

  invisible(previous)
}

#' @title List Available Themes
#' @description Get the names of all available visualization themes.
#' @return *\[character\]* Vector of valid theme names.
#' @export
#' @examples
#' \dontrun{
#' viz.themes()
#' # [1] "blue" "yellow" "green" "red" "purple"
#' }
viz.themes <- function() {
  box::use(artma / visualization / options[get_valid_themes])
  get_valid_themes()
}
