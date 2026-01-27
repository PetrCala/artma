#' @title Interactive Box Plot Variable Selection
#' @description
#' Interactive prompts for box plot configuration.


#' Prompt user to select factor_by variable
#'
#' @description
#' Presents an interactive menu for selecting the grouping variable for box plots.
#' Respects autonomy level settings.
#'
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* Data configuration
#' @param candidates *\[character\]* Vector of candidate variable names
#'
#' @return *\[character\]* Selected variable name
prompt_factor_by_selection <- function(df, config, candidates) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / autonomy[should_prompt_user]
  )

  validate(is.data.frame(df), is.character(candidates), length(candidates) > 0)

  if (!should_prompt_user(required_level = 4)) {
    return(candidates[1])
  }

  cli::cli_h2("Box Plot - Variable Selection")
  cli::cli_text("Select a variable to group the box plots by.")
  cli::cat_line()

  display_names <- vapply(candidates, function(var) {
    n_unique <- length(unique(stats::na.omit(df[[var]])))
    verbose_name <- get_verbose_name(var, config)
    if (verbose_name != var) {
      sprintf("%s (%s) - %d groups", var, verbose_name, n_unique)
    } else {
      sprintf("%s - %d groups", var, n_unique)
    }
  }, character(1))

  selected <- climenu::select(
    choices = display_names,
    prompt = "Select grouping variable",
    selected = 1
  )

  if (rlang::is_empty(selected) || is.null(selected)) {
    cli::cli_alert_info("No selection made. Using default: {.field {candidates[1]}}")
    return(candidates[1])
  }

  candidates[selected]
}


#' Get verbose name from config
#'
#' @param var_name *\[character\]* Variable name
#' @param config *\[list\]* Data configuration
#' @return *\[character\]* Verbose name or original name if not found
#' @keywords internal
get_verbose_name <- function(var_name, config) {
  if (is.null(config) || !var_name %in% names(config)) {
    return(var_name)
  }

  var_config <- config[[var_name]]
  if (!is.null(var_config$var_name_verbose) && nzchar(var_config$var_name_verbose)) {
    var_config$var_name_verbose
  } else {
    var_name
  }
}


box::export(prompt_factor_by_selection)
