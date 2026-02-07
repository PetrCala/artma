#' @title Interactive Prima Facie Graphs Group Selection
#' @description
#' Interactive prompts for selecting which variable groups to include
#' in prima facie graph visualizations.


#' Prompt user to select variable groups for prima facie graphs
#'
#' @description
#' Presents an interactive multi-select menu for choosing which detected
#' variable groups to visualize. Respects autonomy level settings.
#'
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* Data configuration
#' @param group_descriptors *\[list\]* List of group descriptor lists,
#' each with group_id, var_names, group_base, group_type
#'
#' @return *\[character\]* Vector of selected group IDs
prompt_group_selection <- function(df, config, group_descriptors) {
  box::use(
    artma / libs / core / validation[validate],
    artma / libs / core / autonomy[should_prompt_user]
  )

  validate(is.data.frame(df), is.list(group_descriptors), length(group_descriptors) > 0)

  if (!should_prompt_user(required_level = 3)) {
    return(vapply(group_descriptors, function(g) g$group_id, character(1)))
  }

  cli::cli_h2("Prima Facie Graphs - Group Selection")
  cli::cli_text(
    "Prima facie graphs show the distribution of effect sizes grouped by",
    " variable categories. Select which variable groups to visualize."
  )
  cli::cat_line()

  display_names <- vapply(group_descriptors, function(g) {
    n_vars <- length(g$var_names)
    type_label <- switch(g$group_type,
      dummy = "dummy",
      categorical = "categorical",
      g$group_type
    )
    verbose_base <- gsub("_", " ", g$group_base)
    sprintf("%s (%s, %d variables)", verbose_base, type_label, n_vars)
  }, character(1))

  group_ids <- vapply(group_descriptors, function(g) g$group_id, character(1))

  selected_idx <- climenu::checkbox(
    choices = display_names,
    prompt = "Select groups to visualize (use SPACE to select, ENTER to confirm)",
    return_index = TRUE,
    allow_select_all = TRUE
  )

  if (rlang::is_empty(selected_idx) || length(selected_idx) == 0) {
    cli::cli_alert_info("No selection made. Using all detected groups.")
    return(group_ids)
  }

  group_ids[selected_idx]
}


box::export(prompt_group_selection)
