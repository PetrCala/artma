#' @title Autonomy Level Management
#' @description Functions for managing the autonomy level of the package.
#'   Autonomy controls how much user interaction is required during analysis.
#'   Higher levels mean less user interaction and more automatic decision-making.

#' @title Get autonomy levels definition
#' @description Returns the definition of all autonomy levels.
#' @return A list of autonomy level definitions.
#' @export
get_autonomy_levels <- function() {
  list(
    "1" = list(
      name = "Minimal",
      description = "Maximum user control - prompt for all optional decisions",
      prompt_frequency = "high"
    ),
    "2" = list(
      name = "Low",
      description = "Frequent prompts - ask for most non-critical decisions",
      prompt_frequency = "medium-high"
    ),
    "3" = list(
      name = "Medium",
      description = "Balanced - prompt for important decisions only",
      prompt_frequency = "medium"
    ),
    "4" = list(
      name = "High",
      description = "Mostly autonomous - minimal prompts for critical decisions only (default)",
      prompt_frequency = "low"
    ),
    "5" = list(
      name = "Full",
      description = "Fully autonomous - no prompts, use all defaults and auto-detection",
      prompt_frequency = "none"
    )
  )
}

#' @title Get autonomy level
#' @description Get the current autonomy level from options.
#'   In non-interactive mode, returns 5 if level is not set.
#' @return *\[integer\]* The current autonomy level (1-5), or NULL if not set (interactive mode only).
#' @export
get_autonomy_level <- function() {
  box::use(artma / const[CONST])

  level <- getOption(
    paste0(CONST$PACKAGE_NAME, ".autonomy.level"),
    default = NULL
  )

  # In non-interactive mode, default to level 5 if not set
  if (is.null(level) && !interactive()) {
    return(5L)
  }

  if (is.null(level)) {
    return(NULL)
  }

  as.integer(level)
}

#' @title Set autonomy level
#' @description Set the autonomy level in the R options namespace.
#' @param level *\[integer\]* The autonomy level to set (1-5).
#' @return `NULL` (invisible)
#' @export
set_autonomy_level <- function(level) {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[validate]
  )

  validate(
    is.numeric(level),
    length(level) == 1,
    level >= 1,
    level <= 5,
    level == as.integer(level)  # Must be a whole number
  )

  level <- as.integer(level)
  opt_name <- paste0(CONST$PACKAGE_NAME, ".autonomy.level")
  options(stats::setNames(list(level), opt_name))

  invisible(NULL)
}

#' @title Check if autonomy level is set
#' @description Check if the autonomy level has been set.
#' @return *\[logical\]* TRUE if the autonomy level is set, FALSE otherwise.
#' @export
is_autonomy_level_set <- function() {
  !is.null(get_autonomy_level())
}

#' @title Should prompt user
#' @description Determine whether the user should be prompted based on the
#'   current autonomy level and the required level for a given decision.
#' @param required_level *\[integer\]* The minimum autonomy level below which
#'   the user should be prompted. For example, if `required_level = 4`, the user
#'   will be prompted only if the current autonomy level is 3 or lower.
#' @return *\[logical\]* TRUE if the user should be prompted, FALSE otherwise.
#' @export
should_prompt_user <- function(required_level = 4) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.numeric(required_level),
    length(required_level) == 1,
    required_level >= 1,
    required_level <= 5
  )

  # If in non-interactive mode, never prompt
  if (!interactive()) {
    return(FALSE)
  }

  current_level <- get_autonomy_level()

  # If no level is set, we need to prompt (interactive mode only)
  if (is.null(current_level)) {
    return(TRUE)
  }

  # Prompt if current level is below the required level
  current_level < required_level
}

#' @title Get autonomy level description
#' @description Get a human-readable description of an autonomy level.
#' @param level *\[integer, optional\]* The autonomy level. If NULL, uses the current level.
#' @return *\[character\]* A description of the autonomy level.
#' @export
get_autonomy_description <- function(level = NULL) {
  box::use(artma / libs / core / validation[validate])

  if (is.null(level)) {
    level <- get_autonomy_level()
  }

  if (is.null(level)) {
    return("Not set")
  }

  validate(
    is.numeric(level),
    length(level) == 1,
    level >= 1,
    level <= 5
  )

  level <- as.integer(level)
  levels <- get_autonomy_levels()
  level_def <- levels[[as.character(level)]]

  sprintf(
    "Level %d - %s: %s",
    level,
    level_def$name,
    level_def$description
  )
}

#' @title Is fully autonomous
#' @description Check if the package is running in fully autonomous mode (level 5).
#' @return *\[logical\]* TRUE if fully autonomous, FALSE otherwise.
#' @export
is_fully_autonomous <- function() {
  level <- get_autonomy_level()
  if (is.null(level)) {
    return(FALSE)
  }
  level == 5
}

#' @title Get default autonomy level
#' @description Get the default autonomy level.
#'   The default is always 4 (High) for option file creation.
#'   In non-interactive runtime, level 5 is enforced regardless.
#' @return *\[integer\]* The default autonomy level.
#' @export
get_default_autonomy_level <- function() {
  4L # High - mostly autonomous with minimal prompts
}

box::export(
  get_autonomy_level,
  get_autonomy_levels,
  get_autonomy_description,
  get_default_autonomy_level,
  is_autonomy_level_set,
  is_fully_autonomous,
  set_autonomy_level,
  should_prompt_user
)
