#' @title Get Autonomy Level
#' @description Get the current autonomy level.
#'   Autonomy controls how much user interaction is required during analysis.
#'   Higher levels mean less user interaction and more automatic decision-making.
#' @return *\[integer or NULL\]* The current autonomy level (1-5), or NULL if not set.
#' @export
#' @examples
#' \dontrun{
#' # Get current autonomy level
#' level <- autonomy.get()
#' print(level)
#' }
autonomy.get <- function() {
  box::use(artma / libs / autonomy[get_autonomy_level])
  get_autonomy_level()
}

#' @title Set Autonomy Level
#' @description Set the autonomy level for the current session.
#'   This setting controls how much user interaction is required during analysis.
#' @param level *\[integer\]* The autonomy level to set (1-5).
#'   - 1 (Minimal): Maximum user control - prompt for all optional decisions
#'   - 2 (Low): Frequent prompts - ask for most non-critical decisions
#'   - 3 (Medium): Balanced - prompt for important decisions only
#'   - 4 (High): Mostly autonomous - minimal prompts for critical decisions only (default)
#'   - 5 (Full): Fully autonomous - no prompts, use all defaults and auto-detection
#' @return `NULL` (invisible)
#' @export
#' @examples
#' \dontrun{
#' # Set to fully autonomous mode
#' autonomy.set(5)
#'
#' # Set to balanced mode
#' autonomy.set(3)
#' }
autonomy.set <- function(level) {
  box::use(artma / libs / autonomy[set_autonomy_level])
  set_autonomy_level(level)
}

#' @title Check if Autonomy Level is Set
#' @description Check if the autonomy level has been configured.
#' @return *\[logical\]* TRUE if the autonomy level is set, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' if (!autonomy.is_set()) {
#'   message("Autonomy level not configured")
#' }
#' }
autonomy.is_set <- function() {
  box::use(artma / libs / autonomy[is_autonomy_level_set])
  is_autonomy_level_set()
}

#' @title Get Autonomy Level Description
#' @description Get a human-readable description of an autonomy level.
#' @param level *\[integer, optional\]* The autonomy level (1-5).
#'   If NULL, describes the current level.
#' @return *\[character\]* A description of the autonomy level.
#' @export
#' @examples
#' \dontrun{
#' # Get description of current level
#' desc <- autonomy.describe()
#' print(desc)
#'
#' # Get description of a specific level
#' desc <- autonomy.describe(5)
#' print(desc)
#' }
autonomy.describe <- function(level = NULL) {
  box::use(artma / libs / autonomy[get_autonomy_description])
  get_autonomy_description(level)
}

#' @title List Available Autonomy Levels
#' @description Get information about all available autonomy levels.
#' @return *\[list\]* A list of autonomy level definitions.
#' @export
#' @examples
#' \dontrun{
#' levels <- autonomy.levels()
#' print(levels)
#' }
autonomy.levels <- function() {
  box::use(artma / libs / autonomy[get_autonomy_levels])
  get_autonomy_levels()
}

#' @title Check if Fully Autonomous
#' @description Check if the package is running in fully autonomous mode (level 5).
#' @return *\[logical\]* TRUE if fully autonomous, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' if (autonomy.is_full()) {
#'   message("Running in fully autonomous mode")
#' }
#' }
autonomy.is_full <- function() {
  box::use(artma / libs / autonomy[is_fully_autonomous])
  is_fully_autonomous()
}
