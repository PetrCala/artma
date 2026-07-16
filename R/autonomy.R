#' @title Get Autonomy Level
#' @description Get the current autonomy level.
#'   Autonomy controls how much user interaction is required during analysis.
#'   `interactive()` is the hard gate: non-interactive sessions never prompt,
#'   regardless of this setting.
#' @return *\[character or NULL\]* The current autonomy level ("ask_more",
#'   "balanced", or "autonomous"), or NULL if not set.
#' @export
#' @examples
#' \dontrun{
#' # Get current autonomy level
#' level <- autonomy.get()
#' print(level)
#' }
autonomy.get <- function() {
  box::use(artma / libs / core / autonomy[get_autonomy_level])
  get_autonomy_level()
}

#' @title Set Autonomy Level
#' @description Set the autonomy level for the current session.
#'   This setting controls how much user interaction is required during analysis.
#' @param level *\[character\]* The autonomy level to set.
#'   - "ask_more": Prompt for most decisions, including non-critical ones.
#'   - "balanced": Prompt for important decisions only.
#'   - "autonomous" (default): Minimal prompts; use defaults and auto-detection for most decisions.
#'
#'   Legacy numeric levels (1-5) are still accepted and translated, with a
#'   warning (1-2 -> "ask_more", 3 -> "balanced", 4-5 -> "autonomous").
#' @return `NULL` (invisible)
#' @export
#' @examples
#' \dontrun{
#' # Set to fully autonomous mode
#' autonomy.set("autonomous")
#'
#' # Set to balanced mode
#' autonomy.set("balanced")
#' }
autonomy.set <- function(level) {
  box::use(artma / libs / core / autonomy[set_autonomy_level])
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
autonomy.is_set <- function() { # nolint: object_name_linter.
  box::use(artma / libs / core / autonomy[is_autonomy_level_set])
  is_autonomy_level_set()
}

#' @title Check if Fully Autonomous
#' @description Check if the package is running in fully autonomous mode, i.e.
#'   the autonomy level is set to "autonomous", or the session is
#'   non-interactive (where prompts never happen regardless of the configured
#'   level).
#' @return *\[logical\]* TRUE if fully autonomous, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' if (autonomy.is_full()) {
#'   message("Running in fully autonomous mode")
#' }
#' }
autonomy.is_full <- function() { # nolint: object_name_linter.
  box::use(artma / libs / core / autonomy[get_autonomy_level])
  !interactive() || identical(get_autonomy_level(), "autonomous")
}
