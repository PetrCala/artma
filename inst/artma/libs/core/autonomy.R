#' @title Autonomy Level Management
#' @description Functions for managing the autonomy level of the package.
#'   Autonomy controls how much user interaction is required during analysis.
#'   `interactive()` is the hard gate: non-interactive sessions never prompt,
#'   regardless of the configured level. Within interactive sessions, the level
#'   controls how eagerly the package prompts for decisions.

#' @title Normalize an autonomy level value
#' @description Coerce a raw autonomy level value to one of the canonical enum
#'   values. Legacy numeric levels (1-5) are translated (1-2 -> "ask_more",
#'   3 -> "balanced", 4-5 -> "autonomous"). Anything else aborts with a clear
#'   message.
#' @param level [any] The raw level value to normalize.
#' @param warn_on_legacy *\[logical\]* Whether to warn when translating a legacy
#'   numeric value. Defaults to FALSE.
#' @return *\[character\]* One of "ask_more", "balanced", "autonomous".
#' @keywords internal
normalize_autonomy_level <- function(level, warn_on_legacy = FALSE) {
  box::use(artma / const[CONST])

  if (is.character(level) && length(level) == 1 && level %in% CONST$AUTONOMY$LEVELS) {
    return(level)
  }

  if (is.numeric(level) && length(level) == 1 && !is.na(level) &&
    level == as.integer(level) && level >= 1 && level <= 5) {
    translated <- if (level <= 2) "ask_more" else if (level == 3) "balanced" else "autonomous"

    if (isTRUE(warn_on_legacy)) {
      cli::cli_warn(c(
        "!" = "Numeric autonomy level {.val {level}} is deprecated.",
        "i" = "Using {.val {translated}} instead. Set one of {.val {CONST$AUTONOMY$LEVELS}} directly."
      ))
    }

    return(translated)
  }

  cli::cli_abort(
    message = "Invalid autonomy level: {.val {level}}. Must be one of {.val {CONST$AUTONOMY$LEVELS}} (or a legacy numeric value from 1 to 5).",
    .subclass = "validation_error"
  )
}

#' @title Get autonomy level
#' @description Get the current autonomy level from options.
#' @return *\[character or NULL\]* The current autonomy level ("ask_more",
#'   "balanced", or "autonomous"), or NULL if not set.
#' @export
get_autonomy_level <- function() {
  box::use(artma / const[CONST])

  opt_name <- paste0(CONST$PACKAGE_NAME, ".autonomy.level")
  getOption(opt_name, default = NULL)
}

#' @title Set autonomy level
#' @description Set the autonomy level in the R options namespace.
#' @param level *\[character\]* The autonomy level to set: one of "ask_more",
#'   "balanced", "autonomous". Legacy numeric levels (1-5) are still accepted
#'   and translated, with a warning.
#' @return `NULL` (invisible)
#' @export
set_autonomy_level <- function(level) {
  box::use(artma / const[CONST])

  normalized <- normalize_autonomy_level(level, warn_on_legacy = TRUE)

  opt_name <- paste0(CONST$PACKAGE_NAME, ".autonomy.level")
  options(stats::setNames(list(normalized), opt_name))

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
#'   `interactive()` is the hard gate: in non-interactive sessions this always
#'   returns FALSE.
#' @param required_level *\[character\]* The autonomy level at or above which the
#'   user is not prompted. The user is prompted only when the current level is
#'   strictly less autonomous than `required_level`, ordered
#'   "ask_more" < "balanced" < "autonomous". Defaults to "autonomous".
#' @return *\[logical\]* TRUE if the user should be prompted, FALSE otherwise.
#' @export
should_prompt_user <- function(required_level = "autonomous") {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[validate]
  )

  validate(
    is.character(required_level),
    length(required_level) == 1,
    required_level %in% CONST$AUTONOMY$LEVELS
  )

  # interactive() is the hard gate: never prompt outside interactive sessions
  if (!interactive()) {
    return(FALSE)
  }

  current_level <- get_autonomy_level()

  # If no level is set, we need to prompt (interactive mode only)
  if (is.null(current_level)) {
    return(TRUE)
  }

  current_level <- normalize_autonomy_level(current_level)

  match(current_level, CONST$AUTONOMY$LEVELS) < match(required_level, CONST$AUTONOMY$LEVELS)
}

#' @title Get default autonomy level
#' @description Get the default autonomy level, used for new options files.
#' @return *\[character\]* The default autonomy level.
#' @export
get_default_autonomy_level <- function() {
  box::use(artma / const[CONST])
  CONST$AUTONOMY$DEFAULT
}

box::export(
  get_autonomy_level,
  get_default_autonomy_level,
  is_autonomy_level_set,
  set_autonomy_level,
  should_prompt_user
)
