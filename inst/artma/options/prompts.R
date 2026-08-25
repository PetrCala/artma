# Custom prompt functions dispatched from `template.R` (via `prompt: "function"`
# template nodes) or called directly by the configure phase. Each takes the
# option definition as `opt` (unused by most) and forwards `...` to
# `ask_select`, which keeps the menu backend injectable in tests.

prompt_winsorization_level <- function(opt = NULL, ...) {
  box::use(artma / interactive / input[ask_select])

  answer <- ask_select(
    question = "Winsorization level for effect and standard error variables",
    choices = c(
      "None (0%)" = "0",
      "1% (default)" = "0.01",
      "5%" = "0.05",
      "10%" = "0.10"
    ),
    hints = "caps extreme values at the chosen quantile from both tails to reduce outlier influence",
    default = "0.01",
    ...
  )

  as.numeric(answer)
}

prompt_na_handling <- function(opt = NULL, ...) {
  box::use(artma / interactive / input[ask_select])

  answer <- ask_select(
    question = "How should missing values in optional columns be handled?",
    choices = c(
      "Stop: leave them as-is and report them (safest)" = "stop",
      "Remove: drop entire rows with any missing values" = "remove",
      "Median: impute with the column median" = "median",
      "Mean: impute with the column mean" = "mean",
      "Interpolate: linear interpolation from neighboring values" = "interpolate",
      "Mice: multiple imputation by chained equations" = "mice"
    ),
    hints = "required columns (effect, se, study_id, n_obs) must always be complete",
    default = "stop",
    ...
  )

  if (answer == "mice") {
    cli::cli_alert_warning("MICE imputation requires the {.pkg mice} package and may take significant time for large datasets.")
  } else if (answer == "remove") {
    cli::cli_alert_warning("Listwise deletion may significantly reduce sample size if many rows have missing values.")
  }

  answer
}

prompt_se_zero_handling <- function(opt = NULL, ...) {
  box::use(artma / interactive / input[ask_select])

  answer <- ask_select(
    question = "How should rows with a zero standard error be handled?",
    choices = c(
      "Remove them, with a warning (default)" = "remove",
      "Stop: abort the analysis" = "stop",
      "Warn but keep them" = "warn",
      "Ignore silently" = "ignore"
    ),
    hints = "zero standard errors cause division-by-zero errors, e.g. infinite t-statistics",
    default = "remove",
    ...
  )

  if (answer == "stop") {
    cli::cli_alert_warning("Strict mode: analysis will abort if any zero standard errors are found.")
  }

  answer
}

prompt_autonomy_level <- function(opt = NULL, ...) {
  box::use(
    artma / const[CONST],
    artma / interactive / input[ask_select],
    artma / libs / core / autonomy[get_default_autonomy_level]
  )

  descriptions <- c(
    ask_more = "prompt for most decisions, including non-critical ones",
    balanced = "prompt for important decisions only",
    autonomous = "minimal prompts; use defaults and auto-detection for most decisions"
  )

  levels <- CONST$AUTONOMY$LEVELS
  choices <- stats::setNames(
    levels,
    vapply(
      levels,
      function(lvl) sprintf("%s: %s", lvl, descriptions[[lvl]]),
      character(1)
    )
  )

  ask_select(
    question = "Preferred autonomy level",
    choices = choices,
    hints = c(
      "controls how much user interaction is required during analysis",
      "{.code interactive()} is the hard gate: non-interactive sessions never prompt"
    ),
    default = get_default_autonomy_level(),
    ...
  )
}

box::export(
  prompt_autonomy_level,
  prompt_winsorization_level,
  prompt_na_handling,
  prompt_se_zero_handling
)
