# Each function in this module should take a single argument, `opt`, which is the option to prompt the user for.
# These functions are imported into the `template.R` file, which is used to prompt the user for options.

prompt_data_config <- function(opt, ...) {
  box::use(artma / libs / core / utils[get_verbosity])
  if (get_verbosity() >= 2) {
    cli::cli_alert_warning("This option is not yet implemented.")
  }
}

prompt_winsorization_level <- function(opt, ...) {
  box::use(artma / const[CONST])

  choices <- c(
    "None (0%)" = 0,
    "1% (default)" = 0.01,
    "5%" = 0.05,
    "10%" = 0.10
  )

  cli::cli_h1("Winsorization Level")
  cli::cli_text("Winsorization caps extreme values at specified quantiles to reduce outlier influence.")
  cli::cat_line()

  selected <- climenu::select(
    choices = names(choices),
    prompt = "Select winsorization level for effect and standard error variables",
    selected = 2 # "1% (default)"
  )

  if (rlang::is_empty(selected)) {
    cli::cli_alert_info("No selection made. Using default: {CONST$STYLES$OPTIONS$VALUE('0.01')}")
    return(0.01)
  }

  selected_value <- choices[selected][[1]]
  cli::cli_alert_success("Selected winsorization level: {CONST$STYLES$OPTIONS$VALUE(selected_value)}")
  cli::cat_line()

  selected_value
}

prompt_na_handling <- function(opt, ...) {
  box::use(artma / const[CONST])

  choices <- c(
    "Stop (abort if missing values found)" = "stop",
    "Remove rows (listwise deletion)" = "remove",
    "Median imputation (replace with column median)" = "median",
    "Mean imputation (replace with column mean)" = "mean",
    "Linear interpolation (for sequential data)" = "interpolate",
    "Multiple imputation (mice algorithm)" = "mice"
  )

  cli::cli_h1("Missing Value Handling")
  cli::cli_text("Choose how to handle missing values in non-required columns during preprocessing.")
  cli::cli_alert_info("Note: Required columns (effect, se, study_id, n_obs) must always be complete.")
  cli::cat_line()

  cli::cli_h3("Available strategies:")
  cli::cli_ul(c(
    "{.strong stop}: Abort analysis if any missing values are found (safest, ensures data quality)",
    "{.strong remove}: Remove entire rows with any missing values (listwise deletion)",
    "{.strong median}: Replace missing values with the variable's median (robust to outliers)",
    "{.strong mean}: Replace missing values with the variable's mean (assumes normality)",
    "{.strong interpolate}: Use linear interpolation based on neighboring values (best for sequential data)",
    "{.strong mice}: Multiple Imputation by Chained Equations (advanced, most accurate but slower)"
  ))
  cli::cat_line()

  selected <- climenu::select(
    choices = names(choices),
    prompt = "Select missing value handling strategy",
    selected = 1 # "stop" as default
  )

  if (rlang::is_empty(selected)) {
    cli::cli_alert_info("No selection made. Using default: {CONST$STYLES$OPTIONS$VALUE('stop')}")
    return("stop")
  }

  selected_value <- choices[selected][[1]]

  # Provide additional context for certain choices
  if (selected_value == "mice") {
    cli::cli_alert_warning("MICE imputation requires the {.pkg mice} package and may take significant time for large datasets.")
  } else if (selected_value == "remove") {
    cli::cli_alert_warning("Listwise deletion may significantly reduce sample size if many rows have missing values.")
  }

  cli::cli_alert_success("Selected strategy: {CONST$STYLES$OPTIONS$VALUE(selected_value)}")
  cli::cat_line()

  selected_value
}

prompt_autonomy_level <- function(opt, ...) {
  box::use(
    artma / const[CONST],
    artma / libs / core / autonomy[get_autonomy_levels, get_default_autonomy_level]
  )

  levels <- get_autonomy_levels()
  default_level <- get_default_autonomy_level()

  cli::cli_h1("Autonomy Level")
  cli::cli_text("Autonomy controls how much user interaction is required during analysis.")
  cli::cli_text("Higher levels mean less user interaction and more automatic decision-making.")
  cli::cat_line()

  # Create a named vector where names are display text and values are the level numbers
  choices <- stats::setNames(
    as.integer(names(levels)), # Values: 1, 2, 3, 4, 5
    vapply(
      names(levels),
      function(lvl) {
        level_def <- levels[[lvl]]
        sprintf(
          "Level %s - %s: %s",
          lvl,
          level_def$name,
          level_def$description
        )
      },
      character(1)
    )
  )

  selected <- climenu::select(
    choices = names(choices), # Display the descriptive text
    prompt = "Select your preferred autonomy level",
    selected = default_level # Default is index 4
  )

  if (rlang::is_empty(selected)) {
    cli::cli_alert_info("No selection made. Using default: {CONST$STYLES$OPTIONS$VALUE(default_level)}")
    return(default_level)
  }

  # Get the actual level value from the selected index
  selected_value <- choices[selected][[1]]
  cli::cli_alert_success("Selected autonomy level: {CONST$STYLES$OPTIONS$VALUE(selected_value)}")
  cli::cat_line()

  selected_value
}

box::export(
  prompt_autonomy_level,
  prompt_data_config,
  prompt_winsorization_level,
  prompt_na_handling
)
