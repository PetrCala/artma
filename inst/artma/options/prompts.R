# Each function in this module should take a single argument, `opt`, which is the option to prompt the user for.
# These functions are imported into the `template.R` file, which is used to prompt the user for options.

prompt_data_config <- function(opt, ...) {
  box::use(artma / libs / utils[get_verbosity])
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

box::export(
  prompt_data_config,
  prompt_winsorization_level
)
