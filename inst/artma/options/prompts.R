# Each function in this module should take a single argument, `opt`, which is the option to prompt the user for.
# These functions are imported into the `template.R` file, which is used to prompt the user for options.

prompt_data_config <- function(opt, ...) {
  cli::cli_inform("This option is not yet implemented.")
}

box::export(
  prompt_data_config
)
