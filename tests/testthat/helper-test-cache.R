# Silence cli output so it doesn't clutter testthat reports
local_cli_silence <- function(env = parent.frame()) {
  old <- options(cli.autoprint = FALSE)
  withr::defer(options(old), envir = env)
}

# Synthetic "expensive" function used in several tests -----------------------
fake_modeller <- function(x) {
  cli::cli_alert("Running model on {.val {x}}")
  Sys.sleep(0.01) # mimic cost
  x * 2
}
