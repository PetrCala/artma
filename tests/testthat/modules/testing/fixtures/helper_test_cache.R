# Silence cli output so it doesn't clutter testthat reports
local_cli_silence <- function(env = parent.frame()) {
  old <- options(cli.autoprint = FALSE)
  withr::defer(options(old), envir = env)
}

# Synthetic "expensive" function used in cache tests --------------------------
# Returns a fresh modeller together with a call counter so tests can assert on
# how many times the underlying implementation actually ran, instead of
# relying on wall-clock sleeps to simulate cost.
make_fake_modeller <- function() {
  calls <- 0L
  self_env <- environment()
  modeller <- function(x) {
    self_env$calls <- self_env$calls + 1L
    cli::cli_alert("Running model on {.val {x}}")
    x * 2
  }
  list(
    modeller = modeller,
    calls = function() self_env$calls
  )
}

box::export(local_cli_silence, make_fake_modeller)
