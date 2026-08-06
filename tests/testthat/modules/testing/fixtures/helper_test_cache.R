# Silence cli output so it doesn't clutter testthat reports. `cli.autoprint`
# alone only affects auto-printing of returned cli objects at the console top
# level - it does nothing for cli_alert()/cli_inform() calls made inside
# function bodies, which is what actually clutters test output. Drop
# artma.verbose to "errors only" too; this does not hide genuine warning()/
# stop() conditions, which aren't gated by artma.verbose.
local_cli_silence <- function(env = parent.frame()) {
  old <- options(cli.autoprint = FALSE)
  withr::defer(options(old), envir = env)
  withr::local_options(list("artma.verbose" = 1), .local_envir = env)
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
