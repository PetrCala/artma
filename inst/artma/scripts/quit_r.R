#' Quit R from the interactive browser
quit_debugger_and_console <- function() {
  if (interactive()) {
    cat("Q\n") # Quit debugger
    cat("q()\n") # Quit R console
  }
}
