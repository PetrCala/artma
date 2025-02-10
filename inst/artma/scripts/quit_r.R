#' Quit R from the interactive browser
#' @export
quit_debugger_and_console <- function() {
  if (interactive()) {
    cat("Q\n") # Quit debugger
    cat("q()\n") # Quit R console
  }
}
