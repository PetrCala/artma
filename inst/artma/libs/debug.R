#' Determine whether a function call is being made in a debug more or not
#'
#' `boolean` TRUE if the function call is being made in a debug mode,
#'  FALSE otherwise.
#' @export
is_debugging <- function() {
  any(vapply(sys.calls(), FUN = function(x) format(x)[[1]] %in% c("browser", "debug"), FUN.VALUE = list(1)))
}
