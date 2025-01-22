#' Determine whether a function call is being made in a debug more or not
#'
#' @return [boolean] TRUE if the function call is being made in a debug mode,
#'  FALSE otherwise.
#' @export
is_debugging <- function() {
  any(sapply(sys.calls(), function(x) format(x)[[1]] %in% c("browser", "debug")))
}
