#' @title Get verbosity
#' @description Get the verbosity level
#' @return [integer] The verbosity level
#' @export
get_verbosity <- function() {
  box::use(artma / const[CONST])
  getOption("artma.verbose", CONST$DEFAULT_VERBOSITY)
}
