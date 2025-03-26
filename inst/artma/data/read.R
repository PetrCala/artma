#' @export
read_data <- function(path = NULL) {
  path <- if (!is.null(path)) path else getOption("artma.data.source_path")


  return(data.frame())
}
