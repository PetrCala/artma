# source("R/utils/options.R")

#' @export
artma <- function() {
  box::use(
    modules / options[load_options]
  )



  load_options()
  # Usage
  print(getOption("artma"))
}
