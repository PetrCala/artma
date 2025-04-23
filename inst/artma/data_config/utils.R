#' @title Data Config Is Valid
#' @description Check if the data config is valid.
#' @param config *\[list, optional\]* The data config to check. If `NULL` (default), the data config will be retrieved from the options.
#' @return *\[logical\]* Whether the data config is valid.
data_config_is_valid <- function(
    config = NULL) {
  config <- if (is.null(config)) getOption("artma.data.config") else config
  # There is potentially room for more checks here
  if (is.list(config)) {
    return(TRUE)
  }
  return(FALSE)
}

box::export(data_config_is_valid)
