#' Validate that a filename follows the data config file naming convention
#' @description Check if a filename ends with the data config file suffix
#' @param filename *\[character\]* The filename to validate
#' @return *\[logical\]* `TRUE` if the filename is valid, `FALSE` otherwise
validate_data_config_filename <- function(filename) {
  box::use(artma / const[CONST])
  grepl(CONST$REGEX$DATA_CONFIG_FILE_SUFFIX, filename)
}

box::export(
  validate_data_config_filename
)
