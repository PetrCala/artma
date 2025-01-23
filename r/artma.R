#' @export
artma <- function(
    options_file_path = NULL,
    args = commandArgs(trailingOnly = TRUE)) {
  setup.ensure_valid_boxpath()
  box::use(artma / setup[artma_setup])
  artma_setup(
    options_file_path = options_file_path,
    args = args
  )
}
