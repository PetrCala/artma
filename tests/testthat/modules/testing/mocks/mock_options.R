#' @title Create mock colnames
#' @description Create a list of mock colnames. All colnames are made syntactically valid.
#' @param colnames *\[list, optional\]* A list of colnames to mock.
#' @return *\[list\]* A list of mock colnames.
create_mock_options_colnames <- function(
    colnames = NULL) {
  box::use(artma / data / utils[get_standardized_colnames])

  colnames <- if (is.null(colnames)) list() else colnames
  colnames <- lapply(colnames, make.names)

  all_colnames <- get_standardized_colnames()
  base_colnames <- stats::setNames(as.list(all_colnames), all_colnames)

  utils::modifyList(base_colnames, colnames)
}

box::export(create_mock_options_colnames)
