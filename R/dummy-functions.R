# This script serves as a placeholder for the functions that are used inside the internal 'inst/artma' folder logic, so that the R compiler can treat them as 'used' and not throw warnings about unused functions.

# nolint start: object_usage_linter, box_usage_linter, unused_declared_object_linter.

#' @keywords internal
.dummy_use_tidyverse <- function() {
  dplyr::tibble(a = 1)
  stringr::str_trim(" test ")
  purrr::map(1:3, ~ .x + 1)
  tidyr::unnest(tibble::tibble(a = list(1:2)))
  invisible(NULL)
}


# nolint end: object_usage_linter, box_usage_linter, unused_declared_object_linter.
