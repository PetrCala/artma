#' Generate Random Vector with Replacement
#'
#' This function generates a random vector of specified length with numbers drawn from a specified range.
#' The numbers can be drawn with or without replacement.
#'
#' @param from Integer. The starting number of the range.
#' @param to Integer. The ending number of the range.
#' @param length.out Integer. The length of the resulting vector.
#' @param replace Logical. Should sampling be with replacement? Default is TRUE.
#'
#' @return A numeric vector of the specified length with numbers drawn from the specified range.
#' @export
#'
#' @examples
#' # Generate a vector of length 10 with numbers from 1 to 100, with replacement
#' generate_random_vector(1, 100, 10)
#'
#' # Generate a vector of length 5 with numbers from 1 to 50, without replacement
#' generate_random_vector(1, 50, 5, replace = FALSE)
generate_random_vector <- function(from, to, length.out, replace = TRUE) {
  if (!is.numeric(from) || !is.numeric(to) || !is.numeric(length.out) || !is.logical(replace)) {
    rlang::abort("Invalid input: 'from', 'to', and 'length.out' should be numeric, 'replace' should be logical.")
  }

  if (from > to) {
    rlang::abort("Invalid range: 'from' should be less than or equal to 'to'.")
  }

  if (!replace && length.out > (to - from + 1L)) {
    rlang::abort("Invalid input: 'length.out' cannot be greater than the range size when sampling without replacement.")
  }

  sample(from:to, size = length.out, replace = replace)
}
