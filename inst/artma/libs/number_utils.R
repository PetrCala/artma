#' Generate Random Vector
#'
#' This function generates a random vector of specified length with numbers drawn from a specified range.
#'
#' @param from Integer. The starting number of the range.
#' @param to Integer. The ending number of the range.
#' @param length.out Integer. The length of the resulting vector.
#' @param integer Logical. Whether to generate integer values. Defaults to FALSE.
#'
#' @return *\[vector, numeric\]* A numeric vector of the specified length with numbers drawn from the specified range.
#' @export
#'
#' @examples
#' # Generate a vector of length 10 with numbers from 1 to 100
#' generate_random_vector(1, 100, 10)
#'
#' # Generate a vector of length 5 with integer numbers from 1 to 50
#' generate_random_vector(1, 50, 5, integer = TRUE)
generate_random_vector <- function(from, to, length.out, integer = FALSE) {
  if (!is.numeric(from) || !is.numeric(to) || !is.numeric(length.out)) {
    cli::cli_abort("Invalid input: 'from', 'to', and 'length.out' should be numeric.")
  }

  if (from > to) {
    cli::cli_abort("Invalid range: 'from' should be less than or equal to 'to'.")
  }

  if (integer) {
    return(sample(from:to, size = length.out, replace = TRUE))
  }

  stats::runif(n = length.out, min = from, max = to)
}

box::export(generate_random_vector)
