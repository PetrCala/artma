#' Create Mock Study Names with Random Occurrences
#'
#' This function generates a specified number of unique mock study names and assigns
#' each study a random number of occurrences such that the sum of all occurrences equals
#' the specified total. If the total occurrences are less than the number of studies,
#' an error is thrown. Both inputs should be integers.
#'
#' @param n_studies Integer. Number of unique mock study names to create.
#' @param total_occurrences Integer. Total number of occurrences to distribute among the studies.
#'
#' @return A character vector with the study names repeated according to their assigned occurrences.
#'
#' @examples
#' create_mock_study_names(5, 20)
create_mock_study_names <- function(n_studies, total_occurrences) {
  if (!is.numeric(n_studies) || !is.numeric(total_occurrences) || n_studies <= 0 || total_occurrences <= 0) {
    rlang::abort("Both n_studies and total_occurrences should be positive integers.")
  }

  n_studies <- as.integer(n_studies)
  total_occurrences <- as.integer(total_occurrences)

  if (total_occurrences < n_studies) {
    rlang::abort("Total occurrences must be greater than or equal to the number of studies.")
  }

  # Create study names
  study_names <- paste("Mock Study", 1:n_studies)

  # Generate random occurrences ensuring the sum is total_occurrences
  random_occurrences <- function(total, n) {
    # Generate n-1 random points and sort them
    points <- sort(sample(1:(total - 1), n - 1))
    # Calculate differences between points to get the random partitions
    occurrences <- diff(c(0, points, total))
    return(occurrences)
  }

  occurrences <- random_occurrences(total_occurrences, n_studies)

  # Create the output vector
  result <- unlist(mapply(rep, study_names, occurrences))

  return(result)
}
