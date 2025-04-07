# Function to find all test files recursively
find_test_files <- function(path = file.path("tests", "testthat")) {
  test_files <- list.files(path, pattern = "^test.*\\.[rR]$", recursive = TRUE, full.names = TRUE)
  return(test_files)
}

#' Function to run test_dir on a directory and all its subdirectories
#' @export
run_tests_recursively <- function(path = file.path("tests", "testthat"), reporter = NULL) {
  test_files <- find_test_files(path)
  if (length(test_files) == 0) {
    cli::cli_abort("No test files found")
  }
  lapply(test_files, testthat::test_file, reporter = reporter)
}
