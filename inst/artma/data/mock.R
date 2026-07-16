#' Mock meta-analysis data generation.
#'
#' Powers the interactive "mock" file prompt (see `artma/options/template.R`)
#' and is reused by the test suite through `tests/testthat/modules`. The random
#' helpers below are module-private; only `create_mock_df` is exported.

#' Generate a random numeric vector of a given length within a range.
#'
#' @param from *\[numeric\]* The lower bound of the range.
#' @param to *\[numeric\]* The upper bound of the range.
#' @param length.out *\[numeric\]* The length of the resulting vector.
#' @param integer *\[logical\]* Whether to draw integer values. Defaults to FALSE.
#' @return *\[numeric\]* A numeric vector drawn from the specified range.
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

#' Create mock study names with random occurrences.
#'
#' Generates `n_studies` unique study names and distributes `total_occurrences`
#' rows among them at random, returning the repeated names.
#'
#' @param n_studies *\[integer\]* Number of unique mock study names to create.
#' @param total_occurrences *\[integer\]* Total number of rows to distribute.
#' @return *\[character\]* Study names repeated according to their occurrences.
create_mock_study_names <- function(n_studies, total_occurrences) {
  if (!is.numeric(n_studies) || !is.numeric(total_occurrences) || n_studies <= 0 || total_occurrences <= 0) {
    cli::cli_abort("Both n_studies and total_occurrences should be positive integers.")
  }

  n_studies <- as.integer(n_studies)
  total_occurrences <- as.integer(total_occurrences)

  if (total_occurrences < n_studies) {
    cli::cli_abort("Total occurrences must be greater than or equal to the number of studies.")
  }

  study_names <- paste("Mock Study", 1:n_studies)

  random_occurrences <- function(total, n) {
    points <- sort(sample(1:(total - 1), n - 1))
    diff(c(0, points, total))
  }

  occurrences <- random_occurrences(total_occurrences, n_studies)

  unname(unlist(base::Map(rep, study_names, occurrences)))
}

#' Create and return a mock meta-analysis data frame object
#'
#' @param effect_type A string indicating the type of effect to generate
#' @param nrow An integer indicating the number of rows to generate
#' @param n_studies An integer indicating the number of studies to generate
#' @param with_file_creation A boolean indicating whether to create a file with the data frame
#' @param file_path A string indicating the path of the file to create
#' @param colnames_map A list of column names to use for the data frame
#' @param seed An integer indicating the seed to use for the random number generator
#' @return A data frame object
#' @export
create_mock_df <- function(
  effect_type = NULL,
  nrow = NULL,
  n_studies = NULL,
  with_file_creation = FALSE,
  file_path = NULL,
  colnames_map = NULL,
  seed = NULL
) {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[assert],
    artma / libs / core / utils[get_verbosity],
    artma / data / utils[get_standardized_colnames]
  )

  if (is.null(seed)) {
    seed <- CONST$MOCKS$MOCK_DF_SEED
  }

  colnames_map <- if (is.null(colnames_map)) list() else colnames_map
  assert(is.list(colnames_map), "Column names must be a named list")
  colnames_map <- lapply(colnames_map, make.names)

  base_colnames <- get_standardized_colnames()
  base_colnames_map <- stats::setNames(as.list(base_colnames), base_colnames)
  colnames_map <- utils::modifyList(base_colnames_map, colnames_map)
  assert(all(names(colnames_map) %in% base_colnames), "All column names must be in the base column names")

  # TODO
  # if (is.null(effect_type)) {
  #   effect_type <- "random"
  # }

  if (is.null(nrow)) {
    nrow <- CONST$MOCKS$MOCK_DF_NROWS
  }
  if (is.null(n_studies)) {
    n_studies <- CONST$MOCKS$MOCK_DF_NSTUDIES
  }
  # Run all random generation under a local seed so the caller's RNG stream
  # is left untouched. withr::with_seed restores .Random.seed afterwards.
  random_columns <- withr::with_seed(seed, {
    study_names <- create_mock_study_names(n_studies = n_studies, total_occurrences = nrow)
    list(
      study_names = study_names,
      effect = generate_random_vector(from = -1, to = 1, length.out = nrow),
      se = generate_random_vector(from = 0.01, to = 1, length.out = nrow),
      n_obs = generate_random_vector(from = 10, to = 1000, length.out = nrow, integer = TRUE)
    )
  })

  study_names <- random_columns$study_names
  study_id <- as.integer(factor(study_names, levels = unique(study_names)))
  obs_id <- base::seq.int(from = 1, to = nrow, by = 1)

  effect <- random_columns$effect
  se <- random_columns$se
  n_obs <- random_columns$n_obs

  base_df <- list(
    obs_id = obs_id,
    study_id = study_id,
    effect = effect,
    se = se,
    n_obs = n_obs
  )
  df_names <- unname(unlist(vapply(names(base_df), function(x) colnames_map[x], FUN.VALUE = list(1))))
  data_frame <- data.frame(stats::setNames(base_df, df_names))

  if (with_file_creation) {
    if (is.null(file_path)) {
      file_path <- CONST$MOCKS$TMP_DATA_FILE_PATH
    }

    assert(
      is.character(file_path) && length(file_path) == 1,
      "File path must be a single character string"
    )
    assert(
      dir.exists(dirname(file_path)),
      "File path must be a valid path"
    )

    if (get_verbosity() >= 4) {
      cli::cli_inform("Creating mock data file: {file_path}")
    }
    utils::write.csv(data_frame, file_path, row.names = FALSE)
  }

  data_frame
}

box::export(create_mock_df)
