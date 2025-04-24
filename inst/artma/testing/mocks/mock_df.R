box::use(
  artma / libs / number_utils[generate_random_vector],
  artma / testing / mocks / mock_utils[create_mock_study_names]
)

#' Create and return a mock meta-analysis data frame object
#'
#' @param effect_type A string indicating the type of effect to generate
#' @param nrow An integer indicating the number of rows to generate
#' @param n_studies An integer indicating the number of studies to generate
#' @param with_file_creation A boolean indicating whether to create a file with the data frame
#' @param file_path A string indicating the path of the file to create
#' @return A data frame object
#' @export
create_mock_df <- function(
    effect_type = NULL,
    nrow = NULL,
    n_studies = NULL,
    with_file_creation = FALSE,
    file_path = NULL) {
  box::use(
    artma / const[CONST],
    artma / libs / validation[assert]
  )

  if (is.null(nrow)) {
    nrow <- CONST$MOCKS$MOCK_DF_NROWS
  }
  if (is.null(n_studies)) {
    n_studies <- CONST$MOCKS$MOCK_DF_NSTUDIES
  }
  study_names <- create_mock_study_names(n_studies = n_studies, total_occurrences = nrow)

  # TODO
  # if (is.null(effect_type)) {
  #   effect_type <- "random"
  # }

  effect <- generate_random_vector(from = -1, to = 1, length.out = nrow)
  se <- generate_random_vector(from = -1, to = 1, length.out = nrow)
  n_obs <- generate_random_vector(from = 10, to = 1000, length.out = nrow, integer = TRUE)

  data_frame <- data.frame(
    effect = effect,
    se = se,
    study = study_names,
    n_obs = n_obs
  )

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

    cli::cli_inform("Creating mock data file: {file_path}")
    utils::write.csv(data_frame, file_path, row.names = FALSE)
  }

  return(data_frame)
}


box::export(
  create_mock_df
)
