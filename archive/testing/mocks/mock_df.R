#' Create and return a mock meta-analysis data frame object
#'
#' @param effect_type A string indicating the type of effect to generate
#' @param nrow An integer indicating the number of rows to generate
#' @return A data frame object
#' @export
create_mock_df <- function(
    effect_type = NULL,
    nrow = NULL,
    n_studies = NULL) {
  box::use(
    artma / libs / number_utils[generate_random_vector],
    artma / testing / test_const[TEST_CONST],
    artma / testing / mocks / mock_utils[create_mock_study_names],
  )

  if (is.null(nrow)) {
    nrow <- TEST_CONST$MOCK_DF_NROWS
  }
  if (is.null(n_studies)) {
    n_studies <- TEST_CONST$MOCK_DF_NSTUDIES
  }
  study_names <- create_mock_study_names(n_studies = n_studies, total_occurrences = nrow)

  # TODO
  # if (is.null(effect_type)) {
  #   effect_type <- "random"
  # }

  effect <- generate_random_vector(from = -1, to = 1, length.out = nrow, replace = TRUE)
  se <- generate_random_vector(from = -1, to = 1, length.out = nrow, replace = TRUE)

  data_frame <- data.frame(
    effect = effect,
    se = se,
    study = study_names
  )
  return(data_frame)
}
