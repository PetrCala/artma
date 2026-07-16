box::use(
  artma / libs / core / validation[validate]
)

#' Resolve the add_significance_marks option.
#'
#' Reads the single canonical template key `artma.methods.add_significance_marks`.
#' The template default drives the value when the option is unset, so there is no
#' per-method legacy fallback and "unset" resolves to the documented default
#' rather than an implicit TRUE.
#'
#' @return *[logical]* Whether significance marks should be appended.
resolve_add_significance_marks <- function() {
  resolved <- getOption("artma.methods.add_significance_marks", TRUE)
  validate(is.logical(resolved), length(resolved) == 1)
  as.logical(resolved)
}

box::export(resolve_add_significance_marks)
