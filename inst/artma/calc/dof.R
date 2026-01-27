box::use(
  artma / libs / core / validation[assert]
)
#' Calculate degrees of freedom using a t-value and a PCC
#'
#' @note The t_value and PCC can be provided either as a single numeric value, or as vectors of the same length.
#'
#' @param t_value [numeric] The t-value(s) to use for the calculation.
#' @param pcc [numeric] The partial correlation coefficient(s) to use for the calculation.
#' `numeric` The calculated degrees of freedom.
#' @export
calculate_dof <- function(t_value, pcc) {
  assert(length(t_value) == length(pcc), "The length of 't_value' and 'pcc' must be the same.")

  lhs <- t_value^2
  rhs <- (1 / (pcc^2)) - 1
  lhs * rhs
}
