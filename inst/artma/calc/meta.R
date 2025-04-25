#' @title Calculate t-statistic
#' @description Calculate the t-statistic from effect and SE
#' @param effect [numeric] The effect
#' @param se [numeric] The standard error
#' @return [numeric] The t-statistic
t_stat <- function(effect, se) effect / se


box::export(t_stat)
