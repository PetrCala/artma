# nolint start: object_name_linter.

box::use(
  graphics[plot.new, plot, lines, points, segments, abline, legend, layout]
)

#' Compute the weighted mean for the first `i` studies
#'
#' @param beta [numeric] Effect sizes ordered by precision.
#' @param se [numeric] Standard errors ordered by precision.
#' @param sigma [numeric] Heterogeneity parameter.
#' @return Numeric vector of cumulative weighted means.
weighted_mean <- function(beta, se, sigma) {
  n <- length(beta)
  weights <- 1 / (se^2 + sigma^2)
  cumulative_weight <- cumsum(weights)
  cumsum(beta * weights) / cumulative_weight
}

#' Compute weighted second moments
#'
#' @param beta [numeric] Effect sizes ordered by precision.
#' @param se [numeric] Standard errors ordered by precision.
#' @param sigma [numeric] Heterogeneity parameter.
#' @return Numeric vector of weighted second moments.
weighted_mean_squared <- function(beta, se, sigma) {
  n <- length(beta)
  weights <- 1 / (se^2 + sigma^2)
  weight_matrix <- outer(weights, weights)
  weighted_beta <- beta * weights
  beta_matrix <- outer(weighted_beta, weighted_beta)
  cumulative_matrix <- compute_submatrix_sums(weight_matrix)
  cumulative_beta <- compute_submatrix_sums(beta_matrix)
  numerator <- cumulative_beta - cumsum(weighted_beta^2)
  denominator <- cumulative_matrix - cumsum(weights^2)
  out <- numerator / denominator
  out[1] <- 0
  out
}

#' Compute cumulative sums for each leading principal submatrix
#'
#' @param matrix_input [matrix] Square matrix.
#' @return Numeric vector with cumulative sums.
compute_submatrix_sums <- function(matrix_input) {
  cumulative_cols <- apply(matrix_input, 2, cumsum)
  cumulative_rows <- t(apply(cumulative_cols, 1, cumsum))
  diag(cumulative_rows)
}

#' Variance component for a given number of studies
#'
#' @param n_stem [integer] Number of studies included.
#' @param beta [numeric] Effect sizes.
#' @param se [numeric] Standard errors.
#' @param beta_mean [numeric] Pooled mean.
#' @return Non-negative variance component.
variance_0 <- function(n_stem, beta, se, beta_mean) {
  weights <- 1 / se[1:n_stem]^2
  total_weight <- sum(weights)
  centered <- beta[1:n_stem] - beta_mean
  numerator <- sum(weights * centered^2) - (n_stem - 1)
  denominator <- total_weight - sum(weights^2) / total_weight
  pmax(0, numerator / denominator)
}

#' Running variance of the estimator
#'
#' @param se [numeric] Standard errors ordered by precision.
#' @param sigma [numeric] Heterogeneity parameter.
#' @return Vector of variances.
variance_b <- function(se, sigma) {
  weights <- 1 / (se^2 + sigma^2)
  1 / cumsum(weights)
}

#' Core STEM computation for a given heterogeneity guess
#'
#' @param beta [numeric] Effect sizes ordered by precision.
#' @param se [numeric] Standard errors ordered by precision.
#' @param sigma0 [numeric] Initial heterogeneity guess.
#' @return List with estimates and MSE components.
stem_compute <- function(beta, se, sigma0) {
  n <- length(beta)
  eb_all <- weighted_mean(beta, se, sigma0)
  eb_leave_top_out <- weighted_mean(beta[-1], se[-1], sigma0)
  eb_squared <- weighted_mean_squared(beta[-1], se[-1], sigma0)
  mse_original <- eb_squared - 2 * beta[1] * eb_leave_top_out
  var_all <- variance_b(se, sigma0)
  n_stem_min <- 3
  mse <- mse_original[(n_stem_min - 1):(n - 1)]
  bias <- mse - var_all[n_stem_min:n]
  index <- which.min(mse)
  n_stem <- index + (n_stem_min - 1)
  beta_stem <- eb_all[n_stem]
  se_stem <- sqrt(var_all[n_stem])
  var_stem <- variance_0(n, beta, se, beta_stem)
  sigma_stem <- sqrt(var_stem)
  estimates <- cbind(beta_stem, se_stem, sigma_stem, n_stem)
  colnames(estimates) <- c("beta_stem", "se_stem", "sigma_stem", "n_stem")
  mse_table <- rbind(mse, var_all[n_stem_min:n], bias[(n_stem_min - 1):(n - 1)])
  rownames(mse_table) <- c("MSE", "", "")
  list(estimates = estimates, MSE = mse_table)
}

#' Iterate the STEM algorithm until convergence
#'
#' @param initial_sigma [numeric] Initial heterogeneity guess.
#' @param beta_sorted [numeric] Ordered effects.
#' @param se_sorted [numeric] Ordered standard errors.
#' @param param [numeric] Vector with tolerance and maximum iterations.
#' @return List with converged estimates and MSE table.
stem_converge <- function(initial_sigma, beta_sorted, se_sorted, param) {
  tolerance <- param[1]
  max_iterations <- param[2]
  sigma_current <- initial_sigma
  converged <- FALSE
  iterations <- 0
  repeat {
    output <- stem_compute(beta_sorted, se_sorted, sigma_current)
    sigma_new <- output$estimates[3]
    iterations <- iterations + 1
    if (abs(sigma_current - sigma_new) < tolerance || iterations > max_iterations) {
      converged <- TRUE
    }
    if (converged) {
      break
    }
    sigma_current <- sigma_new
  }
  list(estimates = c(output$estimates, iterations), MSE = output$MSE)
}

#' Run the STEM bias correction method
#'
#' @param beta [numeric] Effect sizes.
#' @param se [numeric] Standard errors.
#' @param param [numeric] Vector with tolerance and maximum iterations.
#' @return List with estimates and MSE diagnostics.
stem <- function(beta, se, param) {
  n_study <- length(beta)
  beta_equal <- mean(beta)
  max_sigma_squared <- variance_0(n_study, beta, se, beta_equal)
  max_sigma <- sqrt(max_sigma_squared)
  min_sigma <- 0
  data_sorted <- cbind(beta, se)[order(se), ]
  beta_sorted <- data_sorted[, 1]
  se_sorted <- data_sorted[, 2]
  output_max <- stem_converge(max_sigma, beta_sorted, se_sorted, param)
  output_min <- stem_converge(min_sigma, beta_sorted, se_sorted, param)
  diff_sigma <- abs(output_max$estimates[3] - output_min$estimates[3])
  multiple <- as.integer(diff_sigma > (2 * param[1]))
  n_stem <- output_max$estimates[4]
  sigma0 <- output_max$estimates[3]
  info_in_sample <- sum(1 / (se_sorted[1:n_stem]^2 + sigma0^2)) / sum(1 / (se_sorted^2 + sigma0^2))
  estimates <- c(output_max$estimates, multiple, info_in_sample)
  colnames_matrix <- c("estimate", "se", "sd of total heterogeneity", "n_stem", "n_iteration", "multiple", "% info used")
  estimates_matrix <- matrix(estimates, nrow = 1)
  colnames(estimates_matrix) <- colnames_matrix
  mse_matrix <- t(output_max$MSE)
  colnames(mse_matrix) <- c("MSE", "variance", "bias_squared")
  list(estimates = estimates_matrix, MSE = mse_matrix)
}

#' Rescale standard errors for funnel plots
se_rescale <- function(se) {
  -log10(se)
}

#' Funnel plot helper for the STEM estimates
stem_funnel <- function(beta_input, se_input, stem_estimates, theme, legend_pos = "topleft") {
  colors <- list(
    blue = c("#005CAB", "#e8e813", "#4a19bd"),
    yellow = c("#AB9800", "#009B0F", "#4dacfa"),
    green = c("#009B0F", "#AB0000", "#423091"),
    red = c("#AB0000", "#6A0DAB", "#8cb1fa"),
    purple = c("#6A0DAB", "#005CAB", "#fa8ce6")
  )
  palette <- colors[[theme]]
  if (is.null(palette)) {
    cli::cli_abort("Invalid theme type.")
  }
  b_stem <- stem_estimates[1]
  se_b_stem <- stem_estimates[2]
  sigma0 <- stem_estimates[3]
  n_stem <- stem_estimates[4]
  data_sorted <- cbind(beta_input, se_input)[order(se_input), ]
  beta_sorted <- data_sorted[, 1]
  se_sorted <- data_sorted[, 2]
  cumulative <- weighted_mean(beta_sorted, se_sorted, sigma0)
  se_axis <- se_rescale(se_sorted)
  plot.new()
  graphics::par(mar = c(4.1, 4.1, 1, 1), bg = grDevices::rgb(255, 255, 255, maxColorValue = 255)) # nolint: undesirable_function_linter.
  plot(beta_sorted, se_axis,
    col = palette[1], pch = 1, lwd = 2.5,
    xlim = range(beta_input) + c(-1, 1) * diff(range(beta_input)) / 15,
    xlab = expression(paste("Coefficient ", beta)),
    ylab = expression(paste("Precision ", -log(SE)))
  )
  lines(cumulative, se_axis, col = grDevices::rgb(96, 96, 96, maxColorValue = 255), lwd = 2.5)
  points(b_stem, se_axis[n_stem], pch = 18, col = palette[3], cex = 2)
  segments(b_stem, se_axis[1], b_stem, min(se_axis), col = palette[3], lwd = 2.5)
  points(b_stem, se_axis[1], pch = 18, col = palette[2], cex = 2)
  segments(b_stem - 1.96 * se_b_stem, se_axis[1], b_stem + 1.96 * se_b_stem, se_axis[1], col = palette[2], lwd = 2.5)
  abline(v = 0, col = grDevices::rgb(192, 192, 192, maxColorValue = 255), lty = 2, lwd = 2.5)
  legend(legend_pos,
    legend = c("stem-based estimate", "95 confidence interval", "cumulative estimate", "minimal precision", "study"),
    col = c(palette[2], palette[2], grDevices::rgb(96, 96, 96, maxColorValue = 255), palette[3], palette[1]),
    bty = "n",
    lty = c(NA, 1, 1, NA, NA), lwd = c(NA, 2, 2, NA, 2.5),
    pch = c(18, NA, NA, 18, 1),
    pt.cex = 1.8, cex = 0.8, horiz = FALSE, inset = c(0, 0),
    y.intersp = 1.5
  )
}

#' Plot MSE diagnostics for the STEM method
stem_MSE <- function(MSE_matrix) {
  mse <- MSE_matrix[, 1]
  bias_sq <- MSE_matrix[, 3]
  variance <- MSE_matrix[, 2]
  n_study <- nrow(MSE_matrix)
  n_min <- 2
  num_study <- (n_min + 1):(n_study + 1)
  layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE))
  plot(num_study, bias_sq[n_min:n_study], type = "l", col = "blue", lwd = 2.5, xlab = "Num of included studies i", ylab = "", main = expression(Bias^2 - b[0]^2))
  plot(num_study, variance[n_min:n_study], type = "l", col = "blue", lwd = 2.5, xlab = "Num of included studies i", ylab = "", main = expression(Variance))
  plot(num_study, mse[n_min:n_study], type = "l", col = "blue", lwd = 2.5, xlab = "Num of included studies i", ylab = "", main = expression(MSE - b[0]^2))
}

#' Compute median data within clusters
#'
#' @param data [data.frame] Input data.
#' @param id_var [symbol] Identifier column.
#' @param main_var [symbol] Main variable column.
#' @param additional_var [symbol] Additional variable column.
#' @return Data frame containing the median observations per id.
data_median <- function(data, id_var, main_var, additional_var) {
  complete_data <- na.omit(data)
  column_id <- eval(substitute(complete_data[var], list(var = id_var)))
  colnames(column_id)[1] <- "id"
  column_main <- eval(substitute(complete_data[var], list(var = main_var)))
  colnames(column_main)[1] <- "main"
  column_additional <- eval(substitute(complete_data[var], list(var = additional_var)))
  colnames(column_additional)[1] <- "additional"
  merged_main <- merge(column_id, column_main, by = 0, all = TRUE)
  merged_additional <- merge(column_id, column_additional, by = 0, all = TRUE)
  median_only <- stats::aggregate(main ~ id, merged_main, median)
  median_together <- merge(median_only, merged_main, by.x = "id", by.y = "id")
  merge(median_together, merged_additional, by.x = "Row.names", by.y = "Row.names")
}


box::export(
  weighted_mean,
  weighted_mean_squared,
  compute_submatrix_sums,
  variance_0,
  variance_b,
  stem_compute,
  stem_converge,
  stem,
  stem_funnel,
  stem_MSE,
  data_median
)

# nolint end: object_name_linter.
