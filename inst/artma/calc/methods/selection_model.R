box::use(
  stats[dt, nlminb, pt],
  base[diag, diff, order, sort, cumsum, sqrt]
)

#' Compute a clustered covariance estimate for the score matrix
#'
#' @param score_matrix [matrix] Matrix of score contributions.
#' @param cluster_index [integer] Cluster identifiers.
#' @return Cluster robust covariance matrix.
clustered_covariance_estimate <- function(score_matrix, cluster_index) {
  order_index <- order(cluster_index)
  ordered_clusters <- sort(cluster_index)
  ordered_scores <- score_matrix[order_index, , drop = FALSE]
  centered <- sweep(ordered_scores, 2, colMeans(ordered_scores))
  cumulative <- apply(centered, 2, cumsum)
  cluster_breaks <- c(diff(ordered_clusters) != 0, TRUE)
  aggregated <- cumulative[cluster_breaks, , drop = FALSE]
  aggregated <- rbind(aggregated[1, ], diff(aggregated))
  crossprod(aggregated) / (nrow(centered) - 1)
}

#' Finite difference approximation of the information matrix
#'
#' @param thetahat [numeric] Parameter vector at the optimum.
#' @param stepsize [numeric] Finite difference step.
#' @param llh [function] Function returning a list with element `LLH` for the objective.
#' @return Approximated Hessian matrix.
compute_information_matrix <- function(thetahat, stepsize, llh) {
  p <- length(thetahat)
  info <- matrix(0, p, p)
  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      theta_pp <- thetahat
      theta_pm <- thetahat
      theta_mp <- thetahat
      theta_mm <- thetahat
      theta_pp[i] <- theta_pp[i] + stepsize
      theta_pp[j] <- theta_pp[j] + stepsize
      theta_pm[i] <- theta_pm[i] + stepsize
      theta_pm[j] <- theta_pm[j] - stepsize
      theta_mp[i] <- theta_mp[i] - stepsize
      theta_mp[j] <- theta_mp[j] + stepsize
      theta_mm[i] <- theta_mm[i] - stepsize
      theta_mm[j] <- theta_mm[j] - stepsize
      ll_pp <- llh(theta_pp)$LLH
      ll_pm <- llh(theta_pm)$LLH
      ll_mp <- llh(theta_mp)$LLH
      ll_mm <- llh(theta_mm)$LLH
      diff_plus <- (ll_pp - ll_pm) / (2 * stepsize)
      diff_minus <- (ll_mp - ll_mm) / (2 * stepsize)
      info[i, j] <- (diff_plus - diff_minus) / (2 * stepsize)
    }
  }
  info
}

#' Finite difference approximation of the score matrix
#'
#' @param thetahat [numeric] Parameter vector at the optimum.
#' @param stepsize [numeric] Finite difference step.
#' @param llh [function] Function returning a list with element `logL` for individual log-likelihoods.
#' @return Matrix of individual score contributions.
compute_score_matrix <- function(thetahat, stepsize, llh) {
  p <- length(thetahat)
  n <- length(llh(thetahat)$logL)
  score_mat <- matrix(0, n, p)
  for (i in seq_len(p)) {
    theta_plus <- thetahat
    theta_minus <- thetahat
    theta_plus[i] <- theta_plus[i] + stepsize
    theta_minus[i] <- theta_minus[i] - stepsize
    log_plus <- llh(theta_plus)$logL
    log_minus <- llh(theta_minus)$logL
    score_mat[, i] <- (log_plus - log_minus) / (2 * stepsize)
  }
  score_mat
}

#' Robust variance estimation via finite differences and clustered covariance
#'
#' @param stepsize [numeric] Finite difference step.
#' @param n [integer] Sample size.
#' @param thetahat [numeric] Parameter vector at the optimum.
#' @param llh [function] Likelihood function returning `LLH` and `logL`.
#' @param cluster_ID [integer] Cluster identifiers.
#' @return Robust covariance matrix of the parameters.
robust_variance <- function(stepsize, n, thetahat, llh, cluster_ID) { # nolint: object_name_linter.
  info <- compute_information_matrix(thetahat, stepsize, llh)
  score_mat <- compute_score_matrix(thetahat, stepsize, llh)
  covariance <- clustered_covariance_estimate(score_mat, cluster_ID)
  solve(info) %*% covariance %*% solve(info) * n
}

#' Indicator matrix for publication probability cut-offs
#'
#' @param TT [numeric] Test statistics.
#' @param cutoffs [numeric] Cut-off thresholds.
#' @param symmetric [logical] Should symmetry be enforced?
#' @return Matrix of indicators.
compute_tpowers <- function(TT, cutoffs, symmetric) {
  n <- length(TT)
  tpowers <- matrix(0, n, length(cutoffs) + 1)
  if (symmetric) {
    TT <- abs(TT)
  }
  tpowers[, 1] <- TT < cutoffs[1]
  if (length(cutoffs) > 1) {
    for (m in 2:length(cutoffs)) {
      tpowers[, m] <- (TT < cutoffs[m]) * (TT >= cutoffs[m - 1])
    }
  }
  tpowers[, length(cutoffs) + 1] <- TT >= cutoffs[length(cutoffs)]
  tpowers
}

#' Log-likelihood for the selection model with variance variation
#'
#' @param lambdabar [numeric] Mean parameter.
#' @param tauhat [numeric] Random effects standard deviation.
#' @param betap [numeric] Publication probabilities.
#' @param cutoffs [numeric] Cut-offs for the selection model.
#' @param symmetric [logical] Should symmetry be enforced?
#' @param X [numeric] Observed effects.
#' @param sigma [numeric] Standard errors.
#' @param tpowers [matrix] Indicator matrix produced by [compute_tpowers()].
#' @param df [numeric] Degrees of freedom for the t distribution. Defaults to `Inf` (normal).
#' @return List with `LLH` and per observation `logL`.
variation_variance_loglikelihood <- function(lambdabar, tauhat, betap, cutoffs, symmetric, X, sigma, tpowers, df = Inf) {
  n <- length(X)
  betap <- matrix(betap, nrow = length(betap))
  phat <- tpowers %*% betap
  sigmatilde <- sqrt(sigma^2 + tauhat^2)
  density <- dt((X - lambdabar) / sigmatilde, df) / sigmatilde
  normalized_cutoffs <- outer(sigma / sigmatilde, cutoffs, `*`) - lambdabar / sigmatilde
  if (symmetric) {
    negative_cutoffs <- outer(sigma / sigmatilde, -cutoffs, `*`) - lambdabar / sigmatilde
    cdfs <- pt(normalized_cutoffs, df) - pt(negative_cutoffs, df)
  } else {
    cdfs <- pt(normalized_cutoffs, df)
  }
  cdfs <- cbind(rep(0, n), cdfs, rep(1, n))
  cell_probabilities <- cdfs[, -1, drop = FALSE] - cdfs[, -(length(cutoffs) + 2), drop = FALSE]
  normalizing_constant <- cell_probabilities %*% betap
  likelihood <- phat * density / normalizing_constant
  log_likelihood <- log(likelihood)
  list(LLH = -sum(log_likelihood), logL = log_likelihood)
}

#' Estimate the selection model parameters
#'
#' @param X [numeric] Observed effects.
#' @param sigma [numeric] Standard errors.
#' @param cutoffs [numeric] Cut-off thresholds.
#' @param symmetric [logical] Should symmetry be enforced?
#' @param model [character] Either "normal" or "t".
#' @return A list with parameter estimates and robust standard errors.
metastudies_estimation <- function(X, sigma, cutoffs, symmetric, model = "normal") {
  max_eval <- 1e5
  max_iter <- 1e5
  tol <- 1e-8
  stepsize <- 1e-6
  n <- length(X)
  TT <- X / sigma
  tpowers <- compute_tpowers(TT, cutoffs, symmetric)
  if (model == "normal") {
    llh <- function(Psi) variation_variance_loglikelihood(Psi[1], Psi[2], c(Psi[-c(1, 2)], 1), cutoffs, symmetric, X, sigma, tpowers) # nolint: object_name_linter.
    start <- c(0, 1, rep(1, length(cutoffs)))
  } else if (model == "t") {
    llh <- function(Psi) variation_variance_loglikelihood(Psi[1], Psi[2], c(Psi[-c(1, 2, 3)], 1), cutoffs, symmetric, X, sigma, tpowers, df = Psi[3]) # nolint: object_name_linter.
    start <- c(0, 1, 10, rep(1, length(cutoffs)))
  } else {
    cli::cli_abort("Unsupported model specification")
  }
  objective <- function(Psi) llh(Psi)$LLH # nolint: object_name_linter.
  lower <- c(-Inf, rep(0, length(start) - 1))
  upper <- rep(Inf, length(start))
  optimum <- nlminb(start = start, objective = objective, lower = lower, upper = upper, control = list(eval.max = max_eval, iter.max = max_iter, abs.tol = tol))
  params <- optimum$par
  covariance <- robust_variance(stepsize, n, params, llh, seq_len(n))
  standard_errors <- sqrt(diag(covariance))
  list(Psihat = params, SE = standard_errors)
}

#' Tidy table of selection model estimates
#'
#' @param Psihat [numeric] Parameter estimates.
#' @param SE [numeric] Standard errors.
#' @param cutoffs [numeric] Cut-off thresholds.
#' @param symmetric [logical] Should symmetry be enforced?
#' @param model [character] Either "normal" or "t".
#' @return Matrix with estimates and standard errors, matching the legacy layout.
estimates_table <- function(Psihat, SE, cutoffs, symmetric, model) { # nolint: object_name_linter.
  params <- rbind(Psihat, SE)
  rownames(params) <- c("estimate", "standard error")
  colnames(params) <- rep(" ", ncol(params))
  colnames(params)[1] <- intToUtf8(956)
  colnames(params)[2] <- intToUtf8(964)
  shift <- if (model == "t") 1 else 0
  if (model == "t") {
    colnames(params)[3] <- "df"
  }
  start_index <- 3 + shift
  if (symmetric) {
    colnames(params)[start_index] <- sprintf("[0, %s]", cutoffs[1])
    if (length(cutoffs) > 1) {
      for (i in 2:length(cutoffs)) {
        colnames(params)[start_index + i - 1] <- sprintf("(%s, %s]", cutoffs[i - 1], cutoffs[i])
      }
    }
  } else {
    colnames(params)[start_index] <- sprintf("(-%s, %s]", intToUtf8(8734), cutoffs[1])
    if (length(cutoffs) > 1) {
      for (i in 2:length(cutoffs)) {
        colnames(params)[start_index + i - 1] <- sprintf("(%s, %s]", cutoffs[i - 1], cutoffs[i])
      }
    }
  }
  params
}

# nolint start: object_name_linter.
# Backwards compatible aliases
RobustVariance <- robust_variance
Clustered_covariance_estimate <- clustered_covariance_estimate
Tpowers_fun <- compute_tpowers
VariationVarianceLogLikelihood <- variation_variance_loglikelihood
estimatestable <- estimates_table
# nolint end: object_name_linter.

box::export(
  robust_variance,
  clustered_covariance_estimate,
  compute_tpowers,
  variation_variance_loglikelihood,
  estimates_table,
  RobustVariance,
  Clustered_covariance_estimate,
  Tpowers_fun,
  VariationVarianceLogLikelihood,
  estimatestable,
  metastudies_estimation,
  compute_score_matrix
)
