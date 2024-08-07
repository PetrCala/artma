### RobustVariance.R
### Source: https://github.com/maxkasy/MetaStudiesApp/blob/master/RobustVariance.R

RobustVariance <- function(stepsize, n, thetahat, LLH, cluster_ID) {
  Info <- matrix(0, length(thetahat), length(thetahat));
  for (n1 in 1:length(thetahat)) {
    for (n2 in 1:length(thetahat)) {
      thetaplusplus <- thetahat;
      thetaplusminus <- thetahat;
      thetaminusplus <- thetahat;
      thetaminusminus <- thetahat;



      thetaplusplus[n1] <- thetaplusplus[n1] + stepsize;
      thetaplusplus[n2] <- thetaplusplus[n2] + stepsize;
      LLH_plusplus <- LLH(thetaplusplus);
      LLH_plusplus <- LLH_plusplus$LLH

      thetaplusminus[n1] <- thetaplusminus[n1] + stepsize;
      thetaplusminus[n2] <- thetaplusminus[n2] - stepsize;
      LLH_plusminus <- LLH(thetaplusminus);
      LLH_plusminus <- LLH_plusminus$LLH

      thetaminusplus[n1] <- thetaminusplus[n1] - stepsize;
      thetaminusplus[n2] <- thetaminusplus[n2] + stepsize;
      LLH_minusplus <- LLH(thetaminusplus);
      LLH_minusplus <- LLH_minusplus$LLH

      thetaminusminus[n1] <- thetaminusminus[n1] - stepsize;
      thetaminusminus[n2] <- thetaminusminus[n2] - stepsize;
      LLH_minusminus <- LLH(thetaminusminus);
      LLH_minusminus <- LLH_minusminus$LLH

      Info[n1, n2] = ((LLH_plusplus - LLH_plusminus) / (2 * stepsize) - (LLH_minusplus - LLH_minusminus) / (2 * stepsize)) / (2 * stepsize);
    }
  }

  Var = solve(Info);
  # show(Var)
  # %Calculate misspecification-robust standard errors
  score_mat <- matrix(0, n, length(thetahat));
  for (n1 in 1:length(thetahat)) {
    theta_plus <- thetahat;
    theta_plus[n1] <- theta_plus[n1] + stepsize;
    funvalue <- LLH(theta_plus);

    # funvalue<-funvalue$LLH;
    LLH_plus <- funvalue$LLH;
    logL_plus <- funvalue$logL;

    theta_plus <- thetahat;
    theta_plus[n1] <- theta_plus[n1] - stepsize;
    funvalue <- LLH(theta_plus);
    # funvalue<-funvalue$LLH;
    LLH_minus <- funvalue$LLH;
    logL_minus <- funvalue$logL;

    score_mat[, n1] = (logL_plus - logL_minus) / (2 * stepsize);
  }
  Cov = Clustered_covariance_estimate(score_mat, cluster_ID);
  # show(score_mat)
  # show(Cov)
  Var_robust = n * solve(Info) %*% Cov %*% solve(Info);

  return(Var_robust)
}


Clustered_covariance_estimate <- function(g, cluster_index) {
  # %given a matrix of moment condition values g, compute a clustering-robust
  # %estimate of the covariance matrix Sigma
  I <- order(cluster_index);
  cluster_index <- sort(cluster_index);
  g = g[I, ]
  g = g - matrix(rep(apply(g, 2, mean), length(I)), nrow = length(I), byrow = TRUE);
  gsum = apply(g, 2, cumsum);
  index_diff <- cluster_index[-1] != cluster_index[-length(cluster_index)];
  index_diff <- c(index_diff, 1);

  gsum = gsum[index_diff == 1, ]
  gsum = rbind(gsum[1, ], diff(gsum));
  Sigma = 1 / (dim(g)[1] - 1) * (t(gsum) %*% gsum);


  return(Sigma)
}

### metastudiesfunction.r
### Source: https://github.com/maxkasy/MetaStudiesApp/blob/master/metastudiesfunctions.r




Tpowers_fun = function(TT, cutoffs, symmetric) {
  n = length(TT)
  Tpowers = matrix(0, n, length(cutoffs) + 1)
  if (symmetric) TT = abs(TT)
  Tpowers[, 1] = TT < cutoffs[1]
  if (length(cutoffs) > 1) {
    for (m in 2:length(cutoffs)) {
      Tpowers[, m] = (TT < cutoffs[m]) * (TT >= cutoffs[m - 1]);
    }
  }
  Tpowers[, length(cutoffs) + 1] = (TT) >= cutoffs[length(cutoffs)]
  Tpowers
}



VariationVarianceLogLikelihood <- function(lambdabar, tauhat, betap,
                                           cutoffs, symmetric, X, sigma, Tpowers,
                                           df = Inf) { # if df argument is provided, switch to t-dist
  n = length(X);
  betap = as.matrix(betap, length(betap), 1);

  #  %vector of estimated publication probabilities
  phat = Tpowers %*% betap;

  #  %%%%%%%%%%%%%%%%%%%%%%%%
  # @  vector of un-truncated likelihoods
  sigmatilde = sqrt(sigma^2 + tauhat^2)
  fX = dt((X - lambdabar) / sigmatilde, df) / sigmatilde


  #  normalizingconstant
  normalizedcutoffs = (sigma / sigmatilde) %*% t(cutoffs) - (lambdabar / sigmatilde)
  if (symmetric) {
    normalizednegativecutoffs = (sigma / sigmatilde) %*% t(-cutoffs) - (lambdabar / sigmatilde)
    cdfs = pt(normalizedcutoffs, df) - pt(normalizednegativecutoffs, df)
  } else {
    cdfs = pt(normalizedcutoffs, df)
  }
  cdfs = cbind(rep(0, n), cdfs, rep(1, n))
  cellprobas = cdfs[, -1] - cdfs[, -(length(cutoffs) + 2)]
  normalizingconst = cellprobas %*% betap;

  # likelihood for each observation
  L <- phat * fX / normalizingconst;
  logL <- log(L);
  # objective function; note the sign flip, since we are doing minimization
  LLH <- -sum(logL);

  if (is.nan(LLH)) {
    show(lambdabar)
    show(tauhat)
    show(betap)
  }

  return(list(LLH = LLH, logL = logL))

}



metastudies_estimation = function(X, sigma, cutoffs, symmetric, model = "normal") {
  # parameters for optimization
  MaxEval <- 10^5
  MaxIter <- 10^5;
  Tol <- 10^(-8);
  stepsize <- 10^(-6);

  nn = length(X)
  # %regressors for step function p
  TT = X / sigma;
  Tpowers = Tpowers_fun(TT, cutoffs, symmetric)

  if (model == "normal") {
    LLH <- function(Psi) VariationVarianceLogLikelihood(Psi[1], Psi[2], c(Psi[-c(1, 2)], 1), cutoffs, symmetric, X, sigma, Tpowers)
    Psihat0 = c(0, 1, rep(1, length(cutoffs))) # starting values
  } else if (model == "t") {
    LLH <- function(Psi) VariationVarianceLogLikelihood(Psi[1], Psi[2], c(Psi[-c(1, 2, 3)], 1), cutoffs, symmetric, X, sigma, Tpowers, df = Psi[3])
    Psihat0 = c(0, 1, 10, rep(1, length(cutoffs))) # starting values
  }


  LLH_only <- function(Psi) {
    A <- LLH(Psi);
    return(A$LLH)
  }

  lower.b = c(-Inf, rep(0, length(Psihat0) - 1))
  upper.b = rep(Inf, length(Psihat0))

  findmin <- nlminb(objective = LLH_only, start = Psihat0, lower = lower.b, upper = upper.b, control = list(eval.max = MaxEval, iter.max = MaxIter, abs.tol = Tol));
  Psihat <- findmin$par
  LLHmax <- findmin$objective

  Var_robust <- RobustVariance(stepsize, nn, Psihat, LLH, 1:nn);
  se_robust <- sqrt(diag(Var_robust));

  list(Psihat = Psihat, SE = se_robust)
}

estimatestable = function(Psihat, SE, cutoffs, symmetric, model) {
  l = length(Psihat)
  estimates = matrix(0, 2, l)
  estimates[1, ] = Psihat
  estimates[2, ] = SE
  rownames(estimates) = c("estimate", "standard error")
  colnames(estimates) = rep(" ", l)
  colnames(estimates)[1] = intToUtf8(956) # mu
  colnames(estimates)[2] = intToUtf8(964) # tau
  if (model == "t") {
    colnames(estimates)[3] = "df"
    shift = 1
  } else {shift = 0}

  if (symmetric) {
    colnames(estimates)[3 + shift] = paste("[0,", cutoffs[1], "]")
    for (i in seq(2, length(cutoffs), length = max(0, length(cutoffs) - 1))) {
      colnames(estimates)[2 + i + shift] = paste("(", cutoffs[i - 1], ",", cutoffs[i], "]")
    }
  } else {
    colnames(estimates)[3 + shift] = paste("(-", intToUtf8(8734), ",", cutoffs[1], "]")
    for (i in seq(2, length(cutoffs), length = max(0, length(cutoffs) - 1))) {
      colnames(estimates)[2 + i + shift] = paste("(", cutoffs[i - 1], ",", cutoffs[i], "]")
    }
  }
  estimates
}
