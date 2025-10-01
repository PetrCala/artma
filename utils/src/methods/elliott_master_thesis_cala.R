### Publication Bias - Eliott et al. (2022)
### Source: https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA18583
### Several changes made for faster performance / memory allocation

#####################################################################
# Tests for Application 1
# Paper: Detecting p-hacking
# Authors: G. Elliott, N. Kudrin, K. Wuthrich
#####################################################################

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))

getCDFs <- function() {
  # Simulate cdfs for LCM tests
  M = 10000
  N = 10000
  c = 1:N
  c = c / N
  BBsup = matrix(0, nrow = M, ncol = 1)

  for (m in 1:M) {
    eps <- rnorm(n = N, sd = 1, mean = 0)
    eps = eps / sqrt(N)
    W = cumsum(eps)
    B = W - c * W[N]
    C = c(0, c)
    B = c(0, B)
    ll = gcmlcm(C, B, type = "lcm")

    # Preallocate y vector with the same length as C
    y = numeric(length(C))
    y[1] = 0

    for (s in 2:length(ll$x.knots)) {
      a = ll$y.knots[s] - ll$slope.knots[s - 1] * ll$x.knots[s]
      b = ll$slope.knots[s - 1]
      xl = ll$x.knots[s - 1] * N + 1
      xu = ll$x.knots[s] * N
      xx = xl:xu
      xx = xx / N
      yy = a + b * xx

      # Fill in the y vector with the computed yy values
      y[xl:xu] = yy
    }
    BBsup[m, 1] = max(abs(y - B))
  }
  cdfs <- BBsup[, 1] # To a numeric vector
  return(cdfs)
}

##### Binomial
Binomial <- function(P, p_min, p_max, type) {
  if (type == "c") {
    P = P[P <= p_max & P >= p_min]
  }
  if (type == "o") {
    P = P[P < p_max & P > p_min]
  }
  nn = length(P)
  kk = sum(P > (p_max + p_min) / 2)
  return(1 - pbinom(kk - 1, nn, 0.5))
}


LCM <- function(P, p_min, p_max, norm, cdfs) {
  # norm = 2 - 2-norm
  # norm = 8 - sup-norm
  P = P[P <= p_max & P >= p_min]
  nn = length(P)
  f <- ecdf(P)
  x = seq(0, 1, length = 1000)
  y = f(x * (p_max - p_min) + p_min)
  ll = gcmlcm(x, y, type = "lcm")
  # Preallocate the z vector with the same length as x to save memory
  z = numeric(length(x))
  z[1] = ll$y.knots[1]
  for (s in 2:length(ll$x.knots)) {
    a = ll$y.knots[s] - ll$slope.knots[s - 1] * ll$x.knots[s]
    b = ll$slope.knots[s - 1]
    xl = ll$x.knots[s - 1]
    xu = ll$x.knots[s]
    xx = x * (x > xl) * (x <= xu)
    xx = xx[xx != 0]
    yy = a + b * xx

    # Fill in the z vector with the computed yy values
    z[x > xl & x <= xu] = yy
  }
  F_BMsup <- ecdf(cdfs) # External data (CDFs)
  BM = sqrt(nn) * max(abs(y - z))
  return(1 - F_BMsup(BM))
}

####### Fisher
Fisher <- function(P, p_min, p_max) {
  P = P[P < p_max & P >= p_min]
  nn = length(P)
  statFM = -2 * sum(log(1 - (P - p_min) / (p_max - p_min)))
  return(1 - pchisq(statFM, df = 2 * nn))
}


Discontinuity_test <- function(P, c, h) {
  works = 0
  h_band = h - 0.001
  while (works == 0) {
    h_band = h_band + 0.001
    res = rddensity(P, c = 0.05, h = h_band)
    works = 1 - 1 * (is.na(res$test$p_jk))

  }
  clp = rddensity(P, c = 0.05, h = h_band)
  return(clp$test$p_jk)
}


lambda2 <- function(x1, x2, h) {
  lambda = pnorm(qnorm(1 - x1 / 2) - h) - pnorm(qnorm(1 - x2 / 2) - h) + pnorm(qnorm(1 - x1 / 2) + h) - pnorm(qnorm(1 - x2 / 2) + h)
  return(lambda)
}
# Bounds on the p-curve (two-sided t-test);
# (0, pmax] interval with J bins
Bound0 <- function(pmax, J) {
  h = seq(0, 100, by = 0.001)
  X = linspace(0, pmax, J + 1)
  B = matrix(0, J, 1)
  for (j in 1:(J)) {
    Obj1 = lambda2(X[j], X[j + 1], h)
    B[j] = max(Obj1)
  }
  B[1] = 1
  return(B)
}

# Bounds on the first derivative of the p-curve (two-sided t-test);
# (0, pmax] interval with J bins
Bound1 <- function(pmax, J) {
  h = seq(0, 100, by = 0.001)
  X = linspace(0, pmax, J + 1)
  B = matrix(0, J - 1, 1)
  for (j in 1:(J - 1)) {
    Obj1 = lambda2(X[j], X[j + 1], h)
    Obj2 = lambda2(X[j + 1], X[j + 2], h)
    A = Obj2 - Obj1
    B[j] = max(abs(A))
  }
  B[1] = 1
  return(B)
}

# Bounds on the second derivative of the p-curve (two-sided t-test);
# (0, pmax] interval with J bins
Bound2 <- function(pmax, J) {
  h = seq(0, 100, by = 0.001)
  X = linspace(0, pmax, J + 1)
  B = matrix(0, J - 2, 1)
  for (j in 1:(J - 2)) {
    Obj1 = lambda2(X[j], X[j + 1], h)
    Obj2 = lambda2(X[j + 1], X[j + 2], h)
    Obj3 = lambda2(X[j + 2], X[j + 3], h)
    A = Obj3 - 2 * Obj2 + Obj1
    B[j] = max(abs(A))
  }
  B[1] = 1
  return(B)
}

# Cox-Shi test for K-monotonicity on [p_min, p_max] interval and bounds
# Q - vector of p-values; ind - vector of paper ids; J - number of subintervals;
# B=1 to use bounds, B=0 to test without bounds
CoxShi <- function(Q, ind, p_min, p_max, J, K, B) {
  B0 = Bound0(p_max, J)
  B1 = Bound1(p_max, J)
  B2 = Bound2(p_max, J)
  # Use bounds if B=1; no if B=0
  P = Q[Q <= p_max & Q >= p_min]
  if (length(ind) > 1) {
    ind = ind[Q <= p_max & Q >= p_min]
    indu = unique(ind)
  }
  N = length(P)
  Galpha = N / length(Q)
  bin = seq(p_min, p_max, length = J + 1)
  Phat = matrix(0, nrow = J - 1, ncol = 1)

  for (s in 1:(J - 1)) {
    Phat[s] = sum((P > bin[s]) * (P <= bin[s + 1])) / N
  }
  Phat[1] = Phat[1] + sum(P == bin[1]) / N
  if (B == 0) {
    B0 = -matrix(1, nrow = J, ncol = 1)
  }
  if (B == 1) {
    B0 = -B0 / Galpha
    B1 = -B1 / Galpha
    B2 = -B2 / Galpha
    B0[1] = -1
    B1[1] = -1
    B2[1] = -1
  }

  if (length(ind) > 1) {
    Omega = matrix(0, J - 1, J - 1)
    for (i in c(indu)) {
      X = P[ind == i]

      # Preallocate mq vector
      mq = numeric(J - 1)

      for (q in 1:length(X)) {
        for (s in 1:(J - 1)) {
          mq[s] = ((X[q] > bin[s]) * (X[q] <= bin[s + 1])) * 1 + 1 * (X[q] == 0) * (s == 1)
        }
        mq = mq - Phat

        # Preallocate mr vector
        mr = numeric(J - 1)

        for (r in 1:length(X)) {
          for (s in 1:(J - 1)) {
            mr[s] = ((X[r] > bin[s]) * (X[r] <= bin[s + 1])) * 1 + 1 * (X[r] == 0) * (s == 1)
          }
          mr = mr - Phat
          Omega = Omega + mq %*% t(mr)
        }
      }
    }
    Omega = Omega / length(P)
  }
  if (length(ind) == 1) {
    Qhat = Phat * N / (N + 1) + 1 / (J * (N + 1))
    Omega = diag(c(Qhat)) - Qhat %*% t(Qhat)
  }
  D = matrix(0, J - 1, J)
  for (i in 1:(J - 1)) {
    for (j in 1:J) {
      if (i == j) {
        D[i, j] = -1
      }
      if (i + 1 == j) {
        D[i, j] = 1
      }

    }
  }
  Dk = -D
  if (K > 1) {
    d = D
    for (k in 2:K) {
      d = D[1:(J - k), 1:(J - k + 1)] %*% d
      Dk = rbind(Dk, (-1)^k * d)
    }
  }
  if (B == 0) {
    Dk = rbind(-diag(J), diag(J), Dk)
  }
  if (B == 1) {
    Dk = rbind(-diag(J), -Dk, diag(J), Dk)
  }
  eJ = matrix(0, J, 1)
  eJ[J] = 1

  F1 = rbind(-diag(J - 1), matrix(1, 1, J - 1))
  if (B == 1) {
    c = matrix(0, ((2 + K) * J - (K * (K + 1) / 2) + (J - 1) + (J - 2)), 1)
  }
  if (B == 0) {
    c = matrix(0, ((2 + K) * J - (K * (K + 1) / 2)), 1)
  }
  c[1:J] = B0
  if (B == 1) {
    c[(J + 1):(2 * J - 1)] = B1
    c[(2 * J):(3 * J - 3)] = B2
  }
  A = Dk %*% F1
  b = Dk %*% eJ - c

  fn <- function(t) {
    Obj = N * (t(Phat - t)) %*% solve(Omega) %*% ((Phat - t))
    return(Obj)
  }
  t0 = matrix(1, (J - 1), 1) / (J)
  # Problematic part
  if (!require('NlcOptim')) install.packages('NlcOptim'); library('NlcOptim') # For absolute certainty
  # Might fail in case of mishandled packages - if the function gets stuck here,
  # try to uncover the error
  res = tryCatch(fmincon(t0, fn, A = A, b = b), error = function(e) "error")
  # Fool-proof while loop - check if the result is error, but allow for lists to pass
  while (!is.list(res) || any(grepl("error", as.character(res)))) {
    # Generate a new set of initial values for t0
    ru <- runif(J)
    t0 <- matrix(ru[1:(J - 1)] / sum(ru), J - 1, 1)
    # Try to run fmincon again with the new initial values
    res <- tryCatch(fmincon(t0, fn, A = A, b = b), error = function(e) "error")
  }
  # End of problematic part
  t = res
  t_opt = t(t(t$par))
  T = fn(t_opt)
  Ba = A[which(t$info$lambda$ineqlin > 0), ]
  JX = qr(Ba)$rank
  if (t$convergence == 0) {
    return(1 - pchisq(T, df = JX) * (JX > 0))
  }
  else {
    return(999)
  }
}
