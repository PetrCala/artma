box::use(
  stats[ecdf, pbinom, pchisq, pnorm, qnorm, rnorm]
)

#' Format a numeric value with a fixed number of decimals
format_decimal <- function(x, k) {
  trimws(format(round(x, k), nsmall = k))
}

#' Linearly spaced sequence between two bounds
linspace <- function(start, stop, n) {
  seq(from = start, to = stop, length.out = n)
}

#' Simulate Brownian bridge suprema CDFs used by the LCM test
#'
#' @param iterations [integer] Number of simulations.
#' @param grid_points [integer] Number of grid points per simulation.
#' @return Numeric vector of simulated suprema.
simulate_cdfs <- function(iterations = 10000, grid_points = 10000) {
  c_grid <- seq_len(grid_points) / grid_points
  bb_sup <- numeric(iterations)
  for (m in seq_len(iterations)) {
    eps <- rnorm(grid_points, mean = 0, sd = 1) / sqrt(grid_points)
    w <- cumsum(eps)
    b <- w - c_grid * w[grid_points]
    c_values <- c(0, c_grid)
    b_values <- c(0, b)
    hull <- fdrtool::gcmlcm(c_values, b_values, type = "lcm")
    y <- numeric(length(c_values))
    y[1] <- 0
    for (s in 2:length(hull$x.knots)) {
      a <- hull$y.knots[s] - hull$slope.knots[s - 1] * hull$x.knots[s]
      b_slope <- hull$slope.knots[s - 1]
      lower <- hull$x.knots[s - 1] * grid_points + 1
      upper <- hull$x.knots[s] * grid_points
      xx <- seq(lower, upper) / grid_points
      yy <- a + b_slope * xx
      y[lower:upper] <- yy
    }
    bb_sup[m] <- max(abs(y - b_values))
  }
  bb_sup
}

#' Binomial test for excess significant results
binomial_test <- function(P, p_min, p_max, type) {
  filtered <- switch(type,
    c = P[P <= p_max & P >= p_min],
    o = P[P < p_max & P > p_min],
    stop("Unknown test type.")
  )
  nn <- length(filtered)
  kk <- sum(filtered > (p_max + p_min) / 2)
  1 - pbinom(kk - 1, nn, 0.5)
}

#' LCM-based test for shape restrictions
lcm_test <- function(P, p_min, p_max, norm, cdfs) {
  filtered <- P[P <= p_max & P >= p_min]
  nn <- length(filtered)
  f <- ecdf(filtered)
  x <- seq(0, 1, length.out = 1000)
  y <- f(x * (p_max - p_min) + p_min)
  hull <- fdrtool::gcmlcm(x, y, type = "lcm")
  z <- numeric(length(x))
  z[1] <- hull$y.knots[1]
  for (s in 2:length(hull$x.knots)) {
    a <- hull$y.knots[s] - hull$slope.knots[s - 1] * hull$x.knots[s]
    b <- hull$slope.knots[s - 1]
    lower <- hull$x.knots[s - 1]
    upper <- hull$x.knots[s]
    segment <- x[x > lower & x <= upper]
    z[x > lower & x <= upper] <- a + b * segment
  }
  bm_sup <- sqrt(nn) * max(abs(y - z))
  1 - ecdf(cdfs)(bm_sup)
}

#' Fisher combination test adapted for truncated p-values
fisher_test <- function(P, p_min, p_max) {
  filtered <- P[P < p_max & P >= p_min]
  nn <- length(filtered)
  stat <- -2 * sum(log(1 - (filtered - p_min) / (p_max - p_min)))
  1 - pchisq(stat, df = 2 * nn)
}

#' Discontinuity test based on rddensity
run_discontinuity_test <- function(P, c, h) {
  h_band <- h - 0.001
  repeat {
    h_band <- h_band + 0.001
    res <- rddensity::rddensity(P, c = c, h = h_band)
    if (!is.na(res$test$p_jk)) {
      break
    }
  }
  res$test$p_jk
}

#' Lambda function used in the Cox-Shi bounds
lambda2 <- function(x1, x2, h) {
  pnorm(qnorm(1 - x1 / 2) - h) - pnorm(qnorm(1 - x2 / 2) - h) +
    pnorm(qnorm(1 - x1 / 2) + h) - pnorm(qnorm(1 - x2 / 2) + h)
}

#' Compute bounds for the p-curve and its derivatives
compute_bounds <- function(pmax, J, order) {
  h <- seq(0, 100, by = 0.001)
  grid <- linspace(0, pmax, J + 1)
  width <- switch(order,
    0 = J,
    1 = J - 1,
    2 = J - 2,
    stop("Unsupported order")
  )
  bounds <- numeric(width)
  for (j in seq_len(width)) {
    lambda_left <- lambda2(grid[j], grid[j + 1], h)
    if (order == 0) {
      bounds[j] <- max(lambda_left)
    } else if (order == 1) {
      lambda_right <- lambda2(grid[j + 1], grid[j + 2], h)
      bounds[j] <- max(abs(lambda_right - lambda_left))
    } else {
      lambda_mid <- lambda2(grid[j + 2], grid[j + 3], h)
      bounds[j] <- max(abs(lambda_mid - 2 * lambda2(grid[j + 1], grid[j + 2], h) + lambda_left))
    }
  }
  bounds[1] <- 1
  bounds
}

bound0 <- function(pmax, J) compute_bounds(pmax, J, 0)
bound1 <- function(pmax, J) compute_bounds(pmax, J, 1)
bound2 <- function(pmax, J) compute_bounds(pmax, J, 2)

#' Filter p-values and optional study identifiers
filter_pvalues <- function(Q, ind, p_min, p_max) {
  mask <- Q <= p_max & Q >= p_min
  list(P = Q[mask], ind = if (length(ind) > 1) ind[mask] else ind)
}

#' Empirical probability vector across bins
compute_phat <- function(P, J, p_min, p_max) {
  bins <- seq(p_min, p_max, length.out = J + 1)
  phat <- numeric(J - 1)
  for (s in seq_len(J - 1)) {
    phat[s] <- sum((P > bins[s]) & (P <= bins[s + 1])) / length(P)
  }
  phat[1] <- phat[1] + sum(P == bins[1]) / length(P)
  list(phat = phat, bins = bins)
}

#' Covariance matrix of the empirical distribution under clustering
compute_omega <- function(P, ind, phat, bins) {
  if (length(ind) > 1) {
    omega <- matrix(0, length(phat), length(phat))
    for (cluster in unique(ind)) {
      X <- P[ind == cluster]
      mq <- build_indicator_matrix(X, bins, phat)
      omega <- omega + mq %*% t(mq)
    }
    omega / length(P)
  } else {
    qhat <- phat * length(P) / (length(P) + 1) + 1 / (length(phat) + 1) / (length(P) + 1)
    diag(qhat) - qhat %*% t(qhat)
  }
}

build_indicator_matrix <- function(X, bins, phat) {
  J <- length(phat) + 1
  mq <- matrix(0, nrow = length(X), ncol = J - 1)
  for (q in seq_along(X)) {
    mq[q, ] <- as.numeric((X[q] > head(bins, -1)) & (X[q] <= tail(bins, -1)))
    mq[q, X[q] == 0 & seq_len(J - 1) == 1] <- 1
  }
  sweep(mq, 2, phat, `-`)
}

build_difference_matrix <- function(J) {
  D <- matrix(0, nrow = J - 1, ncol = J)
  for (i in seq_len(J - 1)) {
    D[i, i] <- -1
    D[i, i + 1] <- 1
  }
  D
}

extend_difference_matrix <- function(D, J, K) {
  if (K <= 1) {
    return(-D)
  }
  d <- D
  dk <- -D
  for (k in 2:K) {
    d <- D[1:(J - k), 1:(J - k + 1)] %*% d
    dk <- rbind(dk, (-1)^k * d)
  }
  dk
}

build_constraint_vector <- function(B0, B1, B2, Galpha, use_bounds, J, K) {
  if (identical(use_bounds, 0)) {
    return(c(-B0))
  }
  b0 <- -B0 / Galpha
  b1 <- -B1 / Galpha
  b2 <- -B2 / Galpha
  b0[1] <- -1
  b1[1] <- -1
  b2[1] <- -1
  c(b0, b1, b2)
}

solve_coxshi_problem <- function(t0, fn, A, b) {
  res <- tryCatch(NlcOptim::fmincon(t0, fn, A = A, b = b), error = function(e) NULL)
  while (is.null(res) || !is.list(res)) {
    ru <- runif(length(t0))
    t0 <- matrix(ru / sum(ru), ncol = 1)
    res <- tryCatch(NlcOptim::fmincon(t0, fn, A = A, b = b), error = function(e) NULL)
  }
  res
}

cox_shi_test <- function(Q, ind, p_min, p_max, J, K, use_bounds) {
  filtered <- filter_pvalues(Q, ind, p_min, p_max)
  P <- filtered$P
  ind_filtered <- filtered$ind
  N <- length(P)
  Galpha <- N / length(Q)
  phat_data <- compute_phat(P, J, p_min, p_max)
  phat <- phat_data$phat
  bins <- phat_data$bins
  B0 <- bound0(p_max, J)
  B1 <- bound1(p_max, J)
  B2 <- bound2(p_max, J)
  if (identical(use_bounds, 0)) {
    B0 <- rep(1, J)
  }
  omega <- compute_omega(P, ind_filtered, phat, bins)
  D <- build_difference_matrix(J)
  dk <- extend_difference_matrix(D, J, K)
  if (identical(use_bounds, 0)) {
    dk <- rbind(-diag(J), diag(J), dk)
  } else {
    dk <- rbind(-diag(J), -dk, diag(J), dk)
  }
  eJ <- rep(0, J)
  eJ[J] <- 1
  F1 <- rbind(-diag(J - 1), rep(1, J - 1))
  constraint_vector <- if (identical(use_bounds, 0)) {
    rep(0, nrow(dk))
  } else {
    build_constraint_vector(B0, B1, B2, Galpha, use_bounds, J, K)
  }
  A <- dk %*% F1
  b_vec <- dk %*% eJ - constraint_vector
  fn <- function(t) {
    diff_vec <- phat - t
    N * t(diff_vec) %*% solve(omega) %*% diff_vec
  }
  start <- matrix(1 / (J - 1), ncol = 1, nrow = J - 1)
  result <- solve_coxshi_problem(start, fn, A, b_vec)
  if (is.null(result) || is.null(result$par)) {
    return(NA_real_)
  }
  t_opt <- result$par
  stat <- fn(t_opt)
  Ba <- A[result$info$lambda$ineqlin > 0, , drop = FALSE]
  JX <- qr(Ba)$rank
  if (result$convergence == 0 && JX > 0) {
    1 - pchisq(stat, df = JX)
  } else {
    NA_real_
  }
}
