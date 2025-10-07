#' Format a numeric value with a fixed number of decimals
format_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))

#' Linearly spaced sequence between two bounds
linspace <- function(start_, stop_, n) seq(from = start_, to = stop_, length.out = n)

#' Simulate Brownian bridge suprema CDFs used by the LCM test
#'
#' @param iterations [integer] Number of simulations.
#' @param grid_points [integer] Number of grid points per simulation.
#' @param show_progress [logical] Whether to show progress bar.
#' @return Numeric vector of simulated suprema.
simulate_cdfs <- function(iterations = 10000, grid_points = 10000, show_progress = TRUE) {
  c_grid <- seq_len(grid_points) / grid_points
  bb_sup <- numeric(iterations)

  # Show progress bar if requested and verbosity allows
  verbosity <- getOption("artma.verbose", 3)
  show_pb <- show_progress && verbosity >= 3 && iterations >= 1000

  if (show_pb) {
    cli::cli_progress_bar(
      "Simulating CDFs for LCM test",
      total = iterations,
      format = "{cli::pb_spin} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )
  }

  for (m in seq_len(iterations)) {
    eps <- stats::rnorm(grid_points, mean = 0, sd = 1) / sqrt(grid_points)
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

    # Update progress every 50 iterations for better feedback
    if (show_pb && m %% 50 == 0) {
      cli::cli_progress_update(set = m)
    }
  }

  if (show_pb) {
    cli::cli_progress_done()
  }

  bb_sup
}

#' Binomial test for excess significant results
binomial_test <- function(P, p_min, p_max, type) {
  filtered <- switch(type,
    c = P[P <= p_max & P >= p_min],
    o = P[P < p_max & P > p_min],
    cli::cli_abort("Unknown test type.")
  )
  nn <- length(filtered)
  kk <- sum(filtered > (p_max + p_min) / 2)
  1 - stats::pbinom(kk - 1, nn, 0.5)
}

#' LCM-based test for shape restrictions
lcm_test <- function(P, p_min, p_max, norm, cdfs) {
  filtered <- P[P <= p_max & P >= p_min]
  nn <- length(filtered)
  f <- stats::ecdf(filtered)
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
  1 - stats::ecdf(cdfs)(bm_sup)
}

#' Fisher combination test adapted for truncated p-values
fisher_test <- function(P, p_min, p_max) {
  filtered <- P[P < p_max & P >= p_min]
  nn <- length(filtered)
  stat <- -2 * sum(log(1 - (filtered - p_min) / (p_max - p_min)))
  1 - stats::pchisq(stat, df = 2 * nn)
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
  stats::pnorm(stats::qnorm(1 - x1 / 2) - h) - stats::pnorm(stats::qnorm(1 - x2 / 2) - h) +
    stats::pnorm(stats::qnorm(1 - x1 / 2) + h) - stats::pnorm(stats::qnorm(1 - x2 / 2) + h)
}

#' Compute bounds for the p-curve and its derivatives
compute_bounds <- function(pmax, J, order) {
  h <- seq(0, 100, by = 0.001)
  grid <- linspace(0, pmax, J + 1)
  order <- as.integer(order)
  if (!order %in% 0:2) {
    cli::cli_abort("Unsupported order")
  }
  width <- c(J, J - 1, J - 2)[order + 1]
  bounds <- numeric(width)
  for (j in seq_len(width)) {
    lambda_left <- lambda2(grid[j], grid[j + 1], h)
    if (order == 0L) {
      bounds[j] <- max(lambda_left)
    } else if (order == 1L) {
      lambda_right <- lambda2(grid[j + 1], grid[j + 2], h)
      bounds[j] <- max(abs(lambda_right - lambda_left))
    } else {
      lambda_mid <- lambda2(grid[j + 2], grid[j + 3], h)
      bounds[j] <- max(abs(lambda_mid - 2 * lambda2(grid[j + 1], grid[j + 2], h) + lambda_left))
    }
  }
  bounds[1] <- 1
  matrix(bounds, ncol = 1)
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

build_constraint_vector <- function(B0, B1, B2, Galpha, use_bounds, J, K) { # nolint: object_name_linter.
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

fmincon <- function(x0, fn, gr = NULL, ..., method = "SQP",
                    A = NULL, b = NULL, Aeq = NULL, beq = NULL, # nolint: object_name_linter.
                    lb = NULL, ub = NULL, hin = NULL, heq = NULL,
                    tol = 1e-06, maxfeval = 10000, maxiter = 5000) {
  if (!is.numeric(x0) || length(x0) <= 1) {
    cli::cli_abort("'x0' must be a numeric vector of length greater 1.")
  }
  if (!is.null(gr)) {
    cli::cli_alert_warning("Gradient function is not used for SQP approach.")
  }

  if (!requireNamespace("NlcOptim", quietly = TRUE)) {
    cli::cli_abort("Package 'NlcOptim' missing -- install from CRAN.")
  }
  if (!requireNamespace("quadprog", quietly = TRUE)) {
    cli::cli_abort("Package 'quadprog' missing -- install from CRAN.")
  }

  fun <- match.fun(fn)
  fn <- function(x) fun(x, ...)

  if (!is.null(A)) {
    if (!is.matrix(A) || ncol(A) != length(x0)) {
      cli::cli_abort("Argument 'A' must be a matrix with length(x0) columns.")
    }
    if (is.null(b) || nrow(A) != length(b)) {
      cli::cli_abort("Argument 'b' must be a vector of length(b) = nrow(A).")
    }
  }
  if (!is.null(Aeq)) {
    if (!is.matrix(Aeq) || ncol(Aeq) != length(x0)) {
      cli::cli_abort("Argument 'Aeq' must be a matrix with length(x0) columns.")
    }
    if (is.null(beq) || nrow(Aeq) != length(beq)) {
      cli::cli_abort("Argument 'beq' must be a vector of length(beq) = nrow(Aeq).")
    }
  }
  if (!is.null(lb) && length(lb) != length(x0)) {
    if (length(lb == 1)) {
      lb <- rep(lb, length(x0))
    } else {
      cli::cli_abort("Length of argument 'lb' must be equal to length(x0).")
    }
  }
  if (!is.null(ub) && length(ub) != length(x0)) {
    if (length(ub == 1)) {
      ub <- rep(ub, length(x0))
    } else {
      cli::cli_abort("Length of argument 'ub' must be equal to length(x0).")
    }
  }

  if (is.null(hin) && is.null(heq)) {
    confun <- NULL
  } else if (is.null(heq)) {
    confun <- function(x) list(ceq = NULL, c = hin(x))
  } else if (is.null(hin)) {
    confun <- function(x) list(ceq = heq(x), c = NULL)
  } else {
    confun <- function(x) list(ceq = heq(x), c = hin(x))
  }

  sol <- NlcOptim::solnl(
    X = x0, objfun = fn, confun = confun,
    A = A, B = b, Aeq = Aeq, Beq = beq,
    lb = lb, ub = ub,
    tolX = tol, tolFun = 0.1 * tol, tolCon = 0.1 * tol
  )
  list(
    par = c(sol$par), value = sol$fn, convergence = 0,
    info = list(
      lambda = sol$lambda, grad = sol$grad,
      hessian = sol$hessian
    )
  )
}

solve_coxshi_problem <- function(t0, fn, A, b) {
  res <- tryCatch(fmincon(t0, fn, A = A, b = b), error = function(e) NULL)
  while (is.null(res) || !is.list(res)) {
    ru <- runif(length(t0))
    t0 <- matrix(ru / sum(ru), ncol = 1)
    res <- tryCatch(fmincon(t0, fn, A = A, b = b), error = function(e) NULL)
  }
  res
}

cox_shi_test <- function(Q, ind, p_min, p_max, J, K, use_bounds) {
  filtered <- filter_pvalues(Q, ind, p_min, p_max)
  P <- filtered$P
  ind_filtered <- filtered$ind
  N <- length(P)
  Galpha <- N / length(Q) # nolint: object_name_linter.
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
  ej <- rep(0, J)
  ej[J] <- 1
  F1 <- rbind(-diag(J - 1), rep(1, J - 1))
  constraint_vector <- if (identical(use_bounds, 0)) {
    rep(0, nrow(dk))
  } else {
    build_constraint_vector(B0, B1, B2, Galpha, use_bounds, J, K)
  }
  A <- dk %*% F1
  b_vec <- dk %*% ej - constraint_vector
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
  ba <- A[result$info$lambda$ineqlin > 0, , drop = FALSE]
  JX <- qr(ba)$rank
  if (result$convergence == 0 && JX > 0) {
    1 - stats::pchisq(stat, df = JX)
  } else {
    NA_real_
  }
}

box::export(
  format_decimal,
  linspace,
  simulate_cdfs,
  binomial_test,
  lcm_test,
  fisher_test,
  run_discontinuity_test,
  lambda2,
  compute_bounds,
  bound0,
  bound1,
  bound2,
  filter_pvalues,
  compute_phat,
  compute_omega,
  build_indicator_matrix,
  build_difference_matrix,
  extend_difference_matrix,
  build_constraint_vector,
  solve_coxshi_problem,
  cox_shi_test
)
