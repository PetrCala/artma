box::use(
  artma / visualization / fork_safety[in_forked_worker]
)

#' Format a numeric value with a fixed number of decimals
format_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))

#' Linearly spaced sequence between two bounds
linspace <- function(start_, stop_, n) seq(from = start_, to = stop_, length.out = n)

simulate_cdfs_cpp <- function(iterations, grid_points) {
  .Call("_artma_simulate_cdfs_cpp", PACKAGE = "artma", iterations, grid_points)
}

simulate_cdfs_block_cpp <- function(eps_block) {
  .Call("_artma_simulate_cdfs_block_cpp", PACKAGE = "artma", eps_block)
}

#' Simulate Brownian bridge suprema CDFs used by the LCM test in parallel
#' @param iterations [integer] Number of simulations.
#' @param grid_points [integer] Number of grid points per simulation.
#' @param workers [integer] Number of workers to use.
#' @param block_size [integer] Size of the block to process.
#' @param show_progress [logical] Whether to show progress bar.
#' @param seed [integer, optional] RNG seed to set before simulating. Leave
#'   `NULL` to use (and consume) the caller's current RNG state.
#' @return Numeric vector of simulated suprema.
simulate_cdfs_parallel <- function(
  iterations = 10000,
  grid_points = 10000,
  workers = NULL,
  block_size = 256L,
  show_progress = TRUE,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }

  gp <- as.integer(grid_points)
  it <- as.integer(iterations)
  if (is.null(workers)) {
    chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
    workers <- if (chk %in% c("true", "warn")) {
      2L
    } else {
      max(1L, parallel::detectCores(logical = FALSE) - 1L)
    }
  }
  workers <- as.integer(max(1L, workers))
  block_size <- as.integer(max(1L, block_size))

  # This function forks its own workers via `mclapply()`/`makeCluster()`. When
  # it runs inside a worker already forked by the method-execution orchestrator,
  # forking again is unsafe, so stay sequential in that case.
  if (in_forked_worker()) {
    workers <- 1L
  }

  inv_sqrt_gp <- 1 / sqrt(gp)

  bb_sup <- numeric(it)

  verbosity <- getOption("artma.verbose", 3L)
  show_pb <- isTRUE(show_progress) && verbosity >= 3L && it >= 1000L

  if (show_pb) {
    cli::cli_inform("Pre-computing critical values for LCM test via Brownian bridge simulations")
    Sys.sleep(0.1)
    cli::cli_progress_bar(
      "Simulating {it} iterations",
      total = it,
      format = "{cli::pb_spin} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )
  }

  # The compiled C++ kernel is the single production implementation. It ships
  # with the package (LinkingTo Rcpp), so an unavailable kernel means a broken
  # install rather than a supported configuration; abort with a clear message.
  # The pure R equivalent survives only as a test-only numeric reference (see
  # tests/testthat/modules/testing/reference/elliott.R).
  cpp_available <- tryCatch(
    {
      simulate_cdfs_block_cpp(matrix(0, nrow = 1, ncol = 1))
      TRUE
    },
    error = function(e) FALSE
  )
  if (!cpp_available) {
    cli::cli_abort(c(
      "The compiled C++ implementation for CDF simulation is unavailable.",
      "i" = "artma ships this routine as compiled code. Reinstall the package so its C++ sources are built."
    ))
  }

  use_fork <- (.Platform$OS.type != "windows")
  if (!use_fork && workers > 1L) {
    workers <- 1L
  }

  done <- 0L
  while (done < it) {
    k <- min(block_size, it - done)
    start <- done + 1L
    end <- done + k

    eps_block <- matrix(stats::rnorm(gp * k), nrow = gp) * inv_sqrt_gp

    if (workers == 1L || k == 1L) {
      bb_sup[start:end] <- simulate_cdfs_block_cpp(eps_block)
    } else {
      cols_split <- split(seq_len(k), rep_len(seq_len(min(workers, k)), k))
      res_list <- parallel::mclapply(
        cols_split,
        function(cols) {
          list(
            idx = (start - 1L) + cols,
            val = simulate_cdfs_block_cpp(eps_block[, cols, drop = FALSE])
          )
        },
        mc.cores = min(workers, length(cols_split)),
        mc.preschedule = TRUE
      )
      for (r in res_list) bb_sup[r$idx] <- r$val
    }

    done <- end
    if (show_pb) cli::cli_progress_update(set = done)
  }

  if (show_pb) cli::cli_progress_done()
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
lcm_test <- function(P, p_min, p_max, cdfs) {
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
#' @param P [numeric] P-values to test.
#' @param c [numeric] Cutoff to test for a discontinuity.
#' @param h [numeric, optional] Manual bandwidth override. Leave `NULL`
#'   (the canonical default) to let rddensity select its bandwidth
#'   automatically.
run_discontinuity_test <- function(P, c, h = NULL) {
  res <- if (is.null(h)) {
    rddensity::rddensity(P, c = c)
  } else {
    rddensity::rddensity(P, c = c, h = h)
  }
  if (is.na(res$test$p_jk)) {
    return(skipped_result("rddensity returned no p-value (insufficient mass near the cutoff)"))
  }
  res$test$p_jk
}

#' Lambda function used in the Cox-Shi bounds
lambda2 <- function(x1, x2, h) {
  stats::pnorm(stats::qnorm(1 - x1 / 2) - h) - stats::pnorm(stats::qnorm(1 - x2 / 2) - h) +
    stats::pnorm(stats::qnorm(1 - x1 / 2) + h) - stats::pnorm(stats::qnorm(1 - x2 / 2) + h)
}

#' Adjacent-pair lambda vectors shared by all Cox-Shi bound orders
#'
#' Element `j` is `lambda2(grid[j], grid[j + 1], h)` on the common `h` grid.
#' Every bound order is an elementwise combination of these J vectors, so
#' they are computed once and reused instead of re-evaluated per order.
pair_lambdas <- function(p_min, p_max, J) {
  h <- seq(0, 100, by = 0.001)
  grid <- linspace(p_min, p_max, J + 1)
  lapply(seq_len(J), function(j) lambda2(grid[j], grid[j + 1], h))
}

#' Derive one order of Cox-Shi bounds from precomputed pair lambdas
bounds_from_lambdas <- function(lam, order, p_min) {
  order <- as.integer(order)
  if (!order %in% 0:2) {
    cli::cli_abort("Unsupported order")
  }
  n_pairs <- length(lam)
  width <- c(n_pairs, n_pairs - 1, n_pairs - 2)[order + 1]
  bounds <- numeric(width)
  for (j in seq_len(width)) {
    if (order == 0L) {
      bounds[j] <- max(lam[[j]])
    } else if (order == 1L) {
      bounds[j] <- max(abs(lam[[j + 1]] - lam[[j]]))
    } else {
      bounds[j] <- max(abs(lam[[j + 2]] - 2 * lam[[j + 1]] + lam[[j]]))
    }
  }
  if (p_min == 0) {
    bounds[1] <- 1
  }
  matrix(bounds, ncol = 1)
}

#' Compute bounds for the p-curve and its derivatives
compute_bounds <- function(p_min, p_max, J, order) {
  bounds_from_lambdas(pair_lambdas(p_min, p_max, J), order, p_min)
}

bound0 <- function(p_min, p_max, J) compute_bounds(p_min, p_max, J, 0)
bound1 <- function(p_min, p_max, J) compute_bounds(p_min, p_max, J, 1)
bound2 <- function(p_min, p_max, J) compute_bounds(p_min, p_max, J, 2)

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
  N <- length(P)
  J <- length(phat) + 1
  if (length(ind) > 1) {
    omega <- matrix(0, length(phat), length(phat))
    for (cluster in unique(ind)) {
      mq <- build_indicator_matrix(P[ind == cluster], bins, phat)
      # Outer product of the per-cluster sum of (indicator - phat). The canonical
      # implementation writes this as mq %*% ones(n_c, n_c) %*% t(mq) on the
      # transposed (bins x observations) layout.
      cluster_sum <- matrix(colSums(mq), ncol = 1)
      omega <- omega + cluster_sum %*% t(cluster_sum)
    }
    omega / N
  } else if (min(phat) == 0) {
    qhat <- matrix(phat * N / (N + 1) + 1 / (J * (N + 1)), ncol = 1)
    diag(c(qhat)) - qhat %*% t(qhat)
  } else {
    p <- matrix(phat, ncol = 1)
    diag(c(p)) - p %*% t(p)
  }
}

build_indicator_matrix <- function(X, bins, phat) {
  J <- length(phat) + 1
  mq <- matrix(0, nrow = length(X), ncol = J - 1)
  lower <- bins[1:(J - 1)]
  upper <- bins[2:J]
  for (q in seq_along(X)) {
    mq[q, ] <- as.numeric((X[q] > lower) & (X[q] <= upper))
    if (X[q] == 0) {
      mq[q, 1] <- 1
    }
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

#' Constraint vector for the Cox-Shi programme
#'
#' Mirrors the canonical construction: the bound block (or a vector of -1 when
#' bounds are switched off) followed by the zero block for the shape
#' restrictions. The zero block has `(K + 1) * (J - K / 2)` entries; without it
#' R recycles the bound block and slackens the shape constraints.
build_constraint_vector <- function(B0, B1, B2, bnd_adj, use_bounds, J, K, p_min = 0) { # nolint: object_name_linter.
  zeros <- rep(0, (K + 1) * (J - K / 2))
  if (use_bounds == 0) {
    return(c(rep(-1, J), zeros))
  }
  bounds <- list(-B0 / bnd_adj, -B1 / bnd_adj, -B2 / bnd_adj)[seq_len(K + 1)]
  if (p_min == 0) {
    bounds <- lapply(bounds, function(b) {
      b[1] <- -1
      b
    })
  }
  c(unlist(lapply(bounds, as.vector)), zeros)
}

#' Adapted from pracma::fmincon (GPL-3, compatible with this package's license)
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
    cli::cli_abort("Package {.pkg NlcOptim} is required for the SQP optimization step. Install with: install.packages('NlcOptim')")
  }
  if (!requireNamespace("quadprog", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg quadprog} is required for the SQP optimization step. Install with: install.packages('quadprog')")
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

#' Solve the Cox-Shi quadratic programme, retrying from random starts
#'
#' Returns `NULL` once the retry budget is exhausted so that callers can report
#' nonconvergence instead of looping forever on a deterministic failure.
solve_coxshi_problem <- function(t0, fn, A, b, max_restarts = 10L) { # nolint: object_name_linter.
  res <- tryCatch(fmincon(t0, fn, A = A, b = b), error = function(e) NULL)
  restarts <- 0L
  while ((is.null(res) || !is.list(res)) && restarts < max_restarts) {
    ru <- stats::runif(length(t0))
    t0 <- matrix(ru / sum(ru), ncol = 1)
    res <- tryCatch(fmincon(t0, fn, A = A, b = b), error = function(e) NULL)
    restarts <- restarts + 1L
  }
  if (is.null(res) || !is.list(res)) NULL else res
}

#' NA carrying a human-readable reason for skipping a test
skipped_result <- function(reason) {
  out <- NA_real_
  attr(out, "reason") <- reason
  out
}

cox_shi_test <- function(Q, ind, p_min, p_max, J, K, use_bounds) {
  use_bounds <- as.integer(use_bounds)
  K <- as.integer(K) # nolint: object_name_linter.
  J <- as.integer(J) # nolint: object_name_linter.
  filtered <- filter_pvalues(Q, ind, p_min, p_max)
  P <- filtered$P
  ind_filtered <- filtered$ind
  N <- length(P)
  bnd_adj <- N / length(Q)
  phat_data <- compute_phat(P, J, p_min, p_max)
  phat <- phat_data$phat
  bins <- phat_data$bins
  lam <- pair_lambdas(p_min, p_max, J)
  B0 <- bounds_from_lambdas(lam, 0L, p_min)
  B1 <- bounds_from_lambdas(lam, 1L, p_min)
  B2 <- bounds_from_lambdas(lam, 2L, p_min)
  omega <- compute_omega(P, ind_filtered, phat, bins)
  omega_inv <- if (is.finite(det(omega)) && abs(det(omega)) > 0) {
    tryCatch(solve(omega), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(omega_inv)) {
    return(skipped_result(paste0(
      "the bin covariance matrix is singular with J = ", J,
      " bins on [", p_min, ", ", p_max, "] (", N,
      " p-values in the window); try fewer bins"
    )))
  }
  D <- build_difference_matrix(J)
  dk <- extend_difference_matrix(D, J, K)
  dk <- if (use_bounds == 0) {
    rbind(-diag(J), diag(J), dk)
  } else {
    rbind(-diag(J), -dk, diag(J), dk)
  }
  ej <- rep(0, J)
  ej[J] <- 1
  F1 <- rbind(-diag(J - 1), rep(1, J - 1)) # nolint: object_name_linter.
  constraint_vector <- build_constraint_vector(B0, B1, B2, bnd_adj, use_bounds, J, K, p_min)
  A <- dk %*% F1 # nolint: object_name_linter.
  b_vec <- dk %*% ej - constraint_vector
  fn <- function(t) {
    diff_vec <- phat - t
    N * t(diff_vec) %*% omega_inv %*% diff_vec
  }
  start <- matrix(1 / J, ncol = 1, nrow = J - 1)
  result <- solve_coxshi_problem(start, fn, A, b_vec)
  if (is.null(result) || is.null(result$par) || result$convergence != 0) {
    return(skipped_result("the constrained optimisation did not converge"))
  }
  stat <- fn(result$par)
  ba <- A[result$info$lambda$ineqlin > 0, , drop = FALSE]
  JX <- qr(ba)$rank # nolint: object_name_linter.
  # JX == 0 is the interior (unconstrained) solution: no shape restriction
  # binds, so the p-curve is compatible with the null and p = 1.
  as.numeric(1 - stats::pchisq(stat, df = JX) * (JX > 0))
}

box::export(
  format_decimal,
  linspace,
  simulate_cdfs_block_cpp,
  simulate_cdfs_parallel,
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
  skipped_result,
  cox_shi_test
)
