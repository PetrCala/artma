# Test-only numeric reference for the Elliott LCM Brownian bridge simulation.
#
# Production code (inst/artma/calc/methods/elliott.R) runs the compiled C++
# kernel exclusively. This pure R implementation is kept solely as an
# independent oracle: tests draw the same RNG stream, run both, and assert the
# per-draw suprema agree. It uses fdrtool::gcmlcm for the greatest convex
# minorant / least concave majorant, matching the algorithm the C++ kernel
# reimplements.
#
# The simulation is serial here on purpose: the reference exists to be obviously
# correct, not fast. A single rnorm() draw of gp * iterations values yields the
# identical Mersenne-Twister stream that the production block loop consumes, so
# a shared seed lines the two implementations up exactly.

#' Serial pure R reference for `simulate_cdfs_parallel()`.
#'
#' @param iterations *\[integer\]* Number of Brownian bridge simulations.
#' @param grid_points *\[integer\]* Number of grid points per simulation.
#' @param seed *\[integer, optional\]* RNG seed to set before drawing. Leave
#'   `NULL` to consume the caller's current RNG state, matching the production
#'   contract.
#' @return *\[numeric\]* Vector of simulated suprema, one per iteration.
simulate_cdfs_reference <- function(iterations, grid_points, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }

  gp <- as.integer(grid_points)
  it <- as.integer(iterations)

  inv_gp <- 1 / gp
  inv_sqrt_gp <- 1 / sqrt(gp)

  c_grid <- seq_len(gp) * inv_gp
  c_values <- c(0, c_grid)
  n_values <- gp + 1L

  eps <- matrix(stats::rnorm(gp * it), nrow = gp) * inv_sqrt_gp

  bb_sup <- numeric(it)
  b_values <- numeric(n_values)
  y <- numeric(n_values)

  for (jj in seq_len(it)) {
    w <- cumsum(eps[, jj])
    w_end <- w[gp]

    b_values[1L] <- 0
    b_values[2L:n_values] <- w - c_grid * w_end

    hull <- fdrtool::gcmlcm(c_values, b_values, type = "lcm")

    y[] <- 0
    y[1L] <- 0

    hkx <- hull$x.knots
    hky <- hull$y.knots
    hks <- hull$slope.knots

    # The x-knots are grid points j / gp, but the product hkx * gp can land
    # just below the integer j (e.g. 86.999...); indexing with it truncates
    # and writes the chord one grid slot too low. Round to recover j exactly.
    ki <- as.integer(round(hkx * gp))

    for (s in 2:length(ki)) {
      a <- hky[s] - hks[s - 1L] * hkx[s]
      b_slope <- hks[s - 1L]
      idx <- (ki[s - 1L] + 1L):ki[s]
      y[idx] <- a + b_slope * (idx * inv_gp)
    }

    bb_sup[jj] <- max(abs(y - b_values))
  }

  bb_sup
}

box::export(simulate_cdfs_reference)
