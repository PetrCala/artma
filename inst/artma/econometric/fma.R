#' @title Frequentist model averaging
#' @description
#' Estimate frequentist model averaging weights and coefficients based on a BMA
#' ordering of predictors. Uses quadratic programming to obtain non-negative
#' weights that sum to one.
NULL

solve_fma_weights <- function(G, a) {
  if (!requireNamespace("quadprog", quietly = TRUE)) {
    cli::cli_abort("Package 'quadprog' missing -- install from CRAN.")
  }

  M <- ncol(G)
  amat <- cbind(rep(1, M), diag(M))
  bvec <- c(1, rep(0, M))
  attempts <- c(0, 1e-08, 1e-06, 1e-04)

  solution <- NULL
  for (jitter in attempts) {
    dmat <- G + diag(jitter, M)
    res <- tryCatch(
      quadprog::solve.QP(Dmat = dmat, dvec = a, Amat = amat, bvec = bvec, meq = 1),
      error = function(e) NULL
    )
    if (!is.null(res) && !is.null(res$solution)) {
      solution <- res$solution
      break
    }
  }

  if (is.null(solution)) {
    cli::cli_alert_warning("FMA weight optimization failed. Falling back to uniform weights.")
    solution <- rep(1 / M, M)
  }

  solution[solution < 0 & solution > -1e-08] <- 0
  if (abs(sum(solution) - 1) > 1e-08) {
    solution <- solution / sum(solution)
  }

  as.numeric(solution)
}

resolve_fma_predictors <- function(bma_data, bma_model) {
  predictors <- setdiff(colnames(bma_data), "effect")
  if (!length(predictors)) {
    return(predictors)
  }

  coef_mat <- tryCatch(
    stats::coef(bma_model, order.by.pip = TRUE, exact = TRUE, include.constant = TRUE),
    error = function(e) NULL
  )

  if (!is.null(coef_mat) && !is.null(rownames(coef_mat))) {
    ordered <- rownames(coef_mat)
    ordered <- ordered[!ordered %in% c("(Intercept)", "Intercept", "effect")]
    ordered <- ordered[ordered %in% predictors]
    if (length(ordered)) {
      return(c(ordered, setdiff(predictors, ordered)))
    }
  }

  predictors
}

#' @title Move averaged FMA coefficients back onto the data scale
#' @description
#' `run_fma()` is fitted on the frame produced by `get_bma_data()`, which
#' z-scores every non-binary column, the effect included. The averaged
#' coefficients and standard errors therefore come out in y-SD-per-x-SD units
#' (y-SD units for the unscaled dummies). This function inverts the transform
#' using the centers and scales stored on the frame, mirroring
#' `unscale_bma_coefs()`:
#'
#' - slopes and their SEs: multiplied by `sd(y) / sd(x_j)` (`sd(x_j) = 1` for
#'   columns that were never scaled);
#' - the intercept: rescaled by `sd(y)`, shifted by `mean(y)`, and corrected
#'   for the centering of every scaled regressor; its SE is rescaled by `sd(y)`.
#'
#' The inputs are returned unchanged when the frame carries no scaling
#' attributes.
#'
#' @param coefficient *\[numeric\]* Averaged coefficients, in `var_names` order.
#' @param se *\[numeric\]* Their standard errors, in `var_names` order.
#' @param var_names *\[character\]* The raw column names behind each row, with
#'   the intercept named `(Intercept)` or `Intercept`.
#' @param bma_data *\[data.frame\]* The frame the models were fitted on,
#'   carrying the `bpe_scale_centers` / `bpe_scale_scales` attributes set by
#'   `get_bma_data()` (identity when absent).
#' @return *\[list\]* `list(coefficient=, se=)` on the data scale.
#' @export
unscale_fma_coefs <- function(coefficient, se, var_names, bma_data) {
  box::use(
    artma / libs / core / validation[validate]
  )

  validate(
    is.numeric(coefficient),
    is.numeric(se),
    is.character(var_names),
    length(coefficient) == length(var_names),
    length(se) == length(var_names),
    is.data.frame(bma_data)
  )

  centers <- attr(bma_data, "bpe_scale_centers")
  scales <- attr(bma_data, "bpe_scale_scales")
  if (is.null(centers) || is.null(scales)) {
    return(list(coefficient = coefficient, se = se))
  }

  center_of <- function(name) if (name %in% names(centers)) centers[[name]] else 0
  scale_of <- function(name) if (name %in% names(scales)) scales[[name]] else 1

  is_intercept <- var_names %in% c("(Intercept)", "Intercept")
  slope_rows <- which(!is_intercept)
  slope_names <- var_names[slope_rows]

  y_center <- center_of("effect")
  y_scale <- scale_of("effect")

  factors <- vapply(slope_names, function(name) y_scale / scale_of(name), numeric(1))
  coefficient[slope_rows] <- coefficient[slope_rows] * factors
  se[slope_rows] <- se[slope_rows] * factors

  if (any(is_intercept)) {
    x_centers <- vapply(slope_names, center_of, numeric(1))
    coefficient[is_intercept] <- y_center +
      y_scale * coefficient[is_intercept] -
      sum(coefficient[slope_rows] * x_centers)
    se[is_intercept] <- y_scale * se[is_intercept]
  }

  list(coefficient = unname(coefficient), se = unname(se))
}

#' @title Run frequentist model averaging
#' @param bma_data *\[data.frame\]* Data used for BMA estimation (effect in first column).
#' @param bma_model *\[bma\]* BMA model used to order predictors.
#' @param input_var_list *\[data.frame\]* Variable metadata with verbose names.
#' @param cluster_ids *\[vector, optional\]* Cluster membership (e.g. study IDs)
#'   for each row of `bma_data`. When supplied, coefficient standard errors are
#'   cluster-robust (CR1); when `NULL`, classical iid standard errors are used.
#' @param round_to *\[integer, optional\]* Digits for printed output; NULL uses global default.
#' @param print_results *\[character\]* One of "none", "fast", "verbose", or "all".
#' @return *\[list\]* List with `coefficients` and `weights`.
#' @export
run_fma <- function(bma_data, bma_model, input_var_list, cluster_ids = NULL, round_to = NULL, print_results = "none") {
  box::use(
    artma / libs / core / validation[assert, validate]
  )

  validate(
    is.data.frame(bma_data),
    is.data.frame(input_var_list),
    inherits(bma_model, "bma"),
    all(vapply(bma_data, is.numeric, logical(1))),
    colnames(bma_data)[1] == "effect",
    print_results %in% c("none", "fast", "verbose", "all")
  )

  cluster_factor <- NULL
  if (!is.null(cluster_ids)) {
    validate(
      is.atomic(cluster_ids),
      length(cluster_ids) == nrow(bma_data),
      !any(is.na(cluster_ids))
    )
    assert(
      length(unique(cluster_ids)) > 1L,
      "Clustered FMA requires at least two distinct clusters."
    )
    cluster_factor <- factor(cluster_ids)
  }

  assert(ncol(bma_data) >= 2, "FMA requires at least one predictor variable.")

  predictors <- resolve_fma_predictors(bma_data, bma_model)
  x_data <- as.data.frame(bma_data[, predictors, drop = FALSE])
  x_data <- cbind(`(Intercept)` = rep(1, nrow(x_data)), x_data)

  scale_vector <- vapply(x_data, function(col) {
    val <- max(abs(col), na.rm = TRUE)
    if (!is.finite(val) || val == 0) {
      return(1)
    }
    val
  }, numeric(1))

  x <- sweep(as.matrix(x_data), 2, scale_vector, "/")
  Y <- as.matrix(bma_data[["effect"]])

  n <- nrow(x)
  M <- ncol(x)
  k <- M

  assert(n > M, "FMA requires more observations than predictors.")

  full_fit <- stats::lm.fit(x = x, y = Y)
  beta.full <- as.matrix(stats::coef(full_fit))

  beta <- matrix(0, nrow = k, ncol = M)
  e <- matrix(0, nrow = n, ncol = M)
  k_vector <- matrix(seq_len(M), ncol = 1)
  var.matrix <- matrix(0, nrow = k, ncol = M)
  tol <- sqrt(.Machine$double.eps)

  for (i in seq_len(M)) {
    X <- as.matrix(x[, 1:i, drop = FALSE])
    ortho <- eigen(crossprod(X), symmetric = TRUE)
    Q <- ortho$vectors
    lambda <- ortho$values

    if (any(lambda < -tol)) {
      cli::cli_abort("FMA failed: design matrix not positive semidefinite at step {i}.")
    }
    lambda_adj <- pmax(lambda, tol)

    lambda_half <- diag(lambda_adj^-0.5, i, i)
    x_tilda <- X %*% Q %*% lambda_half
    beta.star <- t(x_tilda) %*% Y
    beta.hat <- Q %*% lambda_half %*% beta.star
    beta[1:i, i] <- beta.hat

    e_i <- Y - x_tilda %*% beta.star
    e[, i] <- e_i

    # Eigen-floored (X'X)^-1: both variance paths share it so collinear
    # designs degrade identically instead of erroring in solve().
    xtx_inv <- Q %*% diag(lambda_adj^-1, i, i) %*% t(Q)
    if (is.null(cluster_factor)) {
      sigma_i <- as.numeric(crossprod(e_i) / (n - i))
      var.matrix[1:i, i] <- sigma_i * diag(xtx_inv)
    } else {
      # CR1 cluster-robust vcov with the Stata small-sample correction,
      # numerically identical to sandwich::vcovCL(type = "HC1").
      scores <- rowsum(X * as.numeric(e_i), cluster_factor)
      n_clusters <- nlevels(cluster_factor)
      dfc <- (n_clusters / (n_clusters - 1)) * ((n - 1) / (n - i))
      cl_vcov <- dfc * xtx_inv %*% crossprod(scores) %*% xtx_inv
      var.matrix[1:i, i] <- diag(cl_vcov)
    }
  }

  e_k <- e[, M, drop = FALSE]
  sigma_hat <- as.numeric(crossprod(e_k) / (n - M))
  G <- crossprod(e)
  G <- (G + t(G)) / 2
  # Hansen (2007) Mallows criterion: minimize w'Gw + 2*sigma^2*k'w. solve.QP
  # minimizes (1/2)b'Db - d'b, so with Dmat = G the penalty enters as -sigma^2*k.
  a <- as.numeric(-sigma_hat * k_vector)

  weights <- solve_fma_weights(G, a)

  beta_scaled <- beta %*% weights
  final_beta <- as.numeric(beta_scaled / scale_vector)
  # Buckland et al. (1997): spread term is squared deviation from the averaged
  # estimate, folded into each model's variance before the weighted sqrt sum.
  bias.sq <- (beta - as.numeric(beta_scaled))^2
  std_scaled <- sqrt(var.matrix + bias.sq) %*% weights
  final_std <- as.numeric(std_scaled / scale_vector)

  t_stats <- final_beta / final_std
  p_values <- stats::pnorm(abs(t_stats), lower.tail = FALSE) * 2
  p_values[!is.finite(p_values)] <- NA_real_

  var_names <- colnames(x_data)

  # The frame handed in is normally z-scored (`get_bma_data(scale_data = TRUE)`),
  # so the fit above is in y-SD-per-x-SD units; report in the units of the
  # input columns instead. The p-values are computed before the back-transform:
  # a slope and its SE pick up the same factor, so the slope tests are
  # unchanged, and the intercept keeps the test of the fitted (centered)
  # parameterization, matching the BMA convention where the intercept SD is a
  # plain rescale carrying no correction for the centering shift.
  unscaled <- unscale_fma_coefs(final_beta, final_std, var_names, bma_data)
  final_beta <- unscaled$coefficient
  final_std <- unscaled$se

  display_names <- var_names
  intercept_mask <- var_names %in% c("(Intercept)", "Intercept")
  display_names[intercept_mask] <- "Intercept"
  idx <- match(var_names, input_var_list$var_name)
  display_names[!is.na(idx)] <- input_var_list$var_name_verbose[stats::na.omit(idx)]

  coefficients <- data.frame(
    variable = display_names,
    coefficient = final_beta,
    se = final_std,
    p_value = p_values,
    stringsAsFactors = FALSE
  )

  if (print_results != "none") {
    digits <- round_to
    if (is.null(digits) || is.na(digits)) {
      digits <- as.integer(getOption("artma.output.number_of_decimals", 3))
    }
    digits <- as.integer(digits)

    printable <- coefficients
    printable$coefficient <- round(printable$coefficient, digits)
    printable$se <- round(printable$se, digits)
    printable$p_value <- round(printable$p_value, digits)

    if (print_results == "fast") {
      cli::cat_print(printable[c("variable", "coefficient", "se")])
    } else if (print_results %in% c("verbose", "all")) {
      cli::cat_print(printable[c("variable", "coefficient", "se", "p_value")])
    }

    if (print_results %in% c("verbose", "all")) {
      weights_df <- data.frame(
        model = seq_along(weights),
        weight = round(weights, digits),
        stringsAsFactors = FALSE
      )
      cli::cli_h3("FMA model weights")
      weight_lines <- utils::capture.output(print(weights_df, row.names = FALSE)) # nolint: undesirable_function_linter.
      cli::cli_verbatim(weight_lines)
    }
  }

  list(
    coefficients = coefficients,
    weights = weights
  )
}

box::export(
  run_fma,
  unscale_fma_coefs
)
