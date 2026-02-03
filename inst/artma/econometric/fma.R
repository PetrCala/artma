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
  Amat <- cbind(rep(1, M), diag(M))
  bvec <- c(1, rep(0, M))
  attempts <- c(0, 1e-08, 1e-06, 1e-04)

  solution <- NULL
  for (jitter in attempts) {
    Dmat <- G + diag(jitter, M)
    res <- tryCatch(
      quadprog::solve.QP(Dmat = Dmat, dvec = a, Amat = Amat, bvec = bvec, meq = 1),
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

#' @title Run frequentist model averaging
#' @param bma_data *\[data.frame\]* Data used for BMA estimation (effect in first column).
#' @param bma_model *\[bma\]* BMA model used to order predictors.
#' @param input_var_list *\[data.frame\]* Variable metadata with verbose names.
#' @param round_to *\[integer, optional\]* Digits for printed output; NULL uses global default.
#' @param print_results *\[character\]* One of "none", "fast", "verbose", "all", or "table".
#' @return *\[list\]* List with `coefficients` and `weights`.
#' @export
run_fma <- function(bma_data, bma_model, input_var_list, round_to = NULL, print_results = "none") {
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
  K_vector <- matrix(seq_len(M), ncol = 1)
  var.matrix <- matrix(0, nrow = k, ncol = M)
  bias.sq <- matrix(0, nrow = k, ncol = M)
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
    bias.sq[, i] <- (beta[, i] - beta.full)^2

    sigma_i <- as.numeric(crossprod(e_i) / (n - i))
    var.matrix.star <- diag(sigma_i, i, i)
    var.matrix.hat <- var.matrix.star %*% (Q %*% diag(lambda_adj^-1, i, i) %*% t(Q))
    var.matrix[1:i, i] <- diag(var.matrix.hat)
    var.matrix[, i] <- var.matrix[, i] + bias.sq[, i]
  }

  e_k <- e[, M, drop = FALSE]
  sigma_hat <- as.numeric(crossprod(e_k) / (n - M))
  G <- crossprod(e)
  G <- (G + t(G)) / 2
  a <- as.numeric((sigma_hat^2) * K_vector)

  weights <- solve_fma_weights(G, a)

  beta_scaled <- beta %*% weights
  final_beta <- as.numeric(beta_scaled / scale_vector)
  std_scaled <- sqrt(var.matrix) %*% weights
  final_std <- as.numeric(std_scaled / scale_vector)

  t_stats <- final_beta / final_std
  p_values <- stats::pnorm(abs(t_stats), lower.tail = FALSE) * 2
  p_values[!is.finite(p_values)] <- NA_real_

  var_names <- colnames(x_data)
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
  run_fma
)
