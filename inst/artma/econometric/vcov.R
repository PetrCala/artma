#' @title Robust variance-covariance helper
#' @description
#' Single implementation of the "clustered robust vcov, fall back to
#' non-clustered robust vcov, fall back to the plain model vcov" ladder that
#' several econometric call sites need. Both the `sandwich` (lm/ivreg) and the
#' `plm` (panel) engines are supported through the `engine` argument.
NULL

box::use(
  artma / libs / core / validation[validate]
)

#' @title Compute a robust variance-covariance matrix with fallbacks
#' @description
#' Runs an ordered ladder of vcov estimators and returns the first that
#' succeeds. The primary estimator is a clustered robust vcov when a usable
#' `cluster` is supplied, otherwise a non-clustered robust vcov of the same HC
#' `clustered_type`. It is followed by the non-clustered HC types listed in
#' `fallback_types`, and finally by `stats::vcov()` when `final_vcov_fallback`
#' is `TRUE`. Every step except the last runs under `tryCatch()` and is skipped
#' on error (or a `NULL` result); the last step runs uncaught so its error
#' propagates, matching the original call sites.
#'
#' The four historical call sites disagreed on the exact ladder, so each
#' difference is preserved through a parameter:
#'
#' * `get_robust_vcov` (exogeneity, ivreg): `engine = "sandwich"`,
#'   `clustered_type = "HC1"`, `fallback_types = c("HC1", "HC0")`,
#'   `require_cluster = TRUE`, `suppress_warnings = TRUE`. The intermediate
#'   non-clustered HC1 step and the warning suppression are unique to this site.
#' * `resolve_bpe_vcov` (best-practice estimate): `engine = "sandwich"`,
#'   `clustered_type = "HC0"`, `match_cluster_length = TRUE`, no HC
#'   `fallback_types`. The cluster is used only when its length equals the
#'   model's `nobs`; otherwise the primary step is the non-clustered HC0 vcov,
#'   and the only fallback is `stats::vcov()`.
#' * `tidy_lm_model` (linear lm specs): `engine = "sandwich"`,
#'   `clustered_type = "HC1"`, `fallback_types = "HC1"`,
#'   `final_vcov_fallback = FALSE`. No plain-vcov last resort: if the
#'   non-clustered HC1 step errors, that error propagates.
#' * `tidy_plm_generic` / `tidy_plm_within` (linear panel specs):
#'   `engine = "plm"`, `clustered_type = "HC1"`, `fallback_types = "HC0"`.
#'   `tidy_plm_generic` keeps the `stats::vcov()` last resort
#'   (`final_vcov_fallback = TRUE`); `tidy_plm_within` does not
#'   (`final_vcov_fallback = FALSE`). For `plm`, `cluster` is the panel
#'   dimension string (e.g. `"group"`) passed straight to `plm::vcovHC()`.
#'
#' @param model *[model object]* Fitted model (`lm`, `AER::ivreg`, or `plm`).
#' @param cluster *[vector or character, optional]* Clustering variable. For
#'   `engine = "sandwich"` a vector (e.g. `study_id`); for `engine = "plm"` the
#'   panel dimension string passed to `plm::vcovHC()` (e.g. `"group"`). `NULL`
#'   means no clustered step is attempted.
#' @param engine *[character]* `"sandwich"` (default) or `"plm"`.
#' @param clustered_type *[character]* HC type used for the primary step.
#'   Defaults to `"HC1"`.
#' @param fallback_types *[character]* Zero or more non-clustered HC types tried,
#'   in order, after the primary step. Defaults to none.
#' @param require_cluster *[logical]* When `TRUE`, `validate()` that both `model`
#'   and `cluster` are non-`NULL`. Defaults to `FALSE`.
#' @param match_cluster_length *[logical]* `sandwich` only. When `TRUE`, use the
#'   cluster only if `length(cluster) == stats::nobs(model)`, otherwise fall
#'   back to the non-clustered primary step. Defaults to `FALSE`.
#' @param final_vcov_fallback *[logical]* When `TRUE` (default), append
#'   `stats::vcov(model)` as the last resort.
#' @param suppress_warnings *[logical]* When `TRUE`, wrap the robust
#'   (`sandwich`/`plm`) steps in `suppressWarnings()`. The `stats::vcov()` last
#'   resort is never suppressed. Defaults to `FALSE`.
#' @return *[matrix]* A variance-covariance matrix.
robust_vcov <- function(model,
                        cluster = NULL,
                        engine = c("sandwich", "plm"),
                        clustered_type = "HC1",
                        fallback_types = character(),
                        require_cluster = FALSE,
                        match_cluster_length = FALSE,
                        final_vcov_fallback = TRUE,
                        suppress_warnings = FALSE) {
  engine <- match.arg(engine)

  if (require_cluster) {
    validate(!is.null(model), !is.null(cluster))
  }

  use_cluster <- if (is.null(cluster)) {
    FALSE
  } else if (match_cluster_length) {
    length(cluster) == stats::nobs(model)
  } else {
    TRUE
  }

  # A robust (sandwich/plm) vcov step, optionally clustered, wrapped in
  # suppressWarnings() when requested.
  robust_step <- function(type, clustered) {
    force(type)
    force(clustered)
    function() {
      value <- if (engine == "sandwich") {
        if (clustered) {
          sandwich::vcovCL(model, cluster = cluster, type = type)
        } else {
          sandwich::vcovHC(model, type = type)
        }
      } else {
        if (clustered) {
          plm::vcovHC(model, type = type, cluster = cluster)
        } else {
          plm::vcovHC(model, type = type)
        }
      }
      value
    }
  }

  steps <- list(robust_step(clustered_type, use_cluster))
  for (type in fallback_types) {
    steps[[length(steps) + 1L]] <- robust_step(type, FALSE)
  }

  run_robust <- function(step) {
    if (suppress_warnings) suppressWarnings(step()) else step()
  }

  # The final stats::vcov() step is never suppressed and runs uncaught.
  final_step <- if (final_vcov_fallback) function() stats::vcov(model) else NULL

  for (i in seq_along(steps)) {
    is_last <- is.null(final_step) && i == length(steps)
    if (is_last) {
      return(run_robust(steps[[i]]))
    }
    result <- tryCatch(run_robust(steps[[i]]), error = function(e) NULL)
    if (!is.null(result)) {
      return(result)
    }
  }

  final_step()
}

box::export(robust_vcov)
