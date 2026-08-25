box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_gte,
    expect_length,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(
  artma / data / column_recognition[
    check_mapping_plausibility,
    format_declined_evidence,
    recognize_columns
  ]
)

# The confirm-me layer: what recognition reports about the roles it declined to
# accept on its own. None of it changes the mapping; it only gives an
# interactive caller (and external tooling) the evidence to ask about.

#' A dataset whose effect column carries no name signal at all: the real
#' failure family behind the missed cells on the benchmark (eis, habit,
#' excess_sensitivity), surrounded by identifier-flavored decoys.
make_nameless_effect_df <- function(n_studies = 25, k = 6, effect_col = "eis") {
  study <- rep(seq_len(n_studies), each = k)
  n <- length(study)
  effect <- round(stats::rnorm(n, 0.2, 0.5), 3)
  se <- pmax(round(exp(stats::rnorm(n, log(0.15), 0.5)), 3), 0.001)

  df <- data.frame(
    idstudy = study,
    study = rep(sprintf("Author%02d", seq_len(n_studies)), each = k),
    idcoeff = rep(seq_len(k), times = n_studies),
    placeholder = effect,
    se = se,
    nobs = rep(sample(80:2000, n_studies, replace = TRUE), each = k),
    ols = sample(0:1, n, replace = TRUE)
  )
  names(df)[names(df) == "placeholder"] <- effect_col
  df
}


test_that("a name-free effect column is offered as a provisional candidate", {
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  df <- make_nameless_effect_df()

  mapping <- recognize_columns(df)

  # Auto-detection is unchanged: the nameless column is still not accepted.
  expect_null(mapping$effect)
  expect_equal(mapping$se, "se")

  provisional <- attr(mapping, "provisional")
  expect_equal(names(provisional), "effect")

  candidate <- provisional$effect
  expect_equal(candidate$kind, "unmapped")
  expect_equal(candidate$column, "eis")
  # The three conditions that make it worth a question: strong value evidence,
  # consistency with the accepted counterpart, a clear lead over the runner-up.
  expect_gte(candidate$evidence, 0.55)
  expect_equal(candidate$pair_with, "se")
  expect_gte(candidate$pair_consistency, 0.5)
  expect_gte(candidate$margin, 0.1)
  # No name signal is exactly why it was declined in the first place.
  expect_equal(candidate$name_score, 0)
  expect_equal(candidate$summary$coverage, 1)
})


test_that("an identifier-flavored candidate is never offered for confirmation", {
  withr::local_seed(3)
  withr::local_options(list("artma.verbose" = 1))
  df <- make_nameless_effect_df()
  df$eis <- NULL # no effect column at all; only decoys remain

  mapping <- recognize_columns(df)

  expect_null(mapping$effect)
  expect_null(attr(mapping, "provisional")$effect)
})


test_that("a confidently recognized dataset produces no provisional candidates", {
  withr::local_seed(5)
  withr::local_options(list("artma.verbose" = 1))
  n_studies <- 20
  k <- 6
  n <- n_studies * k
  effect <- round(stats::rnorm(n, 0.3, 0.4), 3)

  df <- data.frame(
    study = rep(sprintf("Author%02d", seq_len(n_studies)), each = k),
    effect = effect,
    se = pmax(round(exp(stats::rnorm(n, log(0.12), 0.4)), 3), 0.001),
    n_obs = rep(sample(50:900, n_studies, replace = TRUE), each = k)
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "effect")
  expect_length(attr(mapping, "provisional"), 0)
  expect_null(attr(mapping, "declined")$effect)
})


test_that("two near-tied effect candidates are recorded instead of resolved silently", {
  withr::local_seed(7)
  withr::local_options(list("artma.verbose" = 1))
  n_studies <- 20
  k <- 8
  n <- n_studies * k
  effect <- round(stats::rnorm(n, 0.3, 0.4), 3)

  df <- data.frame(
    study = rep(sprintf("Author%02d", seq_len(n_studies)), each = k),
    effect = effect,
    effect_M = round(effect * 1.02 + stats::rnorm(n, 0, 0.01), 3),
    se = pmax(round(exp(stats::rnorm(n, log(0.12), 0.4)), 3), 0.001),
    n_obs = rep(sample(50:900, n_studies, replace = TRUE), each = k)
  )

  mapping <- recognize_columns(df)

  # The automatic pick stands: only an interactive session asks about the twin.
  expect_equal(mapping$effect, "effect")

  tie <- attr(mapping, "provisional")$effect
  expect_equal(tie$kind, "tie")
  expect_equal(tie$column, "effect")
  expect_true("effect_M" %in% tie$alternatives)
})


test_that("candidates too close to call are all offered, not dropped", {
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  df <- make_nameless_effect_df()
  # A winsorized twin of the effect column: the pair of them is exactly what
  # the recognizer cannot separate on the evidence alone.
  df$eis_w <- round(pmax(pmin(df$eis, 1), -1), 3)

  mapping <- recognize_columns(df)
  candidate <- attr(mapping, "provisional")$effect

  expect_null(mapping$effect)
  expect_true("eis_w" %in% c(candidate$column, candidate$alternatives))
  expect_true("eis" %in% c(candidate$column, candidate$alternatives))
  expect_length(candidate$alternative_summaries, length(candidate$alternatives))
})


test_that("declined required roles carry machine-readable evidence", {
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  df <- make_nameless_effect_df()

  declined <- attr(recognize_columns(df), "declined")

  expect_true("effect" %in% names(declined))
  entry <- declined$effect
  expect_equal(entry$role, "effect")
  expect_true(nzchar(entry$reason))
  expect_true(length(entry$candidates) > 0)

  ranked <- vapply(entry$candidates, function(cand) cand$column, character(1))
  expect_equal(ranked[1], "eis")
  # Every candidate carries the numbers behind the decision, not just a name.
  for (cand in entry$candidates) {
    expect_true(all(c("score", "name_score", "evidence") %in% names(cand)))
  }

  lines <- format_declined_evidence(declined)
  expect_length(lines, length(declined))
  expect_true(any(grepl("^effect: ", lines)))
  expect_true(any(grepl("eis", lines, fixed = TRUE)))
})


test_that("a required role with no candidate at all still reports why", {
  withr::local_seed(2)
  withr::local_options(list("artma.verbose" = 1))
  df <- make_nameless_effect_df()
  df$nobs <- NULL

  declined <- attr(recognize_columns(df), "declined")

  expect_true("n_obs" %in% names(declined))
  expect_length(declined$n_obs$candidates, 0)
  expect_true(nzchar(declined$n_obs$reason))
})


test_that("check_mapping_plausibility flags contradictions and passes real columns", {
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  df <- make_nameless_effect_df()

  good <- check_mapping_plausibility(df, "effect", "eis")
  expect_true(good$ok)

  bad <- check_mapping_plausibility(df, "effect", "idcoeff")
  expect_false(bad$ok)
  expect_true(nzchar(bad$reason))

  # The check is a report, not a gate: it stays silent where it cannot judge.
  expect_true(check_mapping_plausibility(df, "effect", "not_a_column")$ok)
  expect_true(check_mapping_plausibility(df, "study_id", "study")$ok)
})
