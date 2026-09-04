box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_named,
    expect_true,
    test_that
  ]
)

# These tests exercise the pure detection layer (schema_detect.R) directly, with
# no prompt mocking and no persistence. detect_schema_drift is covered against
# the store in test-data-schema-reconcile.R; here we pin propose_renames and
# confirm the detection entry points are importable from the pure module.

base_store <- function(extra = list()) {
  utils::modifyList(
    list(
      effect = list(source_name = "effect_size"),
      se = list(source_name = "se_col"),
      study_id = list(source_name = "study")
    ),
    extra
  )
}

base_df <- function(...) {
  data.frame(effect_size = 1:3, se_col = 0.1, study = "A", n_obs = 10L, ...)
}

# detect_schema_drift (pure module import)

test_that("detect_schema_drift is importable from schema_detect and reports no drift", {
  box::use(artma / data / schema_detect[detect_schema_drift])

  result <- detect_schema_drift(base_df(), base_store())

  expect_false(result$has_drift)
})

# propose_renames

test_that("propose_renames returns an empty named list when nothing is missing", {
  box::use(artma / data / schema_detect[propose_renames])

  result <- propose_renames(character(0), c("a", "b"))

  expect_equal(length(result), 0L)
  expect_true(is.list(result))
})

test_that("propose_renames reports every record, with no candidate, when no columns are available", {
  box::use(artma / data / schema_detect[propose_renames])

  result <- propose_renames(c(pub = "publication_year"), character(0))

  expect_named(result, "pub")
  expect_true(is.na(result$pub$candidate))
  expect_false(result$pub$ambiguous)
})

test_that("propose_renames suggests a close string match", {
  box::use(artma / data / schema_detect[propose_renames])

  missing <- c(pub = "publication_year")
  result <- propose_renames(missing, c("pub_year", "region"))

  expect_named(result, "pub")
  expect_identical(result$pub$candidate, "pub_year")
  expect_true(result$pub$score > 0)
})

test_that("propose_renames returns NA candidate when no match clears the threshold", {
  box::use(artma / data / schema_detect[propose_renames])

  missing <- c(effect = "effect_size")
  result <- propose_renames(missing, c("xyz_qq"))

  expect_named(result, "effect")
  expect_true(is.na(result$effect$candidate))
})

test_that("propose_renames uses the pattern signal when roles are known", {
  box::use(artma / data / schema_detect[propose_renames])

  # "beta" is nothing like "effect_size" as a string, but the recognition
  # patterns identify it as an effect column once the role is known.
  df <- data.frame(beta = c(-0.4, 0.1, 0.8, -1.2, 0.3, 2.1, -0.7, 1.5, 0.0, -0.9))

  without_role <- propose_renames(
    c(effect = "effect_size"), "beta",
    raw_df = df, roles_known = FALSE
  )
  with_role <- propose_renames(
    c(effect = "effect_size"), "beta",
    raw_df = df, roles_known = TRUE
  )

  expect_true(with_role$effect$score >= without_role$effect$score)
  expect_identical(with_role$effect$candidate, "beta")
})

# Exclusive assignment and ties

test_that("propose_renames gives each candidate to at most one record", {
  box::use(artma / data / schema_detect[propose_renames])

  # Both removed moderators resemble the one new column; the closer name
  # (x_10, one character off) takes it and x_1 is left without a candidate
  # instead of both being remapped onto x_100.
  result <- propose_renames(c(x_1 = "x_1", x_10 = "x_10"), "x_100")

  expect_identical(result$x_10$candidate, "x_100")
  expect_true(is.na(result$x_1$candidate))
})

test_that("propose_renames keeps the order of the records it was given", {
  box::use(artma / data / schema_detect[propose_renames])

  result <- propose_renames(c(x_1 = "x_1", x_10 = "x_10"), "x_100")

  expect_named(result, c("x_1", "x_10"))
})

test_that("propose_renames flags a proposal as ambiguous when two candidates tie", {
  box::use(artma / data / schema_detect[propose_renames])

  result <- propose_renames(c(effect = "effect_size"), c("effect_size_b", "effect_size_a"))

  expect_false(is.na(result$effect$candidate))
  expect_true(result$effect$ambiguous)
  expect_false(result$effect$contested)
  expect_true(result$effect$runner_up %in% c("effect_size_a", "effect_size_b"))
  expect_false(identical(result$effect$runner_up, result$effect$candidate))
})

test_that("propose_renames flags a proposal as contested when two records tie for it", {
  box::use(artma / data / schema_detect[propose_renames])

  # score_a and score_b are equally close to score_c
  result <- propose_renames(c(a = "score_a", b = "score_b"), "score_c")

  winners <- Filter(function(prop) !is.na(prop$candidate), result)
  expect_equal(length(winners), 1L)
  expect_true(winners[[1]]$ambiguous)
  expect_true(winners[[1]]$contested)
})

test_that("propose_renames leaves a clear winner unflagged", {
  box::use(artma / data / schema_detect[propose_renames])

  result <- propose_renames(c(effect = "effect_size"), c("effect_sise", "region"))

  expect_identical(result$effect$candidate, "effect_sise")
  expect_false(result$effect$ambiguous)
  expect_true(is.na(result$effect$runner_up))
})

# Rename similarity

test_that("rename_similarity suggests but does not auto-accept a name contained in another", {
  box::use(artma / data / column_recognition[MATCH_THRESHOLDS, rename_similarity])

  score <- rename_similarity("gdp", "gdp_growth")

  expect_true(score >= MATCH_THRESHOLDS$rename_suggest)
  expect_true(score < MATCH_THRESHOLDS$rename_auto)
})

test_that("rename_similarity gives very short names no substring credit", {
  box::use(artma / data / column_recognition[MATCH_THRESHOLDS, rename_similarity])

  expect_true(rename_similarity("n", "region") < MATCH_THRESHOLDS$rename_suggest)
  expect_true(rename_similarity("id", "study_id") < MATCH_THRESHOLDS$rename_suggest)
})

test_that("rename_similarity still auto-accepts a typo and an exact match", {
  box::use(artma / data / column_recognition[MATCH_THRESHOLDS, rename_similarity])

  expect_equal(rename_similarity("effect_size", "Effect_Size"), 1)
  expect_true(rename_similarity("effect_size", "effect_sise") >= MATCH_THRESHOLDS$rename_auto)
})

test_that("rename_similarity leaves the keyword scoring rule untouched", {
  box::use(artma / data / column_recognition[string_similarity])

  # Recognition depends on the flat substring score for keywords inside a
  # column name; only the rename path changed.
  expect_equal(string_similarity("effect_size", "effect"), 0.8)
})

# Optional roles

test_that("detect_schema_drift files a vanished optional mapping separately from required roles", {
  box::use(artma / data / schema_detect[detect_schema_drift])

  store <- base_store(list(t_stat = list(source_name = "tstat")))

  result <- detect_schema_drift(base_df(), store)

  expect_true(result$has_drift)
  expect_equal(result$missing_optional_roles[["t_stat"]], "tstat")
  expect_false("t_stat" %in% names(result$missing_roles))
})

test_that("detect_schema_drift treats a mapped role outside the run's required set as optional", {
  box::use(artma / data / schema_detect[detect_schema_drift])

  store <- base_store(list(n_obs = list(source_name = "N")))
  raw_df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  result <- detect_schema_drift(raw_df, store, required = c("study_id", "effect", "se"))

  expect_equal(result$missing_optional_roles[["n_obs"]], "N")
  expect_equal(length(result$missing_roles), 0L)
})
