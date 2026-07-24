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

test_that("propose_renames returns an empty named list when no columns are available", {
  box::use(artma / data / schema_detect[propose_renames])

  result <- propose_renames(c(pub = "publication_year"), character(0))

  expect_equal(length(result), 0L)
  expect_true(is.list(result))
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
