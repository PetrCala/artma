box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_identical,
    expect_null,
    expect_true,
    test_that
  ]
)

# Unified per-column store used across the reconcile tests: role records carry
# the source column mapping; moderators are keyed by their own name.
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

base_reconcile_opts <- function(extra = list()) {
  utils::modifyList(
    list(
      "artma.data.columns" = base_store(),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs"),
      "artma.temp.file_name" = NULL,
      "artma.temp.dir_name" = NULL,
      "artma.verbose" = 1L
    ),
    extra
  )
}

base_df <- function(...) {
  data.frame(effect_size = 1:3, se_col = 0.1, study = "A", n_obs = 10L, ...)
}

# detect_schema_drift

test_that("detect_schema_drift reports no drift when all columns match", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  result <- detect_schema_drift(base_df(), base_store())

  expect_false(result$has_drift)
  expect_equal(length(result$missing_roles), 0L)
  expect_equal(length(result$missing_moderators), 0L)
  expect_equal(length(result$added), 0L)
})

test_that("detect_schema_drift detects a renamed required column", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # effect_size was renamed to es in the df
  raw_df <- data.frame(es = 1:3, se_col = 0.1, study = "A", n_obs = 10L)

  result <- detect_schema_drift(raw_df, base_store())

  expect_true(result$has_drift)
  expect_true("effect" %in% names(result$missing_roles))
  expect_equal(result$missing_roles[["effect"]], "effect_size")
  expect_equal(length(result$missing_moderators), 0L)
})

test_that("detect_schema_drift tracks required roles with no record as identity mappings", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # n_obs has no record in the store; when the df lacks the column under its
  # standard name, that is drift.
  raw_df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  result <- detect_schema_drift(raw_df, base_store())

  expect_true(result$has_drift)
  expect_true("n_obs" %in% names(result$missing_roles))
  expect_equal(result$missing_roles[["n_obs"]], "n_obs")
})

test_that("detect_schema_drift detects a missing moderator column", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # method_iv was removed from df, but its record is in the store
  store <- base_store(list(method_iv = list(bma = TRUE)))

  result <- detect_schema_drift(base_df(), store)

  expect_true(result$has_drift)
  expect_true("method_iv" %in% result$missing_moderators)
  expect_equal(length(result$missing_roles), 0L)
})

test_that("detect_schema_drift detects newly added columns", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # region is new in the df
  result <- detect_schema_drift(base_df(region = "EU"), base_store())

  expect_true(result$has_drift)
  expect_true("region" %in% result$added)
  expect_equal(length(result$missing_roles), 0L)
  expect_equal(length(result$missing_moderators), 0L)
})

test_that("detect_schema_drift ignores role records with an NA source_name", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # An optional role record with NA source_name should not be tracked
  store <- base_store(list(t_stat = list(source_name = NA)))

  result <- detect_schema_drift(base_df(), store)

  expect_false(result$has_drift)
})

test_that("detect_schema_drift skips computed column records", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # Computed columns are added by the pipeline, never present in the raw df
  store <- base_store(list(
    precision = list(var_name = "precision", is_computed = TRUE),
    study_label = list(var_name = "study_label", is_computed = TRUE)
  ))

  result <- detect_schema_drift(base_df(), store)

  expect_false(result$has_drift)
})

test_that("detect_schema_drift handles multiple simultaneous drifts", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  raw_df <- data.frame(es = 1:3, se_col = 0.1, study = "A", n_obs = 10L, region = "EU")
  store <- base_store(list(method_iv = list(bma = TRUE)))

  result <- detect_schema_drift(raw_df, store)

  expect_true(result$has_drift)
  expect_true("effect" %in% names(result$missing_roles))
  expect_true("method_iv" %in% result$missing_moderators)
  expect_true("region" %in% result$added)
})

# Rename scoring (shared matching engine)

test_that("score_rename_candidate finds obvious renames via string similarity", {
  box::use(artma / data / column_recognition[MATCH_THRESHOLDS, score_rename_candidate])

  score_pub <- score_rename_candidate("publication_year", "pub_year")
  score_qq <- score_rename_candidate("effect_size", "xyz_qq")

  # pub_year is a plausible rename of publication_year; xyz_qq is not
  expect_true(score_pub > score_qq)
  expect_true(score_pub >= MATCH_THRESHOLDS$rename_suggest)
  expect_true(score_qq < MATCH_THRESHOLDS$rename_suggest)
})

test_that("score_rename_candidate uses the pattern engine when the role is known", {
  box::use(artma / data / column_recognition[MATCH_THRESHOLDS, score_rename_candidate])

  # "beta" is nothing like "effect_size" as a string, but the recognition
  # patterns identify it as an effect column.
  without_role <- score_rename_candidate("effect_size", "beta")
  with_role <- score_rename_candidate("effect_size", "beta", std_name = "effect")

  expect_true(with_role > without_role)
  expect_true(with_role >= MATCH_THRESHOLDS$rename_auto)
})

# Mapping conflicts: a role mapped to a source column while a different raw
# column already occupies the standard name.

test_that("detect_schema_drift flags a mapping conflict with an occupying raw column", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # study_id is mapped from "study", but the df also has its own study_id
  # column with different content.
  raw_df <- base_df(study_id = c("X", "Y", "Z"))

  result <- detect_schema_drift(raw_df, base_store())

  expect_true(result$has_drift)
  expect_equal(result$conflicts[["study_id"]], "study")
  expect_equal(length(result$missing_roles), 0L)
  # The occupying column carries a standard name, so it is not "added" either.
  expect_false("study_id" %in% result$added)
})

test_that("detect_schema_drift ignores a byte-identical occupying column", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # Identical content: standardize_column_names() resolves this quietly.
  raw_df <- base_df()
  raw_df$study_id <- raw_df$study

  result <- detect_schema_drift(raw_df, base_store())

  expect_equal(length(result$conflicts), 0L)
})

test_that("detect_schema_drift skips conflicts already resolved via drop_conflicting_raw", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  raw_df <- base_df(study_id = c("X", "Y", "Z"))
  store <- base_store(
    list(study_id = list(source_name = "study", drop_conflicting_raw = TRUE))
  )

  result <- detect_schema_drift(raw_df, store)

  expect_equal(length(result$conflicts), 0L)
})

test_that("detect_schema_drift reports a missing source, not a conflict, when the source vanished", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  # "study" is gone; the occupying study_id column is a rename candidate, not
  # a conflict.
  raw_df <- data.frame(
    effect_size = 1:3, se_col = 0.1, study_id = "A", n_obs = 10L
  )

  result <- detect_schema_drift(raw_df, base_store())

  expect_true("study_id" %in% names(result$missing_roles))
  expect_equal(length(result$conflicts), 0L)
})

test_that("reconcile_schema auto resolves a conflict by keeping the mapping", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  raw_df <- base_df(study_id = c("X", "Y", "Z"))

  withr::with_options(
    base_reconcile_opts(),
    {
      reconcile_schema(raw_df, mode = "auto")

      store <- getOption("artma.data.columns")
      expect_equal(store$study_id$source_name, "study")
      expect_true(isTRUE(store$study_id$drop_conflicting_raw))
      expect_true("study_id" %in% getOption("artma.data.expected_schema_columns"))
    }
  )
})

test_that("reconcile_schema resolves conflicts even on the first run without a baseline", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  raw_df <- base_df(study_id = c("X", "Y", "Z"))

  withr::with_options(
    base_reconcile_opts(
      list("artma.data.expected_schema_columns" = NA_character_)
    ),
    {
      reconcile_schema(raw_df, mode = "auto")

      store <- getOption("artma.data.columns")
      expect_true(isTRUE(store$study_id$drop_conflicting_raw))
      expect_true("study_id" %in% getOption("artma.data.expected_schema_columns"))
    }
  )
})

test_that("reconcile_schema strict aborts on a mapping conflict", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  raw_df <- base_df(study_id = c("X", "Y", "Z"))

  withr::with_options(
    base_reconcile_opts(),
    expect_error(
      reconcile_schema(raw_df, mode = "strict"),
      "Mapping conflict"
    )
  )
})

# reconcile_schema (strict mode)
#
# The strict-mode abort paths (missing required column, missing moderator, added
# column) are exercised with stronger `regexp` assertions in
# test-schema-reconcile-integration.R (T2/T3/T4); this file keeps only the
# no-drift pass case, which that file does not cover.

test_that("reconcile_schema in strict mode passes when no drift exists", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  result <- withr::with_options(
    base_reconcile_opts(),
    reconcile_schema(base_df(), mode = "strict")
  )

  expect_null(result)
})

# Derived columns
#
# `data.derived` columns are created at the end of the compute phase, one step
# after reconciliation runs, so a config entry naming one is not drift (#541).

test_that("detect_schema_drift ignores configured data.derived columns", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  store <- base_store(list(se_top5_journal = list(bma = TRUE)))

  result <- withr::with_options(
    list("artma.data.derived" = list(se_top5_journal = "se * top5_journal")),
    detect_schema_drift(base_df(), store)
  )

  expect_false(result$has_drift)
  expect_equal(length(result$missing_moderators), 0L)
})

test_that("detect_schema_drift accepts an explicit derived set", {
  box::use(artma / data / schema_reconcile[detect_schema_drift])

  store <- base_store(list(se_top5_journal = list(bma = TRUE)))

  result <- detect_schema_drift(
    base_df(), store,
    derived = "se_top5_journal"
  )

  expect_equal(length(result$missing_moderators), 0L)

  # Without it, the same entry is the missing moderator it used to be.
  plain <- withr::with_options(
    list("artma.data.derived" = NULL),
    detect_schema_drift(base_df(), store)
  )
  expect_true("se_top5_journal" %in% plain$missing_moderators)
})

test_that("reconcile_schema auto keeps a derived moderator in the config", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_reconcile_opts(list(
      "artma.data.columns" = base_store(list(se_top5_journal = list(bma = TRUE))),
      "artma.data.derived" = list(se_top5_journal = "se * top5_journal")
    )),
    {
      reconcile_schema(base_df(), mode = "auto")

      store <- getOption("artma.data.columns")
      expect_true("se_top5_journal" %in% names(store))
      expect_true(isTRUE(store$se_top5_journal$bma))
    }
  )
})

# Optional roles
#
# A role outside the run's required set whose mapped source column vanished is
# not a blocker: the pipeline tolerates an unmapped optional role, so the
# mapping is dropped (or remapped when the match is clear) and the run goes on.

optional_role_opts <- function(extra = list()) {
  base_reconcile_opts(utils::modifyList(
    list(
      "artma.data.columns" = base_store(list(t_stat = list(source_name = "tstat"))),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "tstat")
    ),
    extra
  ))
}

test_that("reconcile_schema auto drops the mapping of an optional role whose source vanished", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    optional_role_opts(),
    {
      expect_null(reconcile_schema(base_df(), mode = "auto"))
      expect_null(getOption("artma.data.columns")[["t_stat"]])
      expect_false("tstat" %in% getOption("artma.data.expected_schema_columns"))
    }
  )
})

test_that("reconcile_schema auto keeps the rest of an optional role's record when dropping its mapping", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    optional_role_opts(list(
      "artma.data.columns" = base_store(list(t_stat = list(source_name = "tstat", bma = TRUE)))
    )),
    {
      reconcile_schema(base_df(), mode = "auto")
      entry <- getOption("artma.data.columns")[["t_stat"]]
      expect_null(entry$source_name)
      expect_true(isTRUE(entry$bma))
    }
  )
})

test_that("reconcile_schema auto remaps an optional role when the rename is unambiguous", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    optional_role_opts(),
    {
      reconcile_schema(base_df(tstats = c(1.5, 2.5, 3.5)), mode = "auto")
      expect_equal(getOption("artma.data.columns")$t_stat$source_name, "tstats")
    }
  )
})

test_that("reconcile_schema auto treats a mapped role outside the run's required set as optional", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # n_obs was mapped to N by hand; N is gone, and no requested method needs it.
  raw_df <- data.frame(effect_size = 1:3, se_col = 0.1, study = "A")

  withr::with_options(
    base_reconcile_opts(list(
      "artma.data.columns" = base_store(list(n_obs = list(source_name = "N"))),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "N")
    )),
    {
      expect_null(reconcile_schema(
        raw_df,
        mode = "auto", required_colnames = c("study_id", "effect", "se")
      ))
      expect_null(getOption("artma.data.columns")[["n_obs"]])
    }
  )
})

test_that("reconcile_schema strict reports a vanished optional mapping", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    optional_role_opts(),
    expect_error(reconcile_schema(base_df(), mode = "strict"), "optional")
  )
})

# Substring renames and exclusive assignment

test_that("reconcile_schema auto does not remap a moderator onto a column that merely contains its name", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_reconcile_opts(list(
      "artma.data.columns" = base_store(list(gdp = list(bma = TRUE, subset = "gdp > 0"))),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "gdp", "gdp_growth", "log_gdp")
    )),
    {
      reconcile_schema(base_df(gdp_growth = 1:3, log_gdp = 4:6), mode = "auto")
      store <- getOption("artma.data.columns")
      expect_null(store[["gdp"]])
      expect_null(store[["gdp_growth"]])
      expect_null(store[["log_gdp"]])
    }
  )
})

test_that("reconcile_schema auto never remaps two moderators onto one column", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_reconcile_opts(list(
      "artma.data.columns" = base_store(list(
        x_1 = list(bma = TRUE, note = "one"),
        x_10 = list(bma = FALSE, note = "ten")
      )),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "x_1", "x_10")
    )),
    {
      reconcile_schema(base_df(x_100 = 1:3), mode = "auto")
      store <- getOption("artma.data.columns")
      # x_10 is the closer name and takes x_100; x_1 is dropped rather than
      # overwriting x_10's record on the same key.
      expect_equal(store$x_100$note, "ten")
      expect_null(store[["x_1"]])
      expect_null(store[["x_10"]])
    }
  )
})

# Priority across the buckets is by auto-acceptability first, bucket second: a
# bucket withholds a candidate from lower-priority buckets only when it would
# actually apply that rename unasked.

test_that("award_renames hands a merely suggested candidate to a bucket that can auto-apply it", {
  box::use(artma / data / schema_reconcile[award_renames])

  # study_id scores the same on both new columns (a tie it would never apply
  # unasked); the n_studies moderator is confident about "studies".
  raw_df <- data.frame(studies = 1:3, studied = 4:6, se_col = 0.1, n_obs = 10L)
  awarded <- award_renames(
    list(
      roles = list(missing = c(study_id = "study"), roles_known = TRUE),
      moderators = list(missing = c(n_studies = "n_studies"))
    ),
    c("studies", "studied"),
    raw_df
  )

  expect_equal(awarded$moderators$n_studies$candidate, "studies")
  expect_equal(awarded$roles$study_id$candidate, "studied")
})

test_that("award_renames keeps a candidate with the higher bucket when both would auto-apply it", {
  box::use(artma / data / schema_reconcile[award_renames])

  raw_df <- data.frame(study_no = 1:3, se_col = 0.1, n_obs = 10L)
  awarded <- award_renames(
    list(
      roles = list(missing = c(study_id = "study"), roles_known = TRUE),
      moderators = list(missing = c(study_num = "study_num"))
    ),
    "study_no",
    raw_df
  )

  expect_equal(awarded$roles$study_id$candidate, "study_no")
  expect_true(is.na(awarded$moderators$study_num$candidate))
})

test_that("award_renames never proposes one column to two buckets", {
  box::use(artma / data / schema_reconcile[award_renames])

  raw_df <- data.frame(studies = 1:3, studied = 4:6, se_col = 0.1, n_obs = 10L)
  awarded <- award_renames(
    list(
      roles = list(missing = c(study_id = "study"), roles_known = TRUE),
      optional = list(missing = c(t_stat = "tstat"), roles_known = TRUE),
      moderators = list(missing = c(n_studies = "n_studies"))
    ),
    c("studies", "studied"),
    raw_df
  )

  claimed <- unlist(lapply(awarded, function(props) {
    cands <- vapply(props, function(prop) prop$candidate, character(1))
    cands[!is.na(cands)]
  }), use.names = FALSE)
  expect_equal(anyDuplicated(claimed), 0L)
})

test_that("reconcile_schema auto aborts on a tied required-role suggestion instead of picking by column order", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(
    effect_size_b = c(0.1, 0.2, 0.3), effect_size_a = c(0.15, 0.25, 0.35),
    se_col = 0.1, study = "A", n_obs = 10L
  )

  withr::with_options(
    base_reconcile_opts(),
    {
      expect_error(reconcile_schema(df, mode = "auto"), "ambiguous")
      expect_equal(getOption("artma.data.columns")$effect$source_name, "effect_size")
    }
  )
})

test_that("the auto-mode abort points at data.reconcile_mode rather than a nonexistent argument", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  df <- data.frame(xyz_qq = 1:3, se_col = 0.1, study = "A", n_obs = 10L)

  message <- withr::with_options(
    base_reconcile_opts(),
    tryCatch(reconcile_schema(df, mode = "auto"), error = function(e) conditionMessage(e))
  )

  expect_true(grepl("data.reconcile_mode", message, fixed = TRUE))
  expect_true(grepl("config_set", message, fixed = TRUE))
  expect_false(grepl("reconcile = ", message, fixed = TRUE))
})

test_that("the strict-mode abort points at data.reconcile_mode rather than a nonexistent argument", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  message <- withr::with_options(
    base_reconcile_opts(),
    tryCatch(reconcile_schema(base_df(region = "EU"), mode = "strict"), error = function(e) conditionMessage(e))
  )

  expect_true(grepl("data.reconcile_mode", message, fixed = TRUE))
  expect_false(grepl("reconcile = ", message, fixed = TRUE))
})

# Ask mode, driven through the injectable menu backend
#
# `scripted_menu()` answers each prompt in turn with the first choice matching
# the next pattern, and records the choices every prompt offered so a test can
# assert on what was (not) on the menu.

scripted_menu <- function(patterns) {
  state <- new.env()
  state$i <- 0L
  state$offered <- list()
  fn <- function(choices, prompt, ...) {
    state$i <- state$i + 1L
    state$offered[[state$i]] <- unname(choices)
    if (state$i > length(patterns)) {
      stop("unexpected prompt: ", prompt)
    }
    hit <- grep(patterns[[state$i]], choices, value = TRUE)
    if (length(hit) == 0) {
      stop("no choice matching '", patterns[[state$i]], "' among: ", paste(choices, collapse = " | "))
    }
    hit[[1]]
  }
  list(fn = fn, state = state)
}

ask_opts <- function(extra = list()) {
  base_reconcile_opts(utils::modifyList(
    list("artma.autonomy.level" = "ask_more"),
    extra
  ))
}

test_that("reconcile_schema ask offers to drop a vanished optional mapping", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  menu <- scripted_menu(c("^Drop the mapping", "^Save"))

  withr::with_options(
    ask_opts(list(
      "artma.data.columns" = base_store(list(t_stat = list(source_name = "tstat"))),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "tstat")
    )),
    {
      reconcile_schema(base_df(), mode = "ask", is_interactive = TRUE, select_fn = menu$fn)
      expect_null(getOption("artma.data.columns")[["t_stat"]])
    }
  )
  expect_equal(menu$state$i, 2L)
})

test_that("reconcile_schema ask can remap a vanished optional mapping", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  menu <- scripted_menu(c("^Remap to", "^Save"))

  withr::with_options(
    ask_opts(list(
      "artma.data.columns" = base_store(list(t_stat = list(source_name = "tstat"))),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "tstat")
    )),
    {
      reconcile_schema(base_df(tstats = c(1.5, 2.5, 3.5)), mode = "ask", is_interactive = TRUE, select_fn = menu$fn)
      expect_equal(getOption("artma.data.columns")$t_stat$source_name, "tstats")
    }
  )
})

test_that("reconcile_schema ask withholds a column already chosen from later menus", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # Both required sources are gone. The user maps effect by hand onto the very
  # column proposed for se, so the se prompt must not offer to accept it, and
  # the manual list for se must not contain it either.
  df <- data.frame(es = c(0.1, 0.2, 0.3), std_err = c(0.01, 0.02, 0.03), study = "A", n_obs = 10L)
  menu <- scripted_menu(c(
    "^Map to a different column", "^std_err$", # effect: manual, take std_err
    "^Map to a different column", "^es$", # se: no Accept offered, pick es
    "^Save"
  ))

  withr::with_options(
    ask_opts(),
    {
      reconcile_schema(df, mode = "ask", is_interactive = TRUE, select_fn = menu$fn)
      store <- getOption("artma.data.columns")
      expect_equal(store$effect$source_name, "std_err")
      expect_equal(store$se$source_name, "es")
    }
  )

  offered <- menu$state$offered
  expect_true(any(grepl("^Accept", offered[[1]])))
  expect_false(any(grepl("^Accept", offered[[3]])))
  expect_false("std_err" %in% offered[[4]])
})

test_that("reconcile_schema ask offers the confident record the candidate a required role only suggests", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # "study" and "n_studies" both vanished, replaced by "studies" and "studied".
  # study_id is equally unsure about the two, so "studies" goes to the
  # moderator that is confident about it, and study_id keeps "studied".
  menu <- scripted_menu(c("^Accept", "^Remap to", "^Save"))
  df <- data.frame(effect_size = 1:3, se_col = 0.1, studies = 4:6, studied = 7:9, n_obs = 10L)

  withr::with_options(
    ask_opts(list(
      "artma.data.columns" = base_store(list(n_studies = list(bma = TRUE))),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "n_studies")
    )),
    {
      reconcile_schema(df, mode = "ask", is_interactive = TRUE, select_fn = menu$fn)
      store <- getOption("artma.data.columns")
      expect_equal(store$study_id$source_name, "studied")
      expect_true(isTRUE(store$studies$bma))
      expect_null(store[["n_studies"]])
    }
  )

  offered <- menu$state$offered
  expect_true(any(grepl("studied", offered[[1]], fixed = TRUE)))
  expect_true(any(grepl("studies", offered[[2]], fixed = TRUE)))
})

test_that("reconcile_schema ask aborts cleanly when the user declines to save", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  menu <- scripted_menu(c("^Drop the mapping", "^Abort"))

  withr::with_options(
    ask_opts(list(
      "artma.data.columns" = base_store(list(t_stat = list(source_name = "tstat"))),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "tstat")
    )),
    {
      expect_error(
        reconcile_schema(base_df(), mode = "ask", is_interactive = TRUE, select_fn = menu$fn),
        "aborted by user"
      )
      expect_equal(getOption("artma.data.columns")$t_stat$source_name, "tstat")
    }
  )
})

# A menu backend is injected, so what it returns is input, not a given: only
# `NULL` (cancelled) or one of the offered labels may reach the configuration.

test_that("reconcile_schema ask rejects a menu answer that was never offered", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  garbage <- function(choices, prompt, ...) "this is not one of the choices at all"

  withr::with_options(
    ask_opts(list(
      "artma.data.columns" = base_store(list(t_stat = list(source_name = "tstat"))),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "tstat")
    )),
    {
      expect_error(
        reconcile_schema(base_df(), mode = "ask", is_interactive = TRUE, select_fn = garbage),
        "not offered"
      )
      expect_equal(getOption("artma.data.columns")$t_stat$source_name, "tstat")
    }
  )
})

test_that("reconcile_schema ask does not map a role onto a column the manual menu never offered", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  # Asks for the manual column list, then answers it with a column that is not
  # in the data at all.
  invent_column <- function(choices, prompt, ...) {
    hit <- grep("^Map to a different column", choices, value = TRUE)
    if (length(hit) > 0) hit[[1]] else "no_such_column"
  }

  withr::with_options(
    ask_opts(list(
      "artma.data.columns" = base_store(list(t_stat = list(source_name = "tstat"))),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "tstat")
    )),
    {
      expect_error(
        reconcile_schema(base_df(), mode = "ask", is_interactive = TRUE, select_fn = invent_column),
        "not offered"
      )
      expect_equal(getOption("artma.data.columns")$t_stat$source_name, "tstat")
    }
  )
})

test_that("reconcile_schema ask rejects a menu answer that is not a single label", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  return_index <- function(choices, prompt, ...) 1L

  withr::with_options(
    ask_opts(list(
      "artma.data.columns" = base_store(list(t_stat = list(source_name = "tstat"))),
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "tstat")
    )),
    {
      expect_error(
        reconcile_schema(base_df(), mode = "ask", is_interactive = TRUE, select_fn = return_index),
        "not offered"
      )
      expect_equal(getOption("artma.data.columns")$t_stat$source_name, "tstat")
    }
  )
})

# Baseline upkeep
#
# Removing an unmapped column is not drift, but the baseline must still follow
# the data: otherwise the column would never register as new if it came back.

test_that("reconcile_schema refreshes the baseline when an unmapped column disappears without drift", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_reconcile_opts(list(
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "region")
    )),
    {
      expect_null(reconcile_schema(base_df(), mode = "strict"))
      expect_equal(
        sort(getOption("artma.data.expected_schema_columns")),
        sort(c("effect_size", "se_col", "study", "n_obs"))
      )
    }
  )
})

test_that("a column that was removed and later restored is reported as new again", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  withr::with_options(
    base_reconcile_opts(list(
      "artma.data.expected_schema_columns" = c("effect_size", "se_col", "study", "n_obs", "region")
    )),
    {
      reconcile_schema(base_df(), mode = "auto") # region gone: no drift, baseline shrinks
      expect_error(reconcile_schema(base_df(region = "EU"), mode = "strict"), "region")
    }
  )
})

test_that("reconcile_schema leaves an up-to-date baseline alone", {
  box::use(artma / data / schema_reconcile[reconcile_schema])

  baseline <- c("n_obs", "study", "se_col", "effect_size") # same set, different order

  withr::with_options(
    base_reconcile_opts(list("artma.data.expected_schema_columns" = baseline)),
    {
      reconcile_schema(base_df(), mode = "auto")
      expect_identical(getOption("artma.data.expected_schema_columns"), baseline)
    }
  )
})
