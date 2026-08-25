box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_true,
    test_that
  ]
)

box::use(
  artma / data / column_recognition[recognize_columns]
)

# End-to-end auto-detection scenarios on synthetic datasets that mimic real
# meta-analysis exports: misleadingly named identifier columns, terse effect
# column names, decoy effect/se pairs, and wide blocks of auxiliary columns.
# Each scenario asserts that recognize_columns picks the right columns or
# correctly declines to auto-accept a required mapping.

#' Build the shared core of a synthetic meta-analysis dataset: per-study
#' structure, a rounded (effect, se, t) triple, sample sizes, and study labels.
make_meta_core <- function(n_studies = 25, k = 6, effect_mean = 0.2, effect_sd = 0.5) {
  study <- rep(seq_len(n_studies), each = k)
  n <- length(study)
  effect <- round(stats::rnorm(n, effect_mean, effect_sd), 3)
  se <- pmax(round(exp(stats::rnorm(n, log(0.15), 0.5)), 3), 0.001)
  list(
    n = n,
    study = study,
    counter = rep(seq_len(k), times = n_studies),
    effect = effect,
    se = se,
    t = round(effect / se, 4),
    n_obs = rep(sample(80:2000, n_studies, replace = TRUE), each = k),
    labels = rep(
      sprintf("Author%02d (%d)", seq_len(n_studies), 1995 + seq_len(n_studies) %% 20),
      each = k
    )
  )
}


test_that("determinants-style export maps the consistent (e, se, t) triple", {
  withr::local_seed(42)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()
  n <- core$n

  # Mirrors the real failure case: idcoeff is a per-study coefficient counter
  # whose name scores highly for effect, the true triple is (e, se, t), and
  # decoy pairs plus wide auxiliary columns surround them.
  df <- data.frame(
    idstudy = core$study,
    study = core$labels,
    idcoeff = core$counter,
    table = sample(1:9, n, replace = TRUE),
    orige = round(core$effect * 1.7 + stats::rnorm(n, 0, 0.02), 3),
    origse = pmax(round(core$se * 1.7, 3), 0.001),
    t = core$t,
    e = core$effect,
    se = core$se,
    country = sample(c("US", "DE", "CZ"), n, replace = TRUE),
    nobs = core$n_obs,
    start = sample(1990:2010, n, replace = TRUE),
    end = sample(2011:2020, n, replace = TRUE),
    ols = sample(0:1, n, replace = TRUE),
    gmm = sample(0:1, n, replace = TRUE),
    panel = sample(0:1, n, replace = TRUE)
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "e")
  expect_equal(mapping$se, "se")
  expect_equal(mapping$t_stat, "t")
  expect_equal(mapping$n_obs, "nobs")
  expect_equal(mapping$study_id, "study")
})


test_that("identifier columns with effect-flavored names are never picked", {
  withr::local_seed(43)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  df <- data.frame(
    idcoeff = core$counter,
    coef_id = seq_len(core$n),
    estimate_no = seq_len(core$n),
    beta = core$effect,
    stderr = core$se,
    study = core$labels,
    sample_size = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "beta")
  expect_equal(mapping$se, "stderr")
  expect_equal(mapping$n_obs, "sample_size")
  expect_false(mapping$effect %in% c("idcoeff", "coef_id", "estimate_no"))
})


test_that("a fully consistent triple is mapped even without name evidence", {
  withr::local_seed(44)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  df <- data.frame(
    x = core$effect,
    y = core$se,
    z = core$t,
    study = core$labels,
    nobs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "x")
  expect_equal(mapping$se, "y")
})


test_that("dataset with only (effect, se) and terse names resolves correctly", {
  withr::local_seed(45)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  df <- data.frame(
    study = core$labels,
    b = core$effect,
    se = core$se,
    nobs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "b")
  expect_equal(mapping$se, "se")
})


test_that("dataset with (effect, t) but no se maps both and leaves se unmapped", {
  withr::local_seed(46)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  df <- data.frame(
    study = core$labels,
    d = core$effect,
    t_value = core$t,
    nobs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "d")
  expect_equal(mapping$t_stat, "t_value")
  expect_false("se" %in% names(mapping))
})


test_that("percentage-scale effects pass the pair consistency checks", {
  withr::local_seed(47)
  withr::local_options(list("artma.verbose" = 1))
  n_studies <- 25
  k <- 6
  n <- n_studies * k
  effect <- round(stats::rnorm(n, 5, 12), 1)
  se <- round(stats::runif(n, 1, 15), 1)

  df <- data.frame(
    study = rep(sprintf("Study %02d", seq_len(n_studies)), each = k),
    effect = effect,
    se = se,
    n_obs = rep(sample(100:5000, n_studies, replace = TRUE), each = k)
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "effect")
  expect_equal(mapping$se, "se")
  expect_equal(mapping$n_obs, "n_obs")
})


test_that("log-scale effect columns resolve through name plus pair evidence", {
  withr::local_seed(48)
  withr::local_options(list("artma.verbose" = 1))
  n <- 150
  effect <- round(stats::rnorm(n, 0, 0.25), 4)
  se <- pmax(round(stats::runif(n, 0.02, 0.2), 4), 0.001)

  df <- data.frame(
    study = rep(sprintf("Author%02d (2001)", 1:25), each = 6),
    es = effect,
    se = se,
    lnyears = round(log(sample(1:30, n, replace = TRUE)), 3),
    n_obs = rep(sample(50:900, 25, replace = TRUE), each = 6)
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "es")
  expect_equal(mapping$se, "se")
})


test_that("duplicated effect candidates are claimed at most once", {
  withr::local_seed(49)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  df <- data.frame(
    study = core$labels,
    effect = core$effect,
    effect_size = core$effect,
    se = core$se,
    n_obs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_true(mapping$effect %in% c("effect", "effect_size"))
  mapped_cols <- unlist(mapping)
  expect_equal(length(mapped_cols), length(unique(mapped_cols)))
})


test_that("a counter named coef is declined rather than guessed", {
  withr::local_seed(50)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  # The only effect-named column is a per-study counter; auto-detection must
  # refuse the mapping (a later prompt or explicit config resolves it) instead
  # of accepting the identifier.
  df <- data.frame(
    study = core$labels,
    coef = core$counter,
    se = core$se,
    nobs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_false("effect" %in% names(mapping))
  expect_equal(mapping$se, "se")
})


test_that("an unnamed effect column without corroboration is not auto-accepted", {
  withr::local_seed(51)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  # "value" holds real effect sizes but nothing signals it by name and no
  # t-statistic column corroborates the pair, so auto-detection stays
  # conservative and leaves the required mapping to the user.
  df <- data.frame(
    study = core$labels,
    idcoeff = core$counter,
    value = core$effect,
    se = core$se,
    nobs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_false("effect" %in% names(mapping))
  expect_equal(mapping$se, "se")
  expect_equal(mapping$n_obs, "nobs")
})


test_that("decoy effect/se pairs lose to the pair corroborated by the t column", {
  withr::local_seed(52)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()
  n <- core$n

  # eb/seb and eh/seh are alternative-specification estimates (internally
  # consistent pairs on their own), but only (e, se) matches the t column.
  df <- data.frame(
    study = core$labels,
    e = core$effect,
    se = core$se,
    t = core$t,
    eb = round(core$effect * 0.6 + stats::rnorm(n, 0, 0.05), 3),
    seb = pmax(round(core$se * 0.6, 3), 0.001),
    eh = round(core$effect * 1.4 + stats::rnorm(n, 0, 0.05), 3),
    seh = pmax(round(core$se * 1.4, 3), 0.001),
    nobs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "e")
  expect_equal(mapping$se, "se")
  expect_equal(mapping$t_stat, "t")
})


test_that("year columns are not mistaken for numeric roles", {
  withr::local_seed(53)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  df <- data.frame(
    study = core$labels,
    pub_year = rep(sample(1995:2020, 25, replace = TRUE), each = 6),
    effect = core$effect,
    se = core$se,
    n_obs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "effect")
  expect_equal(mapping$se, "se")
  expect_equal(mapping$n_obs, "n_obs")
  expect_false("pub_year" %in% unlist(mapping))
})


# Regression scenarios from the meta-analysis.cz real-data benchmark (2026-08).

test_that("an id-prefixed study key (idstudy) is recognized like the reversed form", {
  withr::local_seed(54)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  # "idstudy" (id-first, no separator) is as common in real exports as
  # "studyid"/"study_id"; it must not rely on keyword substring matching.
  df <- data.frame(
    idstudy = core$study,
    effect = core$effect,
    se = core$se,
    n_obs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$study_id, "idstudy")
})


test_that("a compound t-stat name (tstat_premium) is still recognized", {
  withr::local_seed(55)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  # Datasets with several parallel estimate blocks often suffix the role name
  # with the block label (tstat_premium, TSTAT_L); this must not depend on
  # the bare keyword-substring path removed for the "es"/"se" false positives.
  df <- data.frame(
    study = core$labels,
    premium = core$effect,
    se_premium = core$se,
    tstat_premium = core$t,
    nobs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$t_stat, "tstat_premium")
})


test_that("a study label with low uniqueness (many estimates per study) is preferred", {
  withr::local_seed(56)
  withr::local_options(list("artma.verbose" = 1))
  # 40 studies, 3500 estimates: a uniqueness ratio (~0.011) well under the old
  # 0.05 floor, but completely normal for a real meta-analysis.
  n_studies <- 40
  k <- 88
  n <- n_studies * k
  df <- data.frame(
    idstudy = rep(seq_len(n_studies), each = k),
    author = rep(sprintf("Author%02d (%d)", seq_len(n_studies), 1990 + seq_len(n_studies)), each = k),
    effect = round(stats::rnorm(n, 0.2, 0.4), 3),
    se = pmax(round(stats::runif(n, 0.02, 0.3), 3), 0.001),
    n_obs = rep(sample(50:900, n_studies, replace = TRUE), each = k)
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$study_id, "author")
})


test_that("a t_stat column preferring t_stat by name is not claimed as effect", {
  withr::local_seed(57)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()
  n <- core$n

  # tstats = effect / se by construction, and 1/se is a plausible-looking
  # "se" on its own; the joint pass must not let this decoy pair (tstats,
  # invse) outscore the true (effect, se) pair just because tstats' name
  # more strongly favors t_stat than effect.
  df <- data.frame(
    study = core$labels,
    armel = core$effect,
    se = core$se,
    tstats = core$t,
    invse = round(1 / core$se, 4),
    nobs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "armel")
  expect_equal(mapping$se, "se")
})


test_that("a partially matching witness column cannot flip the se choice", {
  withr::local_seed(59)
  withr::local_options(list("artma.verbose" = 1))
  n <- 200
  effect <- round(stats::rnorm(n, 0.2, 0.5), 3)
  se <- pmax(round(exp(stats::rnorm(n, log(0.15), 0.4)), 3), 0.001)
  se_wide <- round(se * 1.3, 3)

  # "wd" reproduces estimate / se_wide on a minority of rows (a derived
  # interval-width statistic); corroboration that patchy is coincidence and
  # must not outrank the equally-named, first-listed se_low.
  wd <- round(stats::rnorm(n, 0, 3), 3)
  partial <- seq_len(floor(n * 0.2))
  wd[partial] <- round(effect[partial] / se_wide[partial], 4)

  df <- data.frame(
    study = rep(sprintf("Author%02d (2005)", 1:25), each = 8),
    estimate = effect,
    se_low = se,
    se_wide = se_wide,
    wd = wd
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "estimate")
  expect_equal(mapping$se, "se_low")
})


test_that("an anonymous se crowded out by moderator *_se columns still wins via its t witness", {
  withr::local_seed(60)
  withr::local_options(list("artma.verbose" = 1))
  n <- 200
  pcc <- round(stats::rnorm(n, 0.15, 0.2), 4)
  sepcc <- round(stats::runif(n, 0.02, 0.3), 4)

  # Eight se-suffixed moderator columns outrank the true, anonymously named
  # sepcc on name score; only the file's own t-statistic column identifies
  # the real pair arithmetically.
  df <- data.frame(
    study = rep(sprintf("Study%02d", 1:25), each = 8),
    pcc = pcc,
    sepcc = sepcc,
    tstat = round(pcc / sepcc, 4),
    nobs = rep(sample(80:2000, 25, replace = TRUE), each = 8)
  )
  for (i in 1:8) {
    df[[paste0("mod", i, "_se")]] <- round(stats::runif(n, 10, 20), 3)
  }

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "pcc")
  expect_equal(mapping$se, "sepcc")
  expect_equal(mapping$t_stat, "tstat")
})


test_that("a name-matched se column with a handful of distinct values is declined", {
  withr::local_seed(61)
  withr::local_options(list("artma.verbose" = 1))
  n <- 300
  resp <- round(stats::rnorm(n, 0.1, 0.3), 3)
  ci_se <- sample(c(1, 2, 4), n, replace = TRUE)
  ci_se[sample(seq_len(n), floor(n * 0.4))] <- NA

  df <- data.frame(
    study = rep(sprintf("Study%02d", 1:30), each = 10),
    resp = resp,
    ci_se = ci_se,
    nobs = rep(sample(30:544, 30, replace = TRUE), each = 10)
  )

  mapping <- recognize_columns(df)

  expect_false("se" %in% names(mapping))
  expect_false("effect" %in% names(mapping))
  expect_equal(mapping$n_obs, "nobs")
})


test_that("a nearly empty se column is not mapped on the values it does have", {
  withr::local_seed(62)
  withr::local_options(list("artma.verbose" = 1))
  n <- 1800
  se_raw <- rep(NA_real_, n)
  filled <- sample(seq_len(n), 40)
  se_raw[filled] <- round(stats::runif(40, 0.19, 1.1), 3)

  df <- data.frame(
    study = rep(sprintf("Author%02d (2010)", 1:60), each = 30),
    effect = round(stats::rnorm(n, 0.2, 0.4), 3),
    se_raw = se_raw
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$effect, "effect")
  expect_false("se" %in% names(mapping))
})


test_that("an estimates-per-study count loses n_obs to the true sample-size column", {
  withr::local_seed(63)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  # "n" matches the n_obs regex exactly but counts estimates per study
  # (minimum 1); "samsize" holds the real, larger-scale sample sizes.
  df <- data.frame(
    study = core$labels,
    effect = core$effect,
    se = core$se,
    n = sample(1:40, core$n, replace = TRUE),
    samsize = rep(sample(14:2982, 25, replace = TRUE), each = 6)
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$n_obs, "samsize")
})


test_that("a totalobs column is recognized as n_obs", {
  withr::local_seed(64)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  df <- data.frame(
    study = core$labels,
    effect = core$effect,
    se = core$se,
    TotalObs = rep(sample(20:5000, 25, replace = TRUE), each = 6)
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$n_obs, "TotalObs")
})


test_that("a dummy column matching n_obs by name is declined", {
  withr::local_seed(65)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()

  df <- data.frame(
    study = core$labels,
    effect = core$effect,
    se = core$se,
    rel_size = sample(0:1, core$n, replace = TRUE)
  )

  mapping <- recognize_columns(df)

  expect_false("n_obs" %in% names(mapping))
})


test_that("Stata-style missing markers do not sink a well-named se column", {
  withr::local_seed(58)
  withr::local_options(list("artma.verbose" = 1))
  core <- make_meta_core()
  n <- core$n

  se_text <- as.character(core$se)
  se_text[sample(seq_len(n), floor(n * 0.15))] <- "."

  df <- data.frame(
    study = core$labels,
    effect = core$effect,
    se = se_text,
    n_obs = core$n_obs
  )

  mapping <- recognize_columns(df)

  expect_equal(mapping$se, "se")
})
