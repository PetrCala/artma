box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_match,
    expect_no_match,
    expect_true,
    local_mocked_bindings,
    skip_if_not_installed,
    test_that
  ],
  withr[local_options],
  artma / econometric / maive[
    ar_ci_verdict,
    build_maive_interpretation,
    build_maive_summary,
    ci_excludes_zero,
    hausman_rejects,
    instrument_strength,
    maive_ci,
    maive_first_stage_f,
    maive_first_stage_is_weak,
    maive_first_stage_spec,
    maive_method_label,
    maive_p_from_coef,
    prepare_maive_data,
    resolve_maive_first_stage,
    run_maive
  ]
)

base_maive_options <- function(...) {
  defaults <- list(
    method = 3L,
    weight = 0L,
    instrument = 1L,
    studylevel = 2L,
    se = 1L,
    ar = 0L,
    first_stage = 0L,
    seed = 123L,
    add_significance_marks = TRUE,
    round_to = 3L
  )
  utils::modifyList(defaults, list(...))
}

make_maive_output <- function(...) {
  defaults <- list(
    beta = 0.2,
    SE = 0.05,
    `F-test` = 24,
    beta_standard = 0.31,
    SE_standard = 0.03,
    Hausman = 6.2,
    Chi2 = 3.841,
    AR_CI = c(0.05, 0.35),
    `pub bias p-value` = 0.004,
    egger_coef = 1.8,
    egger_se = 0.5,
    egger_boot_ci = c(0.8, 2.8),
    petpeese_selected = "PEESE",
    peese_se2_coef = -2.3,
    peese_se2_se = 0.9
  )
  utils::modifyList(defaults, list(...))
}

make_maive_df <- function(seed = 9, n = 40) {
  set.seed(seed)
  # Standard errors track 1/sqrt(N) so the first stage has something to find;
  # drawing them independently yields a very weak instrument and a MAIVE warning.
  n_obs <- sample(50:400, n, replace = TRUE)
  data.frame(
    effect = rnorm(n, 0.3, 0.1),
    se = 3 / sqrt(n_obs) + abs(rnorm(n, 0, 0.01)),
    n_obs = n_obs,
    study_id = rep(seq_len(8), length.out = n)
  )
}

# maive_p_from_coef ---------------------------------------------------------

test_that("maive_p_from_coef matches the two-sided normal p-value", {
  expect_equal(maive_p_from_coef(1.96, 1), 0.0499957902964409, tolerance = 1e-12)
  expect_equal(maive_p_from_coef(0, 2), 1, tolerance = 1e-12)
})

test_that("maive_p_from_coef returns NA for degenerate inputs", {
  expect_true(is.na(maive_p_from_coef(1, 0)))
  expect_true(is.na(maive_p_from_coef(1, -1)))
  expect_true(is.na(maive_p_from_coef(Inf, 1)))
  expect_true(is.na(maive_p_from_coef(1, NA_real_)))
})

# maive_ci ------------------------------------------------------------------

test_that("maive_ci returns the normal-approximation interval", {
  expect_equal(maive_ci(0, 1), c(-1.95996398454005, 1.95996398454005), tolerance = 1e-10)
})

test_that("maive_ci returns NAs for degenerate inputs", {
  expect_true(all(is.na(maive_ci(1, 0))))
  expect_true(all(is.na(maive_ci(NA_real_, 1))))
})

# prepare_maive_data --------------------------------------------------------

test_that("prepare_maive_data renames columns to the MAIVE contract", {
  df <- data.frame(
    effect = c(0.1, 0.2, 0.3),
    se = c(0.05, 0.06, 0.07),
    n_obs = c(100, 200, 300),
    study_id = c(1, 1, 2)
  )

  out <- prepare_maive_data(df)

  expect_equal(colnames(out), c("bs", "sebs", "Ns", "studyid"))
  expect_equal(out$bs, df$effect)
  expect_equal(out$sebs, df$se)
  expect_equal(out$Ns, df$n_obs)
  expect_equal(out$studyid, df$study_id)
})

test_that("prepare_maive_data omits studyid when study_id is absent", {
  df <- data.frame(effect = 1, se = 0.1, n_obs = 10)
  expect_equal(colnames(prepare_maive_data(df)), c("bs", "sebs", "Ns"))
})

test_that("prepare_maive_data errors when required columns are missing", {
  expect_error(prepare_maive_data(data.frame(effect = 1, se = 0.1)))
})

# Verdicts ------------------------------------------------------------------

test_that("instrument_strength splits at the conventional F of 10", {
  expect_equal(instrument_strength(10), "strong")
  expect_equal(instrument_strength(9.99), "weak")
  expect_equal(instrument_strength(NA_real_), "unknown")
  expect_equal(instrument_strength("NA"), "unknown")
})

test_that("hausman_rejects compares the statistic against its critical value", {
  expect_true(hausman_rejects(6.2, 3.841))
  expect_false(hausman_rejects(1.1, 3.841))
  expect_true(is.na(hausman_rejects(NaN, 3.841)))
  expect_true(is.na(hausman_rejects(6.2, NULL)))
})

test_that("ci_excludes_zero recognises intervals on either side of zero", {
  expect_true(ci_excludes_zero(c(0.1, 0.4)))
  expect_true(ci_excludes_zero(c(-0.4, -0.1)))
  expect_false(ci_excludes_zero(c(-0.1, 0.4)))
  expect_true(is.na(ci_excludes_zero(c(NA, 0.4))))
})

test_that("ar_ci_verdict distinguishes disabled, unusable, and very wide intervals", {
  expect_equal(ar_ci_verdict(c(0, 1), computed = FALSE)$value, "not computed")
  expect_equal(ar_ci_verdict("NA", computed = TRUE)$tone, "bad")
  expect_match(ar_ci_verdict(c(-200, 200), computed = TRUE)$note, "weak identification")
  expect_match(ar_ci_verdict(c(0, 1), computed = TRUE)$note, "weak-instrument robust")
})

test_that("maive_first_stage_spec names the regression that produced the F", {
  expect_equal(maive_first_stage_spec(0L)$spec, "SE^2 ~ 1/N")
  expect_match(maive_first_stage_spec(1L)$spec, "log N")
  expect_match(maive_first_stage_spec(1L)$f_label, "gamma1")
})

# First stage --------------------------------------------------------------

test_that("maive_first_stage_f reads MAIVE's literal \"NA\" as missing", {
  expect_equal(maive_first_stage_f(list(`F-test` = 80.935)), 80.935)
  expect_true(is.na(maive_first_stage_f(list(`F-test` = "NA"))))
  expect_true(is.na(maive_first_stage_f(list())))
  expect_true(is.na(maive_first_stage_f(list(`F-test` = c(1, 2)))))
})

test_that("maive_first_stage_is_weak applies the F < 10 rule of thumb", {
  expect_true(maive_first_stage_is_weak(9.999))
  expect_false(maive_first_stage_is_weak(10))
  expect_false(maive_first_stage_is_weak(NA_real_))
})

test_that("resolve_maive_first_stage passes explicit choices through untouched", {
  wide <- c(100, 10^9)

  # A wide N spread would trigger the log form under 2, but 0 stays 0.
  levels_choice <- resolve_maive_first_stage(0, wide)
  expect_equal(levels_choice$value, 0L)
  expect_false(levels_choice$automatic)

  log_choice <- resolve_maive_first_stage(1, c(100, 200))
  expect_equal(log_choice$value, 1L)
  expect_false(log_choice$automatic)
})

test_that("resolve_maive_first_stage picks log once N spans the threshold", {
  # The master-thesis case: 135 to 14.7M, about 5 orders of magnitude.
  res <- resolve_maive_first_stage(2, c(135, 14746755))

  expect_equal(res$value, 1L)
  expect_true(res$automatic)
  expect_equal(res$orders, 5.038, tolerance = 1e-3)
})

test_that("resolve_maive_first_stage keeps levels for a narrow N spread", {
  res <- resolve_maive_first_stage(2, c(500, 900, 4000))

  expect_equal(res$value, 0L)
  expect_true(res$automatic)
  expect_true(res$orders < 3)
})

test_that("resolve_maive_first_stage decides on the threshold boundary", {
  # Exactly 3 orders of magnitude counts as wide.
  expect_equal(resolve_maive_first_stage(2, c(10, 10000))$value, 1L)
  # Just under does not.
  expect_equal(resolve_maive_first_stage(2, c(10, 9999))$value, 0L)
})

test_that("resolve_maive_first_stage ignores unusable sample sizes", {
  # Zero, negative, and non-finite N cannot enter a ratio.
  res <- resolve_maive_first_stage(2, c(0, -5, NA, Inf, 100, 10^6))
  expect_equal(res$value, 1L)
  expect_equal(res$orders, 4)
})

test_that("resolve_maive_first_stage falls back to levels without a usable spread", {
  for (degenerate in list(numeric(0), c(NA_real_, NA_real_), 1000, c(0, -1))) {
    res <- resolve_maive_first_stage(2, degenerate)
    expect_equal(res$value, 0L)
    expect_true(res$automatic)
    expect_true(is.na(res$orders))
  }
})

test_that("resolve_maive_first_stage never reads anything but sample size", {
  expect_equal(names(formals(resolve_maive_first_stage)), c("first_stage", "n_obs"))
})

test_that("maive_method_label falls back to the raw value for unknown codes", {
  expect_equal(maive_method_label(3L), "PET-PEESE")
  expect_equal(maive_method_label(99L), "99")
})

# build_maive_summary -------------------------------------------------------

test_that("build_maive_summary returns a tidy sectioned frame with no blank rows", {
  summary <- build_maive_summary(make_maive_output(), base_maive_options())

  expect_equal(names(summary), c("Section", "Statistic", "Value", "Note"))
  expect_false(any(!nzchar(summary$Section)))
  expect_false(any(!nzchar(summary$Statistic)))
  expect_equal(
    unique(summary$Section),
    c("Specification", "Corrected mean estimate", "Publication bias and p-hacking", "Diagnostics")
  )
  expect_equal(length(attr(summary, "tone")), nrow(summary))
})

test_that("build_maive_summary echoes the specification the run used", {
  summary <- build_maive_summary(
    make_maive_output(),
    base_maive_options(method = 4L, first_stage = 1L, se = 4L)
  )
  spec <- summary[summary$Section == "Specification", ]

  expect_equal(spec$Value[spec$Statistic == "Model"], "Endogenous kink (EK)")
  expect_equal(spec$Value[spec$Statistic == "Standard errors"], "wild cluster bootstrap")
  expect_match(spec$Value[spec$Statistic == "First-stage specification"], "log N")
})

test_that("build_maive_summary records an automatically chosen first stage", {
  auto_log <- build_maive_summary(
    make_maive_output(), base_maive_options(first_stage = 2L), list(),
    list(value = 1L, automatic = TRUE, orders = 5.04)
  )
  spec_row <- auto_log[auto_log$Statistic == "First-stage specification", ]
  expect_match(spec_row$Value, "log N")
  expect_match(spec_row$Note, "chosen automatically \\(N spans 5\\.0 orders")

  # The F label follows the form actually used, not the configured one.
  expect_true("First-stage F (gamma1)" %in% auto_log$Statistic)

  no_spread <- build_maive_summary(
    make_maive_output(), base_maive_options(first_stage = 2L), list(),
    list(value = 0L, automatic = TRUE, orders = NA_real_)
  )
  expect_match(
    no_spread$Note[no_spread$Statistic == "First-stage specification"],
    "no usable spread"
  )

  # An explicit choice is not annotated.
  explicit <- build_maive_summary(make_maive_output(), base_maive_options())
  expect_equal(explicit$Note[explicit$Statistic == "First-stage specification"], "")
})

test_that("build_maive_summary flags a weak instrument and a rejecting Hausman test", {
  summary <- build_maive_summary(make_maive_output(`F-test` = 4.5), base_maive_options())

  f_row <- summary[summary$Statistic == "First-stage F", ]
  expect_match(f_row$Note, "weak instrument")
  expect_equal(attr(summary, "tone")[summary$Statistic == "First-stage F"], "bad")

  hausman_row <- summary[summary$Statistic == "Hausman statistic", ]
  expect_match(hausman_row$Note, "rejects OLS = IV")
})

test_that("build_maive_summary reports an uninformative Hausman test rather than NA", {
  summary <- build_maive_summary(make_maive_output(Hausman = NaN), base_maive_options())
  hausman_row <- summary[summary$Statistic == "Hausman statistic", ]

  expect_match(hausman_row$Note, "uninformative")
})

test_that("build_maive_summary drops IV diagnostics when instrumenting is off", {
  summary <- build_maive_summary(make_maive_output(), base_maive_options(instrument = 0L))

  expect_false("Diagnostics" %in% summary$Section)
  expect_false("First-stage specification" %in% summary$Statistic)
})

test_that("build_maive_summary drops the Hausman rows under study fixed effects", {
  summary <- build_maive_summary(make_maive_output(), base_maive_options(studylevel = 1L))

  expect_true("First-stage F" %in% summary$Statistic)
  expect_false("Hausman statistic" %in% summary$Statistic)
})

test_that("build_maive_summary marks an uncomputed Anderson-Rubin interval", {
  summary <- build_maive_summary(make_maive_output(), base_maive_options(ar = 0L))
  ar_row <- summary[summary$Statistic == "Anderson-Rubin 95% CI", ][1, ]

  expect_equal(ar_row$Value, "not computed")
})

test_that("build_maive_summary honours the significance-marks switch", {
  with_marks <- build_maive_summary(make_maive_output(), base_maive_options())
  without <- build_maive_summary(
    make_maive_output(),
    base_maive_options(add_significance_marks = FALSE)
  )

  expect_match(with_marks$Value[with_marks$Statistic == "MAIVE estimate"], "\\*")
  expect_no_match(without$Value[without$Statistic == "MAIVE estimate"], "\\*")
})

test_that("build_maive_summary survives an all-NA MAIVE result", {
  summary <- build_maive_summary(
    list(beta = NA_real_, SE = NA_real_, egger_coef = NA_real_, egger_se = NA_real_),
    base_maive_options()
  )

  expect_true(is.data.frame(summary))
  expect_equal(summary$Value[summary$Statistic == "MAIVE estimate"], "NA")
})

# build_maive_interpretation ------------------------------------------------

test_that("build_maive_interpretation reads out the effect, bias, and diagnostics", {
  text <- paste(build_maive_interpretation(make_maive_output(), base_maive_options()), collapse = " ")

  expect_match(text, "bias-corrected mean effect is 0\\.2")
  expect_match(text, "statistically different from zero")
  expect_match(text, "without the IV correction gives 0\\.31")
  expect_match(text, "find evidence of substantial publication bias or p-hacking")
  expect_match(text, "The instrument is strong")
  expect_match(text, "rejects equality of OLS and IV")
})

test_that("build_maive_interpretation recommends logs only for a low F in levels", {
  low_f <- make_maive_output(`F-test` = 4.5)

  levels_text <- paste(build_maive_interpretation(low_f, base_maive_options()), collapse = " ")
  logs_text <- paste(
    build_maive_interpretation(low_f, base_maive_options(first_stage = 1L)),
    collapse = " "
  )
  strong_text <- paste(build_maive_interpretation(make_maive_output(`F-test` = 45), base_maive_options()), collapse = " ")

  expect_match(levels_text, "running the first stage in logs")
  expect_no_match(logs_text, "running the first stage in logs")
  expect_no_match(strong_text, "running the first stage in logs")
})

test_that("build_maive_interpretation skips IV sentences when instrumenting is off", {
  text <- paste(
    build_maive_interpretation(make_maive_output(), base_maive_options(instrument = 0L)),
    collapse = " "
  )

  expect_no_match(text, "instrument is")
  expect_no_match(text, "Hausman")
  expect_match(text, "evidence of substantial publication bias \\(")
})

test_that("build_maive_interpretation reports an insignificant effect as such", {
  text <- paste(
    build_maive_interpretation(make_maive_output(beta = 0.01, SE = 0.5), base_maive_options()),
    collapse = " "
  )

  expect_match(text, "not different from zero")
})

# run_maive -----------------------------------------------------------------

test_that("run_maive skips with a reason when n_obs is absent", {
  df <- make_maive_df()
  df$n_obs <- NULL

  result <- run_maive(df, base_maive_options())

  expect_match(result$skipped, "n_obs")
  expect_true(is.null(result$summary))
})

test_that("run_maive keeps the install hint when the MAIVE package is absent", {
  local_pretend_packages_absent("MAIVE")

  result <- run_maive(make_maive_df(), base_maive_options())

  expect_true(is.null(result$summary))
  # The actionable half lives in a cli bullet, which e$message would drop.
  expect_match(result$skipped, "install.packages")
})

test_that("run_maive reports the installed version when it is too old", {
  # Without the package the absence check fires first and never reaches the
  # version branch under test.
  skip_if_not_installed("MAIVE")
  local_pretend_package_version("MAIVE", "0.0.2.11")

  result <- run_maive(make_maive_df(), base_maive_options())

  expect_true(is.null(result$summary))
  expect_match(result$skipped, "0\\.2\\.4 or higher")
  expect_match(result$skipped, "0\\.0\\.2\\.11")
})

test_that("run_maive builds a summary and interpretation from real MAIVE output", {
  skip_if_not_installed("MAIVE")
  local_options(artma.verbose = 1)

  result <- run_maive(make_maive_df(), base_maive_options())

  expect_true(is.null(result$skipped))
  expect_true(is.data.frame(result$summary))
  expect_true(length(result$interpretation) > 0)
  expect_true(is.list(result$raw))
})

test_that("run_maive resolves the first stage from the data and reports it", {
  skip_if_not_installed("MAIVE")
  local_options(artma.verbose = 1)

  df <- make_maive_df()
  # Sample sizes spanning six orders of magnitude, the case the log form exists
  # for; the SEs track them so the first stage stays estimable.
  df$n_obs <- round(seq(100, 100e6, length.out = nrow(df)))
  df$se <- 3 / sqrt(df$n_obs) + abs(rnorm(nrow(df), 0, 1e-4))

  result <- run_maive(df, base_maive_options(first_stage = 2L))

  expect_equal(result$first_stage$value, 1L)
  expect_true(result$first_stage$automatic)
})

test_that("run_maive is reproducible across two bootstrap runs", {
  skip_if_not_installed("MAIVE")
  local_options(artma.verbose = 1)
  opts <- base_maive_options(se = 3L) # Wild bootstrap: needs the threaded seed.

  # MAIVE hardcodes 999 wild-bootstrap replications with no public knob, which
  # makes the two runs below take close to half a minute. The replication count
  # is irrelevant to what this test guards (both runs seeing identical RNG
  # state), so shrink B while keeping the real bootstrap and its seeding path.
  real_boot <- utils::getFromNamespace("manual_wild_cluster_boot_se", "MAIVE")
  local_mocked_bindings(
    manual_wild_cluster_boot_se = function(model, data, cluster_var, B = 500, seed = 123) {
      real_boot(model = model, data = data, cluster_var = cluster_var, B = 25L, seed = seed)
    },
    .package = "MAIVE"
  )

  expect_equal(
    run_maive(make_maive_df(), opts)$summary,
    run_maive(make_maive_df(), opts)$summary
  )
})
