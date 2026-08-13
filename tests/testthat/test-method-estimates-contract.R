box::use(
  testthat[
    expect_equal,
    expect_length,
    expect_named,
    expect_setequal,
    expect_true,
    test_that
  ]
)

box::use(
  artma / modules / runtime_methods[ESTIMATES_COLUMNS],
  artma / paths[PATHS]
)

# Methods that produce graphics only. They have no numbers to report, so they
# leave `estimates` unset; everything else must fill it.
PLOT_ONLY_METHODS <- c(
  "box_plot",
  "funnel_plot",
  "prima_facie_graphs",
  "t_stat_histogram"
)

discovered_method_names <- function() {
  files <- list.files(PATHS$DIR_METHODS, pattern = "[.][Rr]$", full.names = FALSE)
  sort(tools::file_path_sans_ext(files))
}

# TRUE when any `new_method_result()` call in the file passes an `estimates`
# argument. Walking the parse tree, rather than grepping, keeps a method that
# merely mentions the word from passing the check.
wires_estimates <- function(path) {
  found <- FALSE

  # An empty argument (`df[i, ]`) is the empty symbol, which errors the moment
  # it is evaluated, so every child is forced behind a guard.
  is_empty_arg <- function(x) {
    tryCatch(
      {
        force(x)
        FALSE
      },
      error = function(e) TRUE
    )
  }

  walk <- function(node) {
    if (found) {
      return(invisible(NULL))
    }
    if (is.call(node)) {
      target <- node[[1L]]
      if (is.name(target) && identical(as.character(target), "new_method_result") &&
        "estimates" %in% names(node)) {
        found <<- TRUE
        return(invisible(NULL))
      }
    }
    if (is.call(node) || is.pairlist(node) || is.expression(node)) {
      for (child in as.list(node)) {
        if (is_empty_arg(child)) next
        walk(child)
      }
    }
    invisible(NULL)
  }

  walk(parse(path, keep.source = FALSE))
  found
}

test_that("the plot-only allowlist names methods that actually exist", {
  expect_true(all(PLOT_ONLY_METHODS %in% discovered_method_names()))
})

test_that("every runtime method either reports estimates or is plot-only", {
  method_names <- discovered_method_names()
  expect_true(length(method_names) > 0L)

  reporting <- setdiff(method_names, PLOT_ONLY_METHODS)
  missing <- Filter(
    function(name) !wires_estimates(file.path(PATHS$DIR_METHODS, paste0(name, ".R"))),
    reporting
  )

  # A new method that only builds a display table trips this: either fill the
  # `estimates` slot or declare the method plot-only above.
  expect_equal(as.character(missing), character())
})

# Per-method estimates builders ---------------------------------------------

expect_schema <- function(estimates, expected_method) {
  expect_named(estimates, ESTIMATES_COLUMNS)
  expect_true(is.numeric(estimates$estimate))
  expect_true(is.integer(estimates$n_obs))
  expect_true(is.character(estimates$note))
  if (nrow(estimates) > 0L) {
    expect_setequal(unique(estimates$method), expected_method)
  }
  invisible(estimates)
}

test_that("linear_tests_estimates conforms to the schema and stays unrounded", {
  box::use(artma / methods / linear_tests[linear_tests_estimates])

  estimates <- linear_tests_estimates(data.frame(
    model = c("ols", "ols"),
    term = c("effect", "publication_bias"),
    estimate = c(0.0571234, -0.1289876),
    std_error = c(0.0123456, 0.0234567),
    statistic = c(4.63, -5.5),
    p_value = c(0.0000123, 0.0000456),
    bootstrap_lower = c(0.03, -0.2),
    bootstrap_upper = c(0.08, -0.05),
    n_obs = c(120L, 120L),
    ci_conflict = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  ))

  expect_schema(estimates, "linear_tests")
  expect_equal(estimates$estimate, c(0.0571234, -0.1289876))
  expect_true(is.na(estimates$note[[1L]]))
  expect_true(nzchar(estimates$note[[2L]]))
})

test_that("nonlinear_tests_estimates maps the per-model coefficient frame", {
  box::use(artma / methods / nonlinear_tests[nonlinear_tests_estimates])

  estimates <- nonlinear_tests_estimates(data.frame(
    model = c("waap", "waap"),
    term = c("publication_bias", "effect"),
    estimate = c(0.2345678, 0.0456789),
    std_error = c(0.01, 0.02),
    p_value = c(0.01, 0.2),
    n_obs_total = c(200L, 200L),
    n_obs_model = c(84L, 84L),
    stringsAsFactors = FALSE
  ))

  expect_schema(estimates, "nonlinear_tests")
  expect_equal(estimates$n_obs, c(84L, 84L))
  expect_equal(estimates$estimate, c(0.2345678, 0.0456789))

  expect_equal(nrow(nonlinear_tests_estimates(data.frame())), 0L)
})

test_that("exogeneity_tests_estimates splits the two estimators into models", {
  box::use(artma / methods / exogeneity_tests[exogeneity_tests_estimates])

  coefficients <- function() {
    data.frame(
      term = c("effect", "publication_bias"),
      estimate = c(0.11, 0.22),
      std_error = c(0.01, 0.02),
      statistic = c(11, 11),
      p_value = c(0.001, 0.002),
      n_obs = c(50L, 50L),
      stringsAsFactors = FALSE
    )
  }

  estimates <- exogeneity_tests_estimates(list(
    iv = list(coefficients = coefficients(), weak_instrument = TRUE),
    puniform = list(coefficients = coefficients(), note = "did not converge")
  ))

  expect_schema(estimates, "exogeneity_tests")
  expect_equal(nrow(estimates), 4L)
  expect_setequal(unique(estimates$model), c("iv", "puniform"))
  expect_true(all(grepl("weak", estimates$note[estimates$model == "iv"])))
  expect_true(all(estimates$note[estimates$model == "puniform"] == "did not converge"))

  expect_equal(nrow(exogeneity_tests_estimates(list())), 0L)
})

test_that("p_hacking_tests_estimates maps thresholds to models and widths to terms", {
  box::use(artma / methods / p_hacking_tests[p_hacking_tests_estimates])

  estimates <- p_hacking_tests_estimates(list(
    caliper_results = list(
      list(
        threshold = 1.96, width = 0.05, share_above = 0.6666667, p_value = 0.0412345,
        n_above = 6, n_below = 3, n_studies = 4L, direction = "above",
        tail = "positive", cluster_method = "study"
      )
    ),
    elliott_results = list(
      binomial_005 = list(
        test = "Binomial [0, 0.05]", family = "binomial",
        support = "[0, 0.05]", p_value = 0.0123456
      ),
      lcm_005 = list(
        test = "LCM [0, 0.05]", family = "lcm", support = "[0, 0.05]",
        p_value = structure(NA_real_, reason = "package 'fdrtool' is not installed")
      )
    )
  ))

  expect_schema(estimates, "p_hacking_tests")
  expect_equal(nrow(estimates), 3L)

  caliper <- estimates[estimates$model == "threshold_1.96", ]
  expect_equal(caliper$term, "width_0.05")
  expect_equal(caliper$estimate, 0.6666667)
  expect_equal(caliper$p_value, 0.0412345)
  expect_equal(caliper$n_obs, 9L)
  expect_equal(caliper$n_clusters, 4L)

  lcm <- estimates[estimates$model == "lcm", ]
  expect_true(is.na(lcm$p_value))
  expect_true(grepl("fdrtool", lcm$note))

  expect_equal(nrow(p_hacking_tests_estimates(list())), 0L)
})

test_that("variable_summary_stats_estimates reports missingness as a proportion", {
  box::use(artma / methods / variable_summary_stats[variable_summary_stats_estimates])

  estimates <- variable_summary_stats_estimates(list(
    list(
      label = "Study year", mean = 2005.4567, median = 2006, min = 1998,
      max = 2019, sd = 4.98765, missing_share = 0.125, obs = 56L
    )
  ))

  expect_schema(estimates, "variable_summary_stats")
  expect_equal(estimates$model, rep("Study year", 6L))
  expect_setequal(
    estimates$term,
    c("mean", "median", "min", "max", "sd", "missing_share")
  )
  expect_equal(estimates$estimate[estimates$term == "missing_share"], 0.125)
  expect_equal(estimates$estimate[estimates$term == "mean"], 2005.4567)

  expect_equal(nrow(variable_summary_stats_estimates(list())), 0L)
})

test_that("effect_summary_stats_estimates keeps each mean with its own interval", {
  box::use(artma / methods / effect_summary_stats[effect_summary_stats_estimates])

  estimates <- effect_summary_stats_estimates(list(
    `All Data` = list(
      label = "All Data",
      unweighted = list(
        mean = 0.1234567, sd = 0.5, ci = c(0.05, 0.19),
        median = 0.1, min = -1.2, max = 2.4, obs = 300L
      ),
      weighted = list(mean = 0.0987654, ci = c(0.02, 0.18))
    )
  ))

  expect_schema(estimates, "effect_summary_stats")
  expect_equal(nrow(estimates), 6L)
  expect_equal(estimates$estimate[estimates$term == "mean"], 0.1234567)
  expect_equal(estimates$conf_low[estimates$term == "weighted_mean"], 0.02)
  expect_true(all(is.na(estimates$conf_low[estimates$term %in% c("median", "min", "max", "sd")])))
  expect_equal(unique(estimates$n_obs), 300L)

  expect_equal(nrow(effect_summary_stats_estimates(list())), 0L)
})

test_that("bma_estimates puts the posterior inclusion probability in statistic", {
  box::use(artma / methods / bma[bma_estimates])

  estimates <- bma_estimates(
    data.frame(
      variable = c("Intercept", "Precision"),
      pip = c(0.5512345, 0.9987654),
      post_mean = c(0.0123456, -0.2034567),
      post_sd = c(0.05, 0.023),
      cond_pos_sign = c(0.6, 0),
      stringsAsFactors = FALSE
    ),
    n_obs = 412L
  )

  expect_schema(estimates, "bma")
  expect_equal(estimates$statistic, c(0.5512345, 0.9987654))
  expect_equal(estimates$estimate, c(0.0123456, -0.2034567))
  expect_equal(unique(estimates$n_obs), 412L)
  expect_true(all(grepl("PIP", estimates$note)))
})

test_that("fma_estimates fills n_clusters only when the SEs are clustered", {
  box::use(artma / methods / fma[fma_estimates])

  coefficients <- data.frame(
    variable = c("Intercept", "Precision"),
    coefficient = c(-3.6741234, 0.2881234),
    se = c(0.888, 0.011),
    p_value = c(0.0012345, 0.0000123),
    stringsAsFactors = FALSE
  )

  clustered <- fma_estimates(coefficients, n_obs = 412L, n_clusters = 37L)
  expect_schema(clustered, "fma")
  expect_equal(unique(clustered$n_clusters), 37L)
  expect_equal(clustered$estimate, c(-3.6741234, 0.2881234))

  iid <- fma_estimates(coefficients, n_obs = 412L)
  expect_true(all(is.na(iid$n_clusters)))
})

test_that("robma_estimates carries the credible interval bounds", {
  box::use(artma / methods / robma[robma_estimates])

  tbl <- data.frame(
    Mean = c(0.1234567, 0.0456789),
    Median = c(0.12, 0.04),
    `0.025` = c(0.01, 0.001),
    `0.975` = c(0.25, 0.09),
    check.names = FALSE
  )
  rownames(tbl) <- c("mu", "tau")

  estimates <- robma_estimates(tbl, n_obs = 88L)

  expect_schema(estimates, "robma")
  expect_equal(estimates$term, c("mu", "tau"))
  expect_equal(estimates$estimate, c(0.1234567, 0.0456789))
  expect_equal(estimates$conf_low, c(0.01, 0.001))
  expect_equal(estimates$conf_high, c(0.25, 0.09))

  expect_equal(nrow(robma_estimates(NULL)), 0L)
})

test_that("maive_estimates splits the corrected and unadjusted models", {
  box::use(artma / methods / maive[maive_estimates])

  estimates <- maive_estimates(
    list(
      beta = 0.1234567,
      SE = 0.0456789,
      egger_coef = 1.2345678,
      egger_se = 0.3456789,
      `pub bias p-value` = 0.0004321,
      beta_standard = 0.2345678,
      SE_standard = 0.0345678,
      `F-test` = 24.6813579
    ),
    n_obs = 150L
  )

  expect_schema(estimates, "maive")
  expect_setequal(unique(estimates$model), c("maive", "unadjusted"))

  effect <- estimates[estimates$model == "maive" & estimates$term == "effect", ]
  expect_equal(effect$estimate, 0.1234567)
  expect_true(effect$conf_low < effect$estimate && effect$conf_high > effect$estimate)

  f_row <- estimates[estimates$term == "first_stage_f", ]
  expect_length(f_row$statistic, 1L)
  expect_equal(f_row$statistic, 24.6813579)
  expect_true(is.na(f_row$estimate))

  expect_equal(nrow(maive_estimates(NULL)), 0L)
})

test_that("bpe_estimates maps each scope to a model", {
  box::use(artma / methods / best_practice_estimate[bpe_estimates])

  estimates <- bpe_estimates(
    data.frame(
      scope = c("author", "study"),
      study_id = c(NA_character_, "s1"),
      study_label = c("Author", "Smith (2019)"),
      estimate = c(0.1234567, 0.2345678),
      standard_error = c(0.0123456, 0.0234567),
      ci_lower = c(0.09, 0.19),
      ci_upper = c(0.16, 0.28),
      stringsAsFactors = FALSE
    ),
    n_clusters = 42L
  )

  expect_schema(estimates, "best_practice_estimate")
  expect_equal(estimates$model, c("author", "study"))
  expect_equal(estimates$term, c("Author", "Smith (2019)"))
  expect_equal(estimates$estimate, c(0.1234567, 0.2345678))
  expect_equal(unique(estimates$n_clusters), 42L)

  expect_equal(nrow(bpe_estimates(data.frame())), 0L)
})

# Export round trip ---------------------------------------------------------

# One representative frame per reporting method, built by that method's own
# estimates builder. Every fixture value carries seven decimals, so a rounded
# export shows up as a changed number rather than a formatting nicety.
sample_estimates <- function() {
  box::use(
    artma / methods / best_practice_estimate[bpe_estimates],
    artma / methods / bma[bma_estimates],
    artma / methods / effect_summary_stats[effect_summary_stats_estimates],
    artma / methods / exogeneity_tests[exogeneity_tests_estimates],
    artma / methods / fma[fma_estimates],
    artma / methods / linear_tests[linear_tests_estimates],
    artma / methods / maive[maive_estimates],
    artma / methods / nonlinear_tests[nonlinear_tests_estimates],
    artma / methods / p_hacking_tests[p_hacking_tests_estimates],
    artma / methods / robma[robma_estimates],
    artma / methods / variable_summary_stats[variable_summary_stats_estimates]
  )

  robma_table <- data.frame(
    Mean = 0.1234567, Median = 0.12, `0.025` = 0.0123456, `0.975` = 0.2534567,
    check.names = FALSE
  )
  rownames(robma_table) <- "mu"

  list(
    best_practice_estimate = bpe_estimates(data.frame(
      scope = "author", study_id = NA_character_, study_label = "Author",
      estimate = 0.1234567, standard_error = 0.0123456,
      ci_lower = 0.0912345, ci_upper = 0.1612345, stringsAsFactors = FALSE
    )),
    bma = bma_estimates(data.frame(
      variable = "Precision", pip = 0.9987654, post_mean = -0.2034567,
      post_sd = 0.0234567, stringsAsFactors = FALSE
    )),
    effect_summary_stats = effect_summary_stats_estimates(list(list(
      label = "All Data",
      unweighted = list(
        mean = 0.1234567, sd = 0.5123456, ci = c(0.0512345, 0.1912345),
        median = 0.1023456, min = -1.2034567, max = 2.4056789, obs = 300L
      ),
      weighted = list(mean = 0.0987654, ci = c(0.0212345, 0.1812345))
    ))),
    exogeneity_tests = exogeneity_tests_estimates(list(
      iv = list(
        coefficients = data.frame(
          term = "effect", estimate = 0.1123456, std_error = 0.0212345,
          statistic = 5.2876543, p_value = 0.0012345, n_obs = 50L,
          stringsAsFactors = FALSE
        ),
        weak_instrument = FALSE
      )
    )),
    fma = fma_estimates(data.frame(
      variable = "Precision", coefficient = 0.2881234, se = 0.0112345,
      p_value = 0.0000123, stringsAsFactors = FALSE
    )),
    linear_tests = linear_tests_estimates(data.frame(
      model = "ols", term = "effect", estimate = 0.0571234,
      std_error = 0.0123456, statistic = 4.6271234, p_value = 0.0000123,
      bootstrap_lower = 0.0312345, bootstrap_upper = 0.0812345,
      n_obs = 120L, ci_conflict = FALSE, stringsAsFactors = FALSE
    )),
    maive = maive_estimates(list(
      beta = 0.1234567, SE = 0.0456789, egger_coef = 1.2345678,
      egger_se = 0.3456789, `pub bias p-value` = 0.0004321
    )),
    nonlinear_tests = nonlinear_tests_estimates(data.frame(
      model = "waap", term = "effect", estimate = 0.0456789,
      std_error = 0.0212345, p_value = 0.2012345, n_obs_model = 84L,
      stringsAsFactors = FALSE
    )),
    p_hacking_tests = p_hacking_tests_estimates(list(caliper_results = list(list(
      threshold = 1.96, width = 0.05, share_above = 0.6666667,
      p_value = 0.0412345, n_above = 6, n_below = 3, n_studies = 4L,
      direction = "above", tail = "positive", cluster_method = "study"
    )))),
    robma = robma_estimates(robma_table),
    variable_summary_stats = variable_summary_stats_estimates(list(list(
      label = "Study year", mean = 2005.4567891, median = 2006, min = 1998,
      max = 2019, sd = 4.9876543, missing_share = 0.1256789, obs = 56L
    )))
  )
}

test_that("every method's exported estimates CSV parses as unrounded numerics", {
  box::use(
    artma / output / export[ensure_output_dirs, export_results],
    withr[local_options, local_tempdir]
  )

  dir <- local_tempdir()
  local_options(
    artma.output.dir = dir,
    artma.output.number_of_decimals = 3,
    artma.output.table_formats = "csv",
    artma.visualization.export_path = "graphics",
    artma.verbose = 1
  )
  ensure_output_dirs(dir)

  frames <- sample_estimates()
  results <- lapply(frames, function(estimates) list(tables = list(), estimates = estimates))
  export_results(results, dir)

  for (method_name in names(frames)) {
    path <- file.path(dir, "tables", paste0(method_name, ".csv"))
    expect_true(file.exists(path))

    written <- utils::read.csv(path, stringsAsFactors = FALSE)
    for (column in c("estimate", "std_error", "statistic", "p_value", "conf_low", "conf_high")) {
      values <- written[[column]]
      expect_true(is.numeric(values) || all(is.na(values)))
    }

    # Every fixture carries more precision than the display rounding, so a
    # value that survives rounding unchanged means the method pre-rounded it.
    numbers <- unlist(written[c("estimate", "std_error", "p_value")])
    numbers <- numbers[is.finite(numbers) & numbers != 0]
    expect_true(any(numbers != round(numbers, 3)))
  }
})
