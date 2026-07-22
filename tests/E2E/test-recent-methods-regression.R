# End-to-end regression check for the analytical pipeline touched by the most
# recent run of merged PRs (#293-#304): data preprocessing (zero-SE handling),
# the linear_tests / exogeneity_tests / p_hacking_tests runtime methods,
# fork-safe parallel graphics export, and cached-rerun plot restoration.
#
# It drives the real `artma::artma()` entry point on generated meta-analysis
# data crafted to hit the edge cases those PRs changed, then asserts on the
# structural outputs (result slots, PR-specific columns, exported files). It is
# deliberately assertion-heavy rather than value-heavy: the goal is to catch a
# method that stops running, stops emitting an expected column, or stops
# writing its files, not to pin exact statistics.

# --- Install / load the package under test -----------------------------------
# Mirrors tests/E2E/test-installation.R: install the checkout so we exercise the
# installed box modules, not a dev-loaded copy.
artma_path <- dirname(dirname(getwd()))
if (!dir.exists(file.path(artma_path, "DESCRIPTION")) &&
  !file.exists(file.path(artma_path, "DESCRIPTION"))) {
  # getwd() is tests/E2E when run by scripts/testE2E.sh; fall back gracefully.
  artma_path <- normalizePath(file.path(getwd(), "..", ".."))
}
if (!file.exists(file.path(artma_path, "DESCRIPTION"))) {
  stop(sprintf("Could not locate the artma package root from %s", getwd()))
}

remotes::install_local(artma_path, force = TRUE, upgrade = "never")
library(artma) # nolint: undesirable_function_linter.

# --- Tiny assertion helpers (no testthat dependency at E2E level) ------------
failures <- character(0)
check <- function(cond, msg) {
  ok <- isTRUE(cond)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) failures[[length(failures) + 1L]] <<- msg
  invisible(ok)
}

# --- Build data that exercises the PR-specific edge cases --------------------
# create_mock_df lives in inst/artma/data/mock.R and is reachable via box once
# the package is installed. Load it through the package's own module path.
box::use(artma / data / mock[create_mock_df])

set.seed(101)
df <- create_mock_df(nrow = 240L, n_studies = 20L, seed = 101L)

# PR #304: zero standard errors must be removed-with-warning by default rather
# than aborting the run. Inject a couple of zero-SE rows.
df$se[c(5L, 90L)] <- 0

work_dir <- tempfile("artma-e2e-")
dir.create(work_dir)
data_path <- file.path(work_dir, "mock_data.csv")
utils::write.csv(df, data_path, row.names = FALSE)

options_dir <- file.path(work_dir, "options")
dir.create(options_dir)
output_dir <- file.path(work_dir, "results")
dir.create(output_dir)

# --- Create an options file pointing at the data -----------------------------
# Non-interactive + autonomous so the pipeline never prompts and takes its
# deterministic defaults (including the PR #304 "remove" zero-SE fallback).
artma::options.create(
  options_file_name = "e2e.yaml",
  options_dir = options_dir,
  user_input = list(
    "data.source_path" = data_path,
    "data.na_handling" = "remove",
    "data.reconcile_mode" = "auto",
    "autonomy.level" = "autonomous",
    "output.dir" = output_dir,
    "output.save_results" = TRUE,
    "general.export_graphics" = TRUE,
    "cache.use_cache" = TRUE,
    "verbose" = 2L
  ),
  should_validate = TRUE,
  should_overwrite = TRUE
)

# The first three are the analytical methods changed by #293-#304. The three
# plot methods are independent of each other, so the orchestrator runs them in
# parallel forks: this exercises the fork-safe graphics device (#295) and the
# base-graphics plot files it writes (which #294 must restore on a cache hit).
methods <- c(
  "linear_tests", "exogeneity_tests", "p_hacking_tests",
  "funnel_plot", "box_plot", "t_stat_histogram"
)
plot_methods <- c("funnel_plot", "box_plot", "t_stat_histogram")

run_once <- function() {
  artma::artma(
    methods = methods,
    options = "e2e.yaml",
    options_dir = options_dir
  )
}

# --- First run: full pipeline ------------------------------------------------
cat("\n===== RUN 1 (cold cache) =====\n")
res1 <- run_once()

check(is.list(res1), "artma() returns a list of results")
failed <- attr(res1, "failed_methods")
check(
  is.null(failed) || length(failed) == 0,
  sprintf(
    "no method reported failure (failed: %s)",
    paste(names(failed), collapse = ", ")
  )
)

for (m in methods) {
  check(m %in% names(res1), sprintf("%s produced a result", m))
}

# PR #303: linear_tests must emit the ci_conflict flag on its coefficients.
lt <- res1[["linear_tests"]]
if (!is.null(lt)) {
  coefs <- lt$meta$coefficients
  check(is.data.frame(coefs) && nrow(coefs) > 0, "linear_tests: coefficients table populated")
  check("ci_conflict" %in% names(coefs), "linear_tests: ci_conflict column present (PR #303)")
  check(is.data.frame(lt$tables$summary), "linear_tests: summary table present")
}

# PR #302 / #301 / #300: exogeneity_tests must run and produce its table with
# formatted columns filled even when p-uniform* skips estimation.
ex <- res1[["exogeneity_tests"]]
if (!is.null(ex)) {
  tbls <- ex$tables
  check(
    length(tbls) > 0 && all(vapply(tbls, is.data.frame, logical(1))),
    "exogeneity_tests: at least one data.frame table returned"
  )
  main <- tbls[[1]]
  check(nrow(main) > 0, "exogeneity_tests: main table has rows")
  check(
    !any(vapply(main, function(col) all(is.na(col)), logical(1))),
    "exogeneity_tests: no output column is entirely NA (PR #300/#301)"
  )
}

# PR #297 / #298 / #299: p_hacking_tests must run its caliper/Cox-Shi/MAIVE suite.
ph <- res1[["p_hacking_tests"]]
if (!is.null(ph)) {
  check(
    length(ph$tables) > 0 && all(vapply(ph$tables, is.data.frame, logical(1))),
    "p_hacking_tests: table output present"
  )
}

# The three plot methods ran in parallel forks; each should have returned a
# result and produced a plot object.
for (m in plot_methods) {
  pm <- res1[[m]]
  check(
    !is.null(pm) && length(pm$plots) > 0,
    sprintf("%s produced a plot object (parallel fork)", m)
  )
}

# PR #295: fork-safe graphics device -> plots exported to PNG under output dir.
pngs1 <- list.files(output_dir, pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
csvs1 <- list.files(output_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
check(length(csvs1) > 0, "result CSV tables exported to output dir")
check(
  length(pngs1) >= length(plot_methods),
  sprintf("plots exported to PNG from parallel forks (fork-safe device, PR #295): %d found", length(pngs1))
)

# --- Second run: warm cache --------------------------------------------------
# PR #294: cached reruns must restore base-graphics plot files even though the
# method body is memoised and not re-executed. Delete the PNGs, rerun, and
# confirm they come back.
cat("\n===== RUN 2 (warm cache, plot restoration) =====\n")
file.remove(pngs1)
res2 <- run_once()

check(is.list(res2) && all(methods %in% names(res2)), "cached rerun returns all method results")
pngs2 <- list.files(output_dir, pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
check(length(pngs2) > 0, "cached rerun restored PNG plot files (PR #294)")

# --- Summary -----------------------------------------------------------------
cat("\n================ E2E REGRESSION SUMMARY ================\n")
if (length(failures) == 0) {
  cat("All end-to-end regression checks passed.\n")
} else {
  cat(sprintf("%d check(s) FAILED:\n", length(failures)))
  for (f in failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L, save = "no")
}
