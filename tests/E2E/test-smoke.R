# Cut-down E2E smoke test intended for the PR-gating workflow (see
# .github/workflows/R-CMD-check.yaml). Unlike test-installation.R, which only
# exercises interactive install ergonomics, this runs a real (tiny, committed)
# fixture dataset through two representative runtime methods end to end and
# asserts that neither one failed.

artma_path <- dirname(dirname(getwd()))

if (!dir.exists(artma_path)) {
  cli::cli_abort("The path to the artma package is not valid: {artma_path}")
}

remotes::install_local(artma_path, force = TRUE)
library(artma) # nolint: undesirable_function_linter.

fixture_path <- normalizePath(file.path("fixtures", "smoke_data.csv"), mustWork = TRUE)

options_dir <- tempfile("artma-smoke-options-")
dir.create(options_dir)
output_dir <- tempfile("artma-smoke-output-")
dir.create(output_dir)

artma::options.create(
  options_file_name = "smoke.yaml",
  options_dir = options_dir,
  user_input = list(
    data = list(source_path = fixture_path),
    output = list(dir = output_dir, save_results = TRUE)
  )
)

smoke_methods <- c("funnel_plot", "effect_summary_stats")

result <- artma::artma(
  methods = smoke_methods,
  options = "smoke.yaml",
  options_dir = options_dir
)

failed_methods <- attr(result, "failed_methods")
if (!is.null(failed_methods) && length(failed_methods) > 0) {
  details <- paste(names(failed_methods), unlist(failed_methods), sep = ": ", collapse = "; ")
  cli::cli_abort("Smoke run reported failed methods: {details}")
}

missing_results <- setdiff(smoke_methods, names(result))
if (length(missing_results) > 0) {
  cli::cli_abort("Smoke run did not return results for: {missing_results}")
}

cli::cli_alert_success("Smoke test passed: {paste(smoke_methods, collapse = ', ')} completed successfully.")
