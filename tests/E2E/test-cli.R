# End-to-end smoke test for the command-line interface (E3). It installs the
# package, then drives the CLI as a real subprocess (Rscript) over the committed
# fixture dataset with two cheap methods and --json, asserting:
#   * the process exits 0,
#   * stdout parses as JSON carrying the expected manifest fields,
#   * human-readable output lands on stderr, not stdout.
# It doubles as a regression check on the non-interactive multi-method run path.

artma_path <- dirname(dirname(getwd()))

if (!dir.exists(artma_path)) {
  cli::cli_abort("The path to the artma package is not valid: {artma_path}")
}

remotes::install_local(artma_path, force = TRUE)
library(artma) # nolint: undesirable_function_linter.

fixture_path <- normalizePath(file.path("fixtures", "smoke_data.csv"), mustWork = TRUE)

options_dir <- tempfile("artma-cli-options-")
dir.create(options_dir)
output_dir <- tempfile("artma-cli-output-")
dir.create(output_dir)

artma::options.create(
  options_file_name = "cli.yaml",
  options_dir = options_dir,
  user_input = list(
    data = list(source_path = fixture_path),
    output = list(dir = output_dir, save_results = TRUE)
  )
)

stdout_file <- tempfile("artma-cli-stdout-", fileext = ".txt")
stderr_file <- tempfile("artma-cli-stderr-", fileext = ".txt")

cli_args <- c(
  "-e", "quit(save = 'no', status = artma::cli.run())",
  "run",
  "--options", "cli.yaml",
  "--options-dir", options_dir,
  "--methods", "funnel_plot,effect_summary_stats",
  "--json"
)

status <- system2("Rscript", cli_args, stdout = stdout_file, stderr = stderr_file)

stdout_txt <- paste(readLines(stdout_file, warn = FALSE), collapse = "\n")
stderr_txt <- paste(readLines(stderr_file, warn = FALSE), collapse = "\n")

if (!identical(as.integer(status), 0L)) {
  cli::cli_abort(c(
    "CLI run exited with status {status}.",
    "stderr: {stderr_txt}"
  ))
}

manifest <- tryCatch(
  jsonlite::fromJSON(stdout_txt, simplifyVector = TRUE),
  error = function(e) {
    cli::cli_abort(c(
      "CLI --json stdout did not parse as JSON: {conditionMessage(e)}",
      "stdout was: {stdout_txt}"
    ))
  }
)

expected_fields <- c(
  "methods_run", "methods_skipped", "output_dir",
  "exported_files", "package_version"
)
missing_fields <- setdiff(expected_fields, names(manifest))
if (length(missing_fields) > 0) {
  cli::cli_abort("Manifest is missing fields: {paste(missing_fields, collapse = ', ')}")
}

if (!("funnel_plot" %in% manifest$methods_run)) {
  cli::cli_abort("Expected 'funnel_plot' in methods_run, got: {paste(manifest$methods_run, collapse = ', ')}")
}

if (!nzchar(stderr_txt)) {
  cli::cli_abort("Expected human-readable output on stderr, but stderr was empty.")
}

cli::cli_alert_success("CLI E2E test passed: --json manifest and stream separation verified.")
