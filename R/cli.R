# nolint start: box_usage_linter.

#' @title Run the artma command-line interface
#' @description
#' Scriptable entry point behind the `artma` launcher and
#' `Rscript -e 'artma::cli.run()'`. It parses an argument vector, dispatches to
#' the matching public function (`artma()`, `methods.list()`,
#' `options.validate()` / `options.create()` / `options.list()`, or the package
#' version) and returns an exit code. No analysis logic lives here: each
#' subcommand is a thin translation to the existing API.
#'
#' The function never calls `quit()`; it returns the exit code invisibly so it
#' can be driven in-process by tests. The launcher script turns that code into a
#' process exit status.
#'
#' Exit codes:
#' - `0` success (including `--help`).
#' - `1` an R error was raised while dispatching (message printed to stderr).
#' - `2` a usage error (unknown command, unknown flag, malformed value); usage
#'   is printed to stderr.
#'
#' Subcommands: `run`, `methods`, `options` (with sub-actions `validate`,
#' `create`, `list`) and `version`. Every subcommand accepts `--help`.
#'
#' Flags for `run` become an in-session `options()` overlay applied around the
#' `artma()` call, so the user's YAML options file is never mutated:
#' `--data` sets `artma.data.source_path`, `--output-dir` sets
#' `artma.output.dir`, `--verbose` sets `artma.verbose`, `--no-cache` sets
#' `artma.cache.use_cache` to `FALSE`, and `--report` sets
#' `artma.output.report` to `TRUE`. `--options`, `--options-dir` and
#' `--methods` are forwarded as `artma()` arguments.
#'
#' In `--json` mode stdout carries only a JSON run manifest (`methods_run`,
#' `methods_skipped` with reasons, `output_dir`, `exported_files`,
#' `package_version`); all human-readable output is routed to stderr.
#'
#' @param args *\[character, optional\]* The argument vector to parse. Defaults
#'   to `commandArgs(trailingOnly = TRUE)`.
#' @return *\[integer\]* The exit code (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # List available methods
#' artma::cli.run("methods")
#'
#' # Run two methods against an options file, emitting a JSON manifest
#' artma::cli.run(c(
#'   "run", "--options", "my_analysis.yaml",
#'   "--methods", "funnel_plot,effect_summary_stats", "--json"
#' ))
#' }
cli.run <- function(args = commandArgs(trailingOnly = TRUE)) {
  box::use(
    artma / cli / parser[cli_parse, cli_help_text]
  )

  parsed <- cli_parse(args)

  if (identical(parsed$action, "error")) {
    cli_emit_to_stderr(c(parsed$message, "", cli_help_text(parsed$subcommand)))
    return(invisible(2L))
  }

  if (identical(parsed$action, "help")) {
    writeLines(cli_help_text(parsed$subcommand), con = stdout())
    return(invisible(0L))
  }

  status <- tryCatch(
    {
      cli_dispatch(parsed)
      0L
    },
    error = function(e) {
      cli_emit_to_stderr(conditionMessage(e))
      1L
    }
  )

  invisible(status)
}

#' @title Install the artma CLI launcher
#' @description
#' Copy the launcher shipped inside the installed package to a directory on your
#' `PATH`, so `artma <command>` works from any shell. The launcher relies solely
#' on the installed package; it resolves nothing from its own location.
#'
#' The no-install alternative is to call the CLI through `Rscript` directly:
#' `Rscript -e 'artma::cli.run()' run --options my_analysis.yaml`.
#'
#' In interactive sessions the copy is confirmed via a menu. In non-interactive
#' sessions it requires `force = TRUE` and otherwise aborts. It never writes
#' inside `R.home()` and never elevates privileges.
#'
#' @param dir *\[character, optional\]* Directory to install the launcher into.
#'   Defaults to `"~/.local/bin"`.
#' @param force *\[logical, optional\]* Required to install in a non-interactive
#'   session. Ignored (the menu governs) when interactive. Defaults to `FALSE`.
#' @return *\[character\]* Path to the installed launcher (invisibly).
#' @export
#' @examples
#' \dontrun{
#' artma::cli.install()
#' artma::cli.install(dir = "~/bin", force = TRUE)
#' }
cli.install <- function(dir = "~/.local/bin", force = FALSE) {
  launcher <- system.file("cli", "artma", package = "artma")
  if (!nzchar(launcher) || !file.exists(launcher)) {
    cli::cli_abort(c(
      "x" = "Could not locate the artma CLI launcher in the installed package.",
      "i" = "Reinstall {.pkg artma} and try again."
    ))
  }

  dir <- path.expand(dir)
  normalized_dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  r_home <- normalizePath(R.home(), winslash = "/", mustWork = FALSE)
  if (identical(normalized_dir, r_home) ||
    startsWith(paste0(normalized_dir, "/"), paste0(r_home, "/"))) {
    cli::cli_abort(c(
      "x" = "Refusing to install the launcher inside the R installation directory.",
      "i" = "Choose a directory on your {.envvar PATH} such as {.path ~/.local/bin}."
    ))
  }

  target <- file.path(dir, "artma")

  if (interactive()) {
    choice <- climenu::select(
      choices = c("Yes", "No"),
      prompt = cli::format_inline("Install the artma CLI launcher to {.path {target}}?")
    )
    if (!identical(choice, "Yes")) {
      cli::cli_abort("Aborting CLI launcher installation.")
    }
  } else if (!isTRUE(force)) {
    cli::cli_abort(c(
      "x" = "Refusing to install the launcher in a non-interactive session.",
      "i" = "Re-run with {.code force = TRUE} to install to {.path {target}}."
    ))
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(launcher, target, overwrite = TRUE)) {
    cli::cli_abort("Failed to copy the launcher to {.path {target}}.")
  }
  Sys.chmod(target, mode = "0755")

  if (getOption("artma.verbose", 3) >= 3) {
    cli::cli_alert_success("Installed the artma CLI launcher to {.path {target}}.")
    cli::cli_alert_info("Ensure {.path {dir}} is on your {.envvar PATH}.")
  }

  invisible(target)
}

#' @title Emit lines to stderr
#' @description Route CLI usage and error text to stderr, keeping stdout clean
#'   for machine-readable output (the JSON manifest in `--json` mode).
#' @param lines *\[character\]* Lines to print.
#' @return `NULL` (invisible)
#' @keywords internal
cli_emit_to_stderr <- function(lines) {
  writeLines(lines, con = stderr())
  invisible(NULL)
}

#' @title Dispatch a parsed CLI invocation
#' @description Translate a `dispatch` parse result into a call on the public
#'   API. Kept separate from [cli.run()] so the top-level error boundary and
#'   exit-code contract stay small and testable.
#' @param parsed *\[list\]* A parse result with `action == "dispatch"`.
#' @return `NULL` (invisible)
#' @keywords internal
cli_dispatch <- function(parsed) {
  switch(parsed$subcommand,
    run = cli_dispatch_run(parsed$flags),
    methods = methods.list(), # nolint: box_usage_linter. Package function (R/methods.R)
    version = writeLines(as.character(utils::packageVersion("artma")), con = stdout()),
    options = cli_dispatch_options(parsed$subaction, parsed$flags),
    cli::cli_abort("Unhandled subcommand: {parsed$subcommand}.")
  )
  invisible(NULL)
}

#' @title Dispatch the `options` subcommand
#' @param subaction *\[character\]* One of `validate`, `create`, `list`.
#' @param flags *\[list\]* Parsed flags.
#' @return `NULL` (invisible)
#' @keywords internal
cli_dispatch_options <- function(subaction, flags) {
  options_name <- flags[["options"]]
  options_dir <- flags[["options-dir"]]

  switch(subaction,
    validate = options.validate( # nolint: box_usage_linter. Package function (R/options.R)
      options_file_name = options_name,
      options_dir = options_dir
    ),
    create = options.create( # nolint: box_usage_linter. Package function (R/options.R)
      options_file_name = options_name,
      options_dir = options_dir
    ),
    list = {
      files <- options.list(options_dir = options_dir) # nolint: box_usage_linter. Package function (R/options.R)
      if (length(files) > 0L) {
        writeLines(files, con = stdout())
      }
    },
    cli::cli_abort("Unhandled options sub-action: {subaction}.")
  )
  invisible(NULL)
}

#' @title Dispatch the `run` subcommand
#' @description Apply the flag-derived options overlay around an [artma()] call.
#'   In `--json` mode stray stdout from the run is redirected to stderr so the
#'   manifest is the only thing on stdout.
#' @param flags *\[list\]* Parsed `run` flags.
#' @return `NULL` (invisible)
#' @keywords internal
cli_dispatch_run <- function(flags) {
  box::use(
    artma / cli / parser[build_option_overlay]
  )

  overlay <- build_option_overlay(flags)
  json_mode <- isTRUE(flags[["json"]])

  run_artma <- function() {
    # The overlay is carried in a dedicated session option so it survives
    # artma()'s own option-loading step (which would otherwise reload the file
    # values on top of the overlay). runtime_setup re-applies it after loading.
    withr::with_options(
      c(overlay, list(artma.temp.cli_overrides = overlay)),
      artma( # nolint: box_usage_linter. Package function (R/artma.R)
        methods = flags[["methods"]],
        options = flags[["options"]],
        options_dir = flags[["options-dir"]]
      )
    )
  }

  if (!json_mode) {
    run_artma()
    return(invisible(NULL))
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "The {.pkg jsonlite} package is required for {.code --json} output.",
      "i" = "Install it with {.code install.packages(\"jsonlite\")}."
    ))
  }

  # Keep stdout clean for the manifest: any stray stdout the run emits is
  # redirected to stderr for the duration of the call.
  results <- withr::with_output_sink(stderr(), run_artma())

  manifest <- cli_build_run_manifest(results)
  writeLines(
    jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    con = stdout()
  )

  invisible(NULL)
}

#' @title Build the JSON run manifest
#' @description Summarise an [artma()] result into the manifest emitted in
#'   `--json` mode: methods that ran, methods that were skipped or failed (with
#'   reasons), the resolved output directory, the files exported there, and the
#'   package version.
#' @param results *\[list\]* The value returned by [artma()].
#' @return *\[list\]* The manifest, ready for `jsonlite::toJSON()`.
#' @keywords internal
cli_build_run_manifest <- function(results) {
  methods_run <- setdiff(names(results), "ma_table")

  skipped <- attr(results, "skipped_methods")
  failed <- attr(results, "failed_methods")
  methods_skipped <- c(as.list(skipped), as.list(failed))
  if (length(methods_skipped) == 0L) {
    methods_skipped <- stats::setNames(list(), character(0))
  }

  output_dir <- read_last_export_dir() # nolint: box_usage_linter. Package function (R/results.R)
  exported_files <- character(0)
  if (!is.null(output_dir) && dir.exists(output_dir)) {
    exported_files <- list.files(output_dir, recursive = TRUE, full.names = TRUE)
  }

  list(
    methods_run = as.character(methods_run),
    methods_skipped = methods_skipped,
    output_dir = output_dir,
    exported_files = as.character(exported_files),
    package_version = as.character(utils::packageVersion("artma"))
  )
}

# nolint end: box_usage_linter.
