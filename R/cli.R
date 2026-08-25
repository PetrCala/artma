# nolint start: box_usage_linter.

#' @title Run the artma command-line interface
#' @description
#' Scriptable entry point behind the `artma` launcher and
#' `Rscript -e 'artma::cli_run()'`. It parses an argument vector, dispatches to
#' the matching public function (`artma()`, `methods_list()`,
#' `options_validate()` / `options_create()` / `options_list()`, or the package
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
#' `methods_skipped` with reasons, `output_dir`, `exported_files`, `seed`,
#' `package_version`), read back from the `run.json` the run wrote; all
#' human-readable output is routed to stderr.
#'
#' @param args *\[character, optional\]* The argument vector to parse. Defaults
#'   to `commandArgs(trailingOnly = TRUE)`.
#' @return *\[integer\]* The exit code (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # List available methods
#' artma::cli_run("methods")
#'
#' # Run two methods against an options file, emitting a JSON manifest
#' artma::cli_run(c(
#'   "run", "--options", "my_analysis.yaml",
#'   "--methods", "funnel_plot,effect_summary_stats", "--json"
#' ))
#' }
cli_run <- function(args = commandArgs(trailingOnly = TRUE)) {
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
#' `Rscript -e 'artma::cli_run()' run --options my_analysis.yaml`.
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
#' artma::cli_install()
#' artma::cli_install(dir = "~/bin", force = TRUE)
#' }
cli_install <- function(dir = "~/.local/bin", force = FALSE) {
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
    box::use(artma / interactive / input[ask_yes_no])
    if (!ask_yes_no(cli::format_inline("Install the artma CLI launcher to {.path {target}}?"))) {
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
#'   API. Kept separate from [cli_run()] so the top-level error boundary and
#'   exit-code contract stay small and testable.
#' @param parsed *\[list\]* A parse result with `action == "dispatch"`.
#' @return `NULL` (invisible)
#' @keywords internal
cli_dispatch <- function(parsed) {
  switch(parsed$subcommand,
    run = cli_dispatch_run(parsed$flags),
    methods = methods_list(), # nolint: box_usage_linter. Package function (R/methods.R)
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
    validate = options_validate( # nolint: box_usage_linter. Package function (R/options.R)
      options_file_name = options_name,
      options_dir = options_dir
    ),
    create = options_create( # nolint: box_usage_linter. Package function (R/options.R)
      options_file_name = options_name,
      options_dir = options_dir
    ),
    list = {
      files <- options_list(options_dir = options_dir) # nolint: box_usage_linter. Package function (R/options.R)
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
#'   reasons), the resolved output directory, the files exported there, the
#'   effective seed and the package version.
#'
#'   The run's own `run.json` is the source of truth, so the CLI reports the
#'   same run identity as the R API rather than whatever has accumulated in the
#'   output directory. A run that wrote no manifest (`output.save_results` off,
#'   or `jsonlite` unavailable) falls back to what the results object carries,
#'   with an empty file list.
#' @param results *\[list\]* The value returned by [artma()].
#' @return *\[list\]* The manifest, ready for `jsonlite::toJSON()`.
#' @keywords internal
cli_build_run_manifest <- function(results) {
  box::use(
    artma / output / run_manifest[manifest_files, read_run_manifest]
  )

  output_dir <- read_last_export_dir() # nolint: box_usage_linter. Package function (R/results.R)
  run_manifest <- read_run_manifest(output_dir)

  as_reason_map <- function(x) {
    if (length(x) == 0L) {
      return(stats::setNames(list(), character(0)))
    }
    as.list(x)
  }

  # I() keeps the list-valued fields arrays in the emitted JSON even when they
  # hold a single element, so a consumer never has to handle both shapes.
  if (!is.null(run_manifest)) {
    return(list(
      methods_run = I(as.character(run_manifest$methods_run)),
      methods_skipped = as_reason_map(c(
        as.list(run_manifest$methods_skipped), as.list(run_manifest$methods_failed)
      )),
      output_dir = run_manifest$output_dir %||% output_dir,
      exported_files = I(manifest_files(run_manifest)),
      seed = run_manifest$seed,
      package_version = as.character(run_manifest$artma_version)
    ))
  }

  list(
    methods_run = I(as.character(setdiff(names(results), "ma_table"))),
    methods_skipped = as_reason_map(c(
      as.list(attr(results, "skipped_methods")), as.list(attr(results, "failed_methods"))
    )),
    output_dir = output_dir,
    exported_files = I(character(0)),
    seed = attr(results, "run_info")$seed,
    package_version = as.character(utils::packageVersion("artma"))
  )
}

# nolint end: box_usage_linter.

# Deprecated dotted aliases (see contributingGuides/API.md).

#' @rdname artma-deprecated
#' @export
cli.install <- function(...) {
  lifecycle::deprecate_warn("0.4.0", "cli.install()", "cli_install()")
  cli_install(...)
}

# PERMANENT alias, unlike the others: launchers installed by cli.install()
# before 0.4.0 call artma::cli.run() from a script on the user's PATH, and a
# removed or warning alias would break or pollute every such installation on
# package upgrade. Forwards silently; never remove.

#' @rdname artma-deprecated
#' @export
cli.run <- function(...) {
  cli_run(...)
}
