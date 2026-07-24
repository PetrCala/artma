# Hand-rolled argument parser for the artma command-line interface. Pure and
# dependency-free on purpose: it never prompts, never calls into the package
# API, and never aborts. A single spec table (`cli_flag_spec()`) drives both the
# parser and the generated `--help` text, so the two can never drift.

#' @title CLI subcommand definitions
#' @description The subcommands the CLI accepts, each with a one-line help
#'   string and, where relevant, the sub-actions it requires (only `options`
#'   has any).
#' @return *\[list\]* Named list keyed by subcommand.
#' @keywords internal
cli_subcommands <- function() {
  list(
    run = list(
      help = "Run a meta-analysis over a dataset.",
      subactions = NULL
    ),
    methods = list(
      help = "List the available runtime methods.",
      subactions = NULL
    ),
    options = list(
      help = "Manage user options files.",
      subactions = c("validate", "create", "list")
    ),
    version = list(
      help = "Print the installed artma version.",
      subactions = NULL
    )
  )
}

#' @title CLI flag specification
#' @description The single source of truth for every long flag: its name, value
#'   type, help text and the subcommands that accept it. Consumed by both the
#'   parser and the help generator.
#' @return *\[list\]* A list of flag definitions.
#' @keywords internal
cli_flag_spec <- function() {
  list(
    list(
      name = "options", type = "string",
      help = "Options file name (or path) to run with.",
      subcommands = c("run", "options")
    ),
    list(
      name = "options-dir", type = "string",
      help = "Directory that holds the options files.",
      subcommands = c("run", "options")
    ),
    list(
      name = "data", type = "string",
      help = "Dataset path (overrides data.source_path for this run).",
      subcommands = "run"
    ),
    list(
      name = "methods", type = "list",
      help = "Comma-separated method names, or 'all'.",
      subcommands = "run"
    ),
    list(
      name = "output-dir", type = "string",
      help = "Directory to export results into.",
      subcommands = "run"
    ),
    list(
      name = "verbose", type = "integer",
      help = "Verbosity from 1 (errors only) to 4 (debug).",
      subcommands = "run"
    ),
    list(
      name = "no-cache", type = "flag",
      help = "Disable the on-disk method cache for this run.",
      subcommands = "run"
    ),
    list(
      name = "report", type = "flag",
      help = "Render an HTML report after the run.",
      subcommands = "run"
    ),
    list(
      name = "json", type = "flag",
      help = "Emit a JSON run manifest on stdout (all other output goes to stderr).",
      subcommands = "run"
    )
  )
}

#' @title Flags a subcommand accepts
#' @param subcommand *\[character\]* The subcommand name.
#' @return *\[list\]* The subset of `cli_flag_spec()` accepted by the subcommand.
#' @keywords internal
flags_for_subcommand <- function(subcommand) {
  Filter(function(f) subcommand %in% f$subcommands, cli_flag_spec())
}

#' @title Coerce a raw flag value to its declared type
#' @param fdef *\[list\]* The flag definition (from `cli_flag_spec()`).
#' @param raw *\[character\]* The raw string value read from the argument vector.
#' @return *\[list\]* Either `list(value = ...)` on success or
#'   `list(error = "...")` on a malformed value.
#' @keywords internal
coerce_flag_value <- function(fdef, raw) {
  switch(fdef$type,
    string = list(value = raw),
    list = {
      parts <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0L) {
        return(list(error = sprintf("Flag '--%s' expects a non-empty comma-separated list.", fdef$name)))
      }
      list(value = parts)
    },
    integer = {
      if (!grepl("^-?[0-9]+$", trimws(raw))) {
        return(list(error = sprintf("Flag '--%s' expects an integer, got '%s'.", fdef$name, raw)))
      }
      list(value = as.integer(trimws(raw)))
    },
    list(error = sprintf("Flag '--%s' has an unsupported type.", fdef$name))
  )
}

#' @title Parse a CLI argument vector
#' @description Translate a raw argument vector into a structured result. The
#'   parser is total: it never throws and never prompts. Usage problems come
#'   back as `action = "error"`; a `--help` request anywhere comes back as
#'   `action = "help"`; a well-formed invocation comes back as
#'   `action = "dispatch"` with the resolved subcommand, sub-action and flags.
#' @param argv *\[character\]* The argument vector (typically
#'   `commandArgs(trailingOnly = TRUE)`).
#' @return *\[list\]* A parse result with an `action` field.
#' @keywords internal
cli_parse <- function(argv) {
  subs <- cli_subcommands()

  if (length(argv) == 0L || argv[[1]] %in% c("--help", "-h", "help")) {
    return(list(action = "help", subcommand = NULL))
  }

  subcommand <- argv[[1]]
  if (!(subcommand %in% names(subs))) {
    return(list(
      action = "error",
      subcommand = NULL,
      message = sprintf("Unknown command: '%s'.", subcommand)
    ))
  }

  rest <- argv[-1]
  sub_def <- subs[[subcommand]]

  # A --help / -h token anywhere below the subcommand prints that subcommand's help.
  if (any(rest %in% c("--help", "-h"))) {
    return(list(action = "help", subcommand = subcommand))
  }

  subaction <- NULL
  if (!is.null(sub_def$subactions)) {
    if (length(rest) == 0L) {
      return(list(
        action = "error",
        subcommand = subcommand,
        message = sprintf(
          "Command '%s' requires a sub-action: %s.",
          subcommand, paste(sub_def$subactions, collapse = ", ")
        )
      ))
    }
    subaction <- rest[[1]]
    if (!(subaction %in% sub_def$subactions)) {
      return(list(
        action = "error",
        subcommand = subcommand,
        message = sprintf(
          "Unknown '%s' sub-action: '%s'. Expected one of: %s.",
          subcommand, subaction, paste(sub_def$subactions, collapse = ", ")
        )
      ))
    }
    rest <- rest[-1]
  }

  accepted <- flags_for_subcommand(subcommand)
  accepted_by_name <- stats::setNames(accepted, vapply(accepted, `[[`, character(1), "name"))

  flags <- list()
  i <- 1L
  while (i <= length(rest)) {
    tok <- rest[[i]]
    if (!startsWith(tok, "--")) {
      return(list(
        action = "error",
        subcommand = subcommand,
        message = sprintf("Unexpected argument: '%s'.", tok)
      ))
    }

    body <- substring(tok, 3L)
    eq <- regexpr("=", body, fixed = TRUE)
    inline_val <- NULL
    key <- body
    if (eq > 0L) {
      key <- substr(body, 1L, eq - 1L)
      inline_val <- substr(body, eq + 1L, nchar(body))
    }

    if (!nzchar(key) || !(key %in% names(accepted_by_name))) {
      return(list(
        action = "error",
        subcommand = subcommand,
        message = sprintf("Unknown flag for '%s': '--%s'.", subcommand, key)
      ))
    }

    fdef <- accepted_by_name[[key]]

    if (identical(fdef$type, "flag")) {
      if (!is.null(inline_val)) {
        return(list(
          action = "error",
          subcommand = subcommand,
          message = sprintf("Flag '--%s' does not take a value.", key)
        ))
      }
      flags[[key]] <- TRUE
      i <- i + 1L
      next
    }

    if (!is.null(inline_val)) {
      raw <- inline_val
      i <- i + 1L
    } else {
      if (i + 1L > length(rest)) {
        return(list(
          action = "error",
          subcommand = subcommand,
          message = sprintf("Flag '--%s' requires a value.", key)
        ))
      }
      raw <- rest[[i + 1L]]
      i <- i + 2L
    }

    coerced <- coerce_flag_value(fdef, raw)
    if (!is.null(coerced$error)) {
      return(list(action = "error", subcommand = subcommand, message = coerced$error))
    }
    flags[[key]] <- coerced$value
  }

  list(
    action = "dispatch",
    subcommand = subcommand,
    subaction = subaction,
    flags = flags
  )
}

#' @title Build the in-session options overlay for a run
#' @description Translate parsed `run` flags into a named list of `artma.*`
#'   options. The overlay is applied around the `artma()` call so the user's
#'   YAML options file is never mutated. `--report` sets `artma.output.report`
#'   (defined by the HTML-report feature); the flag is inert until that option
#'   exists in the template.
#' @param flags *\[list\]* The parsed flags for the `run` subcommand.
#' @return *\[list\]* A named list suitable for `withr::with_options()`.
#' @keywords internal
build_option_overlay <- function(flags) {
  overlay <- list()
  if (!is.null(flags[["data"]])) {
    overlay[["artma.data.source_path"]] <- flags[["data"]]
  }
  if (!is.null(flags[["output-dir"]])) {
    overlay[["artma.output.dir"]] <- flags[["output-dir"]]
  }
  if (!is.null(flags[["verbose"]])) {
    overlay[["artma.verbose"]] <- flags[["verbose"]]
  }
  if (isTRUE(flags[["no-cache"]])) {
    overlay[["artma.cache.use_cache"]] <- FALSE
  }
  if (isTRUE(flags[["report"]])) {
    overlay[["artma.output.report"]] <- TRUE
  }
  overlay
}

#' @title Generate CLI help text
#' @description Build the `--help` text from the same spec that drives the
#'   parser. With no subcommand, prints the top-level usage and the subcommand
#'   list; with a subcommand, prints its usage and accepted flags.
#' @param subcommand *\[character, optional\]* The subcommand to document, or
#'   `NULL` for the top-level help. Defaults to `NULL`.
#' @return *\[character\]* Help lines (one element per line).
#' @keywords internal
cli_help_text <- function(subcommand = NULL) {
  subs <- cli_subcommands()

  if (is.null(subcommand)) {
    lines <- c(
      "artma: Automatic Replication Tools for Meta-Analysis",
      "",
      "Usage: artma <command> [options]",
      "",
      "Commands:"
    )
    name_width <- max(nchar(names(subs)))
    for (nm in names(subs)) {
      lines <- c(lines, sprintf("  %-*s  %s", name_width, nm, subs[[nm]]$help))
    }
    lines <- c(
      lines,
      "",
      "Run 'artma <command> --help' for command-specific options.",
      "Without the launcher, invoke as: Rscript -e 'artma::cli.run()' <command> [options]"
    )
    return(lines)
  }

  sub_def <- subs[[subcommand]]
  usage <- sprintf("Usage: artma %s", subcommand)
  if (!is.null(sub_def$subactions)) {
    usage <- sprintf("%s <%s> [options]", usage, paste(sub_def$subactions, collapse = "|"))
  } else {
    usage <- sprintf("%s [options]", usage)
  }

  lines <- c(usage, "", sub_def$help)

  accepted <- flags_for_subcommand(subcommand)
  if (length(accepted) > 0L) {
    lines <- c(lines, "", "Options:")
    labels <- vapply(accepted, function(f) {
      if (identical(f$type, "flag")) paste0("--", f$name) else paste0("--", f$name, " <", f$type, ">")
    }, character(1))
    label_width <- max(nchar(labels))
    for (k in seq_along(accepted)) {
      lines <- c(lines, sprintf("  %-*s  %s", label_width, labels[[k]], accepted[[k]]$help))
    }
  }
  lines <- c(lines, "", "  --help                        Show this help and exit.")

  lines
}

box::export(
  cli_subcommands,
  cli_flag_spec,
  cli_parse,
  coerce_flag_value,
  build_option_overlay,
  cli_help_text
)
