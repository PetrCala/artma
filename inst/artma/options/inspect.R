#' Inspection helpers for the options system.
#'
#' These back the discovery half of the `artma::options.*` API: rendering the
#' template as a browsable tree, expanding option name prefixes, summarising the
#' user options files on disk and diffing two of them.

# Sentinel for a key that is absent altogether, as opposed to a key explicitly
# set to null. Comparisons treat the two as different.
MISSING_OPTION <- `class<-`(list(), "artma_missing_option") # nolint: object_name_linter.

#' @title Format an option value for display
#' @description Render any option value as a single short line. Lists are
#'   summarised by their length, vectors are bracketed, and long values are
#'   truncated.
#' @param value *\[any\]* The value to format.
#' @param max_len *\[integer, optional\]* Maximum width of the rendered value.
#' @return *\[character\]* A single-element character vector.
format_option_value <- function(value, max_len = 60L) {
  format_scalar <- function(x) {
    if (is.na(x)) {
      return("NA")
    }
    if (is.logical(x)) {
      return(if (isTRUE(x)) "true" else "false")
    }
    if (is.character(x)) {
      return(x)
    }
    format(x)
  }

  out <- if (inherits(value, "artma_missing_option")) {
    "(unset)"
  } else if (is.null(value)) {
    "null"
  } else if (length(value) == 0) {
    "(empty)"
  } else if (is.list(value)) {
    cli::format_inline("<list of {length(value)} entr{?y/ies}>")
  } else if (length(value) > 1) {
    paste0("[", paste(vapply(value, format_scalar, character(1)), collapse = ", "), "]")
  } else {
    format_scalar(value)
  }

  out <- gsub("\\s+", " ", out)
  if (nchar(out) <= max_len) {
    return(out)
  }

  ellipsis <- cli::symbol$ellipsis
  paste0(substr(out, 1L, max(max_len - nchar(ellipsis), 1L)), ellipsis)
}

#' @title Compare two option values
#' @description Equality that tolerates the storage-type noise of a YAML
#'   round-trip (an integer default read back as a double, say) while keeping
#'   `NULL`, `NA` and the missing-key sentinel distinct from each other.
#' @param a *\[any\]* First value.
#' @param b *\[any\]* Second value.
#' @return *\[logical\]* Whether the two values are equivalent.
values_equal <- function(a, b) {
  a_missing <- inherits(a, "artma_missing_option")
  b_missing <- inherits(b, "artma_missing_option")
  if (a_missing || b_missing) {
    return(a_missing && b_missing)
  }
  if (is.null(a) || is.null(b)) {
    return(is.null(a) && is.null(b))
  }
  isTRUE(all.equal(a, b))
}

#' @title Pick a value or the missing sentinel
#' @param x *\[list\]* The list to read from.
#' @param key *\[character\]* The key to read.
#' @return *\[any\]* The value, or [MISSING_OPTION] when the key is absent.
#' @keywords internal
pick_value <- function(x, key) {
  if (!(key %in% names(x))) {
    return(MISSING_OPTION)
  }
  x[[key]]
}

#' @title Expand option name tokens against the template
#' @description Resolves user-supplied tokens to option names. A token that names
#'   an option matches it exactly; a token that names a group (e.g. `methods.bma`)
#'   expands to every option underneath it. Order follows the template, and
#'   duplicates are dropped.
#' @param tokens *\[character\]* The requested names or group prefixes.
#' @param all_names *\[character\]* Every option name defined by the template.
#' @return *\[list\]* With `matched` (character vector of option names) and
#'   `not_found` (the tokens that matched nothing).
expand_option_tokens <- function(tokens, all_names) {
  matched <- character(0)
  not_found <- character(0)

  for (token in tokens) {
    hits <- if (token %in% all_names) {
      token
    } else {
      all_names[startsWith(all_names, paste0(token, "."))]
    }

    if (length(hits) == 0) {
      not_found <- c(not_found, token)
    } else {
      matched <- c(matched, hits)
    }
  }

  list(matched = unique(matched), not_found = not_found)
}

#' @title Print the option template as a tree
#' @description Prints every template option, grouped by top-level section, one
#'   line per option carrying its name, type and default.
#' @param defs *\[list\]* Flattened template option definitions.
#' @return `NULL`, invisibly.
print_option_tree <- function(defs) {
  box::use(artma / const[CONST])

  opt_styles <- CONST$STYLES$OPTIONS
  opt_names <- vapply(defs, `[[`, character(1), "name")
  sections <- vapply(strsplit(opt_names, ".", fixed = TRUE), `[[`, character(1), 1L)

  cli::cli_h1("artma options")
  cli::cli_text(
    "{length(defs)} option{?s} in {length(unique(sections))} section{?s}. ",
    "Call {.code artma::options_help('<name>')} with an option or a group name for details."
  )
  cli::cat_line()

  for (section in unique(sections)) {
    idx <- which(sections == section)
    cli::cli_h2(section)

    name_width <- max(nchar(opt_names[idx]))
    type_width <- min(max(nchar(vapply(defs[idx], function(d) d$type, character(1)))), 28L)

    for (i in idx) {
      opt_def <- defs[[i]]
      default <- if ("default" %in% names(opt_def)) format_option_value(opt_def$default) else "(required)"
      type <- opt_def$type
      if (nchar(type) > type_width) {
        type <- paste0(substr(type, 1L, type_width - 1L), cli::symbol$ellipsis)
      }

      cli::cat_line(paste0(
        "  ",
        opt_styles$NAME(formatC(opt_def$name, width = -name_width)),
        "  ",
        opt_styles$TYPE(formatC(type, width = -type_width)),
        "  ",
        opt_styles$DEFAULT(default)
      ))
    }
    cli::cat_line()
  }

  invisible(NULL)
}

#' @title Print the detailed help of a single option
#' @param opt_def *\[list\]* A flattened template option definition.
#' @return `NULL`, invisibly.
print_option_detail <- function(opt_def) {
  box::use(
    artma / const[CONST],
    artma / options / utils[print_options_help_text]
  )

  opt_styles <- CONST$STYLES$OPTIONS
  default <- if ("default" %in% names(opt_def)) {
    # Accessing a null default value would assign null, so we coerce it to a 'null' string.
    opt_def$default %||% "null"
  } else {
    "This option is required"
  }

  cli::cli_text("{.strong Option name:} {opt_styles$NAME(opt_def$name)}")
  cli::cli_text("{.strong Type:} {opt_styles$TYPE(opt_def$type)}")
  cli::cli_text("{.strong Default:} {opt_styles$DEFAULT(default)}")
  print_options_help_text(opt_def$help %||% "(No help text provided.)")
  cli::cli_rule()
  cli::cat_line()

  invisible(NULL)
}

#' @title Resolve the output directory of an options file without loading it
#' @description Mirrors [artma/output/export]'s resolution so a file can be
#'   inspected without being loaded into the R options namespace.
#' @param options_file_name *\[character\]* Name of the options file.
#' @param flat_options *\[list\]* The file's options, flattened to dotted paths.
#' @return *\[character\]* The output directory path.
#' @keywords internal
resolve_output_dir_for_file <- function(options_file_name, flat_options) {
  box::use(artma / output / export[auto_output_subdir, is_auto_output_dir])

  configured <- flat_options[["output.dir"]]
  if (is_auto_output_dir(configured)) {
    return(file.path(
      tools::R_user_dir("artma", which = "data"),
      "results",
      auto_output_subdir(options_file_name)
    ))
  }
  configured
}

#' @title Read the time an options file was last run
#' @description Until the per-run manifest lands, the most recent write to the
#'   file's output directory is the best available record of when it last ran.
#'   With `output.run_subdirectories` enabled the runs live in their own
#'   subdirectories, so the latest of those is what carries the run's time.
#'   Returns `NA` when the directory has never been written.
#' @param options_file_name *\[character\]* Name of the options file.
#' @param flat_options *\[list\]* The file's options, flattened to dotted paths.
#' @return *\[POSIXct\]* The last run time, or `NA`.
#' @keywords internal
last_run_time <- function(options_file_name, flat_options) {
  box::use(artma / output / export[latest_run_output_dir])

  output_dir <- resolve_output_dir_for_file(options_file_name, flat_options)

  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !dir.exists(output_dir)) {
    return(as.POSIXct(NA))
  }

  if (isTRUE(flat_options[["output.run_subdirectories"]])) {
    latest_run <- latest_run_output_dir(output_dir)
    if (!is.null(latest_run)) {
      return(file.mtime(latest_run))
    }
  }

  file.mtime(output_dir)
}

#' @title Collect the options of a file that deviate from the template defaults
#' @param flat_options *\[list\]* The file's options, flattened to dotted paths.
#' @param defaults *\[list\]* The template defaults, keyed by dotted path.
#' @param leaf_names *\[character\]* Every option name defined by the template.
#' @return *\[data.frame\]* With `option`, `default` and `value` columns.
collect_deviations <- function(flat_options, defaults, leaf_names) {
  present <- leaf_names[leaf_names %in% names(flat_options)]
  differs <- vapply(
    present,
    function(nm) !values_equal(pick_value(flat_options, nm), pick_value(defaults, nm)),
    logical(1),
    USE.NAMES = FALSE
  )
  deviating <- present[differs]

  data.frame(
    option = deviating,
    default = vapply(deviating, function(nm) format_option_value(pick_value(defaults, nm)), character(1), USE.NAMES = FALSE),
    value = vapply(deviating, function(nm) format_option_value(flat_options[[nm]]), character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

#' @title Describe the user options files in a directory
#' @description Builds the data frame behind `artma::options_list(details = TRUE)`.
#' @param options_dir *\[character\]* Directory holding the user options files.
#' @param template_path *\[character\]* Path to the options template.
#' @return *\[data.frame\]* One row per options file.
describe_options_files <- function(options_dir, template_path) {
  box::use(
    artma / options / files[list_options_files, options_file_path, read_options_file],
    artma / options / template[collect_leaf_paths, flatten_user_options, get_template_defaults]
  )

  file_names <- list_options_files(options_dir)

  empty <- data.frame(
    file = character(0),
    data_source_path = character(0),
    modified = as.POSIXct(character(0)),
    last_run = as.POSIXct(character(0)),
    n_non_default = integer(0),
    stringsAsFactors = FALSE
  )

  if (length(file_names) == 0) {
    return(empty)
  }

  leaf_names <- collect_leaf_paths(template_path)
  defaults <- get_template_defaults(template_path)

  rows <- lapply(file_names, function(file_name) {
    path <- options_file_path(options_dir, file_name)
    flat <- tryCatch(
      flatten_user_options(user_options = read_options_file(path), leaf_set = leaf_names),
      error = function(cond) list()
    )

    source_path <- flat[["data.source_path"]]
    if (!is.character(source_path) || length(source_path) != 1L) {
      source_path <- NA_character_
    }

    data.frame(
      file = file_name,
      data_source_path = source_path,
      modified = file.mtime(path),
      last_run = last_run_time(file_name, flat),
      n_non_default = nrow(collect_deviations(flat, defaults, leaf_names)),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @title Diff two option value trees
#' @description Walks two values in parallel, descending into named lists so a
#'   list-typed option (e.g. `data.columns`) reports the sub-paths that differ
#'   rather than a wholesale mismatch.
#' @param path *\[character\]* The dotted path of the current node.
#' @param a *\[any\]* The value from the first file.
#' @param b *\[any\]* The value from the second file.
#' @return *\[list\]* A list of `list(option, a, b)` records.
#' @keywords internal
diff_option_values <- function(path, a, b) {
  box::use(artma / options / template[is_descendable])

  if (is_descendable(a) && is_descendable(b)) {
    keys <- union(names(a), names(b))
    records <- list()
    for (key in keys) {
      records <- c(records, diff_option_values(
        path = paste(path, key, sep = "."),
        a = pick_value(a, key),
        b = pick_value(b, key)
      ))
    }
    return(records)
  }

  if (values_equal(a, b)) {
    return(list())
  }

  list(list(option = path, a = format_option_value(a), b = format_option_value(b)))
}

#' @title Compare two user options files
#' @description Returns the options where the two files differ, plus each file's
#'   deviations from the template defaults.
#' @param options_dir *\[character\]* Directory holding the user options files.
#' @param file_a *\[character\]* Name of the first options file.
#' @param file_b *\[character\]* Name of the second options file.
#' @param template_path *\[character\]* Path to the options template.
#' @return *\[list\]* With `files`, `differences` and `deviations`.
diff_options_files <- function(options_dir, file_a, file_b, template_path) {
  box::use(
    artma / options / files[options_file_path, read_options_file],
    artma / options / template[collect_leaf_paths, flatten_user_options, get_template_defaults],
    artma / libs / core / validation[assert]
  )

  paths <- vapply(c(file_a, file_b), function(nm) options_file_path(options_dir, nm), character(1), USE.NAMES = FALSE)
  for (path in paths) {
    assert(file.exists(path), cli::format_inline("The options file {.path {path}} does not exist."))
  }

  leaf_names <- collect_leaf_paths(template_path)
  defaults <- get_template_defaults(template_path)

  flat_a <- flatten_user_options(user_options = read_options_file(paths[[1]]), leaf_set = leaf_names)
  flat_b <- flatten_user_options(user_options = read_options_file(paths[[2]]), leaf_set = leaf_names)

  # Options the files carry outside the template still matter to a diff, so the
  # comparison runs over the union of the template leaves and both files' keys.
  compared <- unique(c(leaf_names, names(flat_a), names(flat_b)))
  compared <- compared[compared %in% c(names(flat_a), names(flat_b))]

  records <- list()
  for (nm in compared) {
    records <- c(records, diff_option_values(nm, pick_value(flat_a, nm), pick_value(flat_b, nm)))
  }

  differences <- if (length(records) == 0) {
    data.frame(option = character(0), a = character(0), b = character(0), stringsAsFactors = FALSE)
  } else {
    data.frame(
      option = vapply(records, `[[`, character(1), "option"),
      a = vapply(records, `[[`, character(1), "a"),
      b = vapply(records, `[[`, character(1), "b"),
      stringsAsFactors = FALSE
    )
  }
  names(differences)[2:3] <- c(file_a, file_b)

  list(
    files = c(file_a, file_b),
    differences = differences,
    deviations = stats::setNames(
      list(
        collect_deviations(flat_a, defaults, leaf_names),
        collect_deviations(flat_b, defaults, leaf_names)
      ),
      c(file_a, file_b)
    )
  )
}

#' @title Print the result of a two-file options diff
#' @param diff *\[list\]* The value returned by [diff_options_files()].
#' @return `NULL`, invisibly.
print_options_diff <- function(diff) {
  box::use(artma / const[CONST])

  opt_styles <- CONST$STYLES$OPTIONS
  file_a <- diff$files[[1]]
  file_b <- diff$files[[2]]

  cli::cli_h1("Options diff")
  cli::cli_text("{.strong A}: {.file {file_a}}")
  cli::cli_text("{.strong B}: {.file {file_b}}")
  cli::cat_line()

  differences <- diff$differences
  if (nrow(differences) == 0) {
    cli::cli_alert_success("The two options files carry identical values.")
  } else {
    cli::cli_h2("Differing options ({nrow(differences)})")
    width <- max(nchar(differences$option))
    for (i in seq_len(nrow(differences))) {
      cli::cat_line(paste0(
        "  ",
        opt_styles$NAME(formatC(differences$option[[i]], width = -width)),
        "  ",
        opt_styles$VALUE(differences[[2]][[i]]),
        " -> ",
        opt_styles$VALUE(differences[[3]][[i]])
      ))
    }
    cli::cat_line()
  }

  for (file_name in diff$files) {
    deviations <- diff$deviations[[file_name]]
    cli::cli_h2("{file_name}: deviations from template defaults ({nrow(deviations)})")
    if (nrow(deviations) == 0) {
      cli::cli_text("None; every option matches its template default.")
    } else {
      width <- max(nchar(deviations$option))
      for (i in seq_len(nrow(deviations))) {
        cli::cat_line(paste0(
          "  ",
          opt_styles$NAME(formatC(deviations$option[[i]], width = -width)),
          "  ",
          opt_styles$DEFAULT(deviations$default[[i]]),
          " -> ",
          opt_styles$VALUE(deviations$value[[i]])
        ))
      }
    }
    cli::cat_line()
  }

  invisible(NULL)
}

box::export(
  collect_deviations,
  describe_options_files,
  diff_options_files,
  diff_option_values,
  expand_option_tokens,
  format_option_value,
  last_run_time,
  print_option_detail,
  print_option_tree,
  print_options_diff,
  values_equal
)
