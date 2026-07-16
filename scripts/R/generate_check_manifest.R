# Generates R/generated_check_manifest.R, the single file that keeps R CMD
# check quiet about symbols that only exist inside the `box`-managed
# `inst/artma` tree (invisible to check's static analysis).
#
# It replaces three previously hand-maintained mechanisms:
#   - R/globals.R (utils::globalVariables() entries for box::use() symbols)
#   - register_runtime_dependencies() in R/zzz.R (Imports keep-alive refs)
#   - R/dummy_functions.R (more Imports keep-alive refs, different idiom)
#
# Run via `make document` (wired into the document target), or directly with
# `Rscript scripts/R/generate_check_manifest.R`.

#' List the R/*.R source files to scan for `box::use()` calls.
#'
#' @param pkg_root *[character]* Path to the package root.
#' @return *[character]* Full paths to the R source files, excluding the
#'   generated manifest itself.
.cm_r_source_files <- function(pkg_root) {
  files <- list.files(file.path(pkg_root, "R"), pattern = "\\.R$", full.names = TRUE)
  files[basename(files) != "generated_check_manifest.R"]
}

#' Parse a single R source file into a list of top-level expressions.
#'
#' @param path *[character]* Path to the R source file.
#' @return *[list]* Parsed top-level expressions, or an empty list if the
#'   file contains no parseable code.
.cm_parse_file <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) {
    return(list())
  }
  as.list(exprs)
}

#' Check whether an expression is a call to `box::use`.
#'
#' @param expr *[call|any]* The expression to test.
#' @return *[logical]* `TRUE` if `expr` is a `box::use(...)` call.
.cm_is_box_use_call <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(box::use))
}

#' Recursively collect every `box::use(...)` call found anywhere within an
#' expression, including inside nested function bodies.
#'
#' @param expr *[call|any]* The expression to search.
#' @param acc *[list]* Accumulator of matches found so far.
#' @return *[list]* All `box::use(...)` call expressions found.
.cm_find_box_use_calls <- function(expr, acc = list()) {
  if (!is.call(expr)) {
    return(acc)
  }
  if (.cm_is_box_use_call(expr)) {
    acc[[length(acc) + 1]] <- expr
  }
  for (i in seq_along(expr)) {
    # A call element can be R's "missing argument" placeholder (e.g. the
    # empty index in `x[, 1]`); touching it at all raises "argument is
    # missing", so the is.call() check itself must run inside tryCatch.
    part_is_call <- tryCatch(is.call(expr[[i]]), error = function(e) FALSE)
    if (part_is_call) {
      acc <- .cm_find_box_use_calls(expr[[i]], acc)
    }
  }
  acc
}

#' Derive the `globalVariables()` symbol list from every `box::use()` call
#' under `R/*.R`.
#'
#' Every bare identifier that appears anywhere inside a `box::use(...)` call
#' - module path segments (e.g. `libs`, `core`, `validation`) as well as the
#' imported symbol names (e.g. `assert`, `validate`) - is parsed by R as a
#' free variable reference (box's `pkg / path / mod[fn]` syntax is just
#' ordinary R syntax for division and indexing). `R CMD check`'s static
#' analysis flags any such reference that isn't otherwise resolvable, so all
#' of them must be declared, whether or not they end up called.
#'
#' A `box::use(alias = pkg / path / mod)` form additionally needs `alias`
#' itself declared: box binds it as a local variable at runtime, but as a
#' named call argument it is invisible to codetools' local-assignment
#' detection, so later bare uses of `alias` in the function body would
#' otherwise be flagged too.
#'
#' @param pkg_root *[character]* Path to the package root.
#' @return *[character]* Sorted, deduplicated symbol names.
.cm_collect_global_variables <- function(pkg_root) {
  calls <- unlist(
    lapply(.cm_r_source_files(pkg_root), function(path) {
      unlist(lapply(.cm_parse_file(path), .cm_find_box_use_calls), recursive = FALSE)
    }),
    recursive = FALSE
  )
  value_symbols <- unlist(lapply(calls, all.vars))
  alias_symbols <- unlist(lapply(calls, function(call) {
    tags <- names(call)
    if (is.null(tags)) character(0) else tags[nzchar(tags)]
  }))
  symbols <- unique(as.character(c(value_symbols, alias_symbols)))
  sort(symbols, method = "radix")
}

#' Parse the `Imports` field of DESCRIPTION into bare package names.
#'
#' @param desc_path *[character]* Path to the DESCRIPTION file.
#' @return *[character]* Package names, stripped of version constraints.
.cm_imports_packages <- function(desc_path) {
  fields <- read.dcf(desc_path, fields = "Imports")
  imports_raw <- fields[1, "Imports"]
  if (is.na(imports_raw)) {
    return(character(0))
  }
  pkgs <- strsplit(imports_raw, ",")[[1]]
  pkgs <- trimws(gsub("\\(.*\\)", "", pkgs))
  pkgs[nzchar(pkgs)]
}

#' Recursively collect every `pkg::fun` namespace reference found anywhere
#' within an expression.
#'
#' @param expr *[call|any]* The expression to search.
#' @param acc *[character]* Accumulator of `"pkg::fun"` strings found so far.
#' @return *[character]* Every `"pkg::fun"` reference found.
.cm_find_pkg_fun_refs <- function(expr, acc = character(0)) {
  if (!is.call(expr)) {
    return(acc)
  }
  if (identical(expr[[1]], quote(`::`)) && length(expr) == 3 &&
    is.name(expr[[2]]) && is.name(expr[[3]])) {
    acc <- c(acc, paste0(as.character(expr[[2]]), "::", as.character(expr[[3]])))
  }
  for (i in seq_along(expr)) {
    # See the matching guard in .cm_find_box_use_calls() for why this check
    # must run inside tryCatch.
    part_is_call <- tryCatch(is.call(expr[[i]]), error = function(e) FALSE)
    if (part_is_call) {
      acc <- .cm_find_pkg_fun_refs(expr[[i]], acc)
    }
  }
  acc
}

#' Collect every `pkg::fun` namespace reference found under a directory tree.
#'
#' @param dir *[character]* Directory to scan, recursively, for `.R` files.
#' @return *[character]* Unique `"pkg::fun"` references found.
.cm_collect_pkg_fun_refs <- function(dir) {
  files <- list.files(dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  files <- files[basename(files) != "generated_check_manifest.R"]
  refs <- unlist(lapply(files, function(path) {
    acc <- character(0)
    for (top_expr in .cm_parse_file(path)) {
      acc <- .cm_find_pkg_fun_refs(top_expr, acc)
    }
    acc
  }))
  unique(as.character(refs))
}

#' Pick one keep-alive `pkg::fun` reference per Imports package that is used
#' only inside `inst/artma` (never referenced directly under `R/`), so
#' `R CMD check` does not flag those Imports as unused.
#'
#' @param pkg_root *[character]* Path to the package root.
#' @return *[character]* Sorted `"pkg::fun"` references, one per package.
.cm_collect_inst_only_keepalive_refs <- function(pkg_root) {
  imports_pkgs <- .cm_imports_packages(file.path(pkg_root, "DESCRIPTION"))
  r_refs <- .cm_collect_pkg_fun_refs(file.path(pkg_root, "R"))
  r_used_pkgs <- unique(sub("::.*$", "", r_refs))
  inst_only_pkgs <- setdiff(imports_pkgs, r_used_pkgs)

  inst_refs <- .cm_collect_pkg_fun_refs(file.path(pkg_root, "inst", "artma"))

  keepalive_refs <- vapply(inst_only_pkgs, function(pkg) {
    candidates <- sort(inst_refs[startsWith(inst_refs, paste0(pkg, "::"))], method = "radix")
    if (length(candidates) == 0) NA_character_ else candidates[[1]]
  }, character(1))

  sort(keepalive_refs[!is.na(keepalive_refs)], method = "radix")
}

#' Build the full contents of the generated check manifest file.
#'
#' @param pkg_root *[character]* Path to the package root.
#' @return *[character]* Lines of the generated R source file.
build_check_manifest <- function(pkg_root = ".") {
  global_vars <- .cm_collect_global_variables(pkg_root)
  keepalive_refs <- .cm_collect_inst_only_keepalive_refs(pkg_root)

  quoted_vars <- paste0('  "', global_vars, '"', ifelse(seq_along(global_vars) == length(global_vars), "", ","))
  keepalive_lines <- paste0("    ", keepalive_refs, ifelse(seq_along(keepalive_refs) == length(keepalive_refs), "", ","))

  c(
    "# Generated by scripts/R/generate_check_manifest.R. Do not edit by hand.",
    "# Run `make document` to regenerate.",
    "#",
    "# Consolidates what used to be three hand-maintained R CMD check workarounds:",
    "# globalVariables() entries for symbols that box::use()'s pkg/path/mod[fn]",
    "# syntax parses as free variable references, and keep-alive references for",
    "# Imports packages that are only used inside the box-managed inst/artma tree.",
    "",
    "utils::globalVariables(c(",
    quoted_vars,
    "))",
    "",
    "#' @keywords internal",
    "#' @noRd",
    ".artma_check_manifest_keep_alive <- function() {",
    "  invisible(list(",
    keepalive_lines,
    "  ))",
    "}"
  )
}

#' Write the generated check manifest to `R/generated_check_manifest.R`.
#'
#' @param pkg_root *[character]* Path to the package root.
#' @return *[character]* The output path, invisibly.
write_check_manifest <- function(pkg_root = ".") {
  out_path <- file.path(pkg_root, "R", "generated_check_manifest.R")
  writeLines(build_check_manifest(pkg_root), out_path)
  invisible(out_path)
}

# Only write the file when run as a script (`Rscript` / `make document`).
# testthat sets TESTTHAT=true for every worker, so tests that source this
# file to call build_check_manifest() directly do not trigger a write.
if (!identical(Sys.getenv("TESTTHAT"), "true")) {
  write_check_manifest()
}
