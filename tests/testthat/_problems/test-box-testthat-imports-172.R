# Extracted from test-box-testthat-imports.R:172

# prequel ----------------------------------------------------------------------
box::use(
  testthat[fail, succeed, test_path, test_that]
)
qualifier_tokens <- c("NS_GET", "NS_GET_INT", "'$'", "'@'")
attach_all_exports <- function(target) {
  if (!is.symbol(target)) {
    # Module paths (artma / x / y[...]) cannot be enumerated without loading
    # the module; no test file uses this form.
    return(character(0L))
  }
  pkg <- as.character(target)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return(character(0L))
  }
  getNamespaceExports(pkg)
}
module_basename <- function(path_call) {
  leaf <- path_call[[3L]]
  if (is.symbol(leaf)) as.character(leaf) else character(0L)
}
spec_bindings <- function(spec, given_name) {
  if (is.call(spec) && identical(spec[[1L]], quote(`[`))) {
    target <- spec[[2L]]
    entries <- as.list(spec)[-c(1L, 2L)]
    entry_names <- names(entries)
    if (is.null(entry_names)) {
      entry_names <- rep("", length(entries))
    }
    bound <- if (nzchar(given_name)) given_name else character(0L)
    for (i in seq_along(entries)) {
      entry <- entries[[i]]
      if (nzchar(entry_names[[i]])) {
        bound <- c(bound, entry_names[[i]])
      } else if (identical(entry, quote(...))) {
        bound <- c(bound, attach_all_exports(target))
      } else if (is.symbol(entry)) {
        bound <- c(bound, as.character(entry))
      } else if (is.character(entry)) {
        bound <- c(bound, entry)
      }
    }
    bound
  } else if (is.symbol(spec)) {
    if (nzchar(given_name)) given_name else as.character(spec)
  } else if (is.call(spec) && identical(spec[[1L]], quote(`/`))) {
    if (nzchar(given_name)) given_name else module_basename(spec)
  } else {
    character(0L)
  }
}
collect_box_bindings <- function(node) {
  bound <- character(0L)
  if (is.call(node) && identical(node[[1L]], quote(box::use))) {
    specs <- as.list(node)[-1L]
    spec_names <- names(specs)
    if (is.null(spec_names)) {
      spec_names <- rep("", length(specs))
    }
    for (i in seq_along(specs)) {
      bound <- c(bound, spec_bindings(specs[[i]], spec_names[[i]]))
    }
  }
  if (is.call(node) || is.pairlist(node)) {
    children <- as.list(node)
    for (i in seq_along(children)) {
      if (identical(children[[i]], quote(expr = ))) {
        next
      }
      bound <- c(bound, collect_box_bindings(children[[i]]))
    }
  }
  bound
}
bare_call_names <- function(terminals) {
  call_idx <- which(terminals$token == "SYMBOL_FUNCTION_CALL")
  prev_token <- rep(NA_character_, length(call_idx))
  has_prev <- call_idx > 1L
  prev_token[has_prev] <- terminals$token[call_idx[has_prev] - 1L]
  unique(terminals$text[call_idx[!prev_token %in% qualifier_tokens]])
}
local_names <- function(terminals) {
  formals_names <- terminals$text[terminals$token == "SYMBOL_FORMALS"]

  assign_idx <- which(terminals$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))
  target_idx <- assign_idx[assign_idx > 1L] - 1L
  assigned <- terminals$text[target_idx[terminals$token[target_idx] == "SYMBOL"]]

  right_idx <- which(terminals$token == "RIGHT_ASSIGN")
  rhs_idx <- right_idx[right_idx < nrow(terminals)] + 1L
  right_assigned <- terminals$text[rhs_idx[terminals$token[rhs_idx] == "SYMBOL"]]

  unique(c(formals_names, assigned, right_assigned))
}
missing_testthat_imports <- function(path, testthat_exports) {
  exprs <- tryCatch(
    parse(path, keep.source = TRUE),
    error = function(e) NULL
  )
  if (is.null(exprs)) {
    # An unparseable file is reported by the suite itself.
    return(character(0L))
  }
  parse_data <- utils::getParseData(exprs)
  if (is.null(parse_data) || nrow(parse_data) == 0L) {
    return(character(0L))
  }
  terminals <- parse_data[parse_data$terminal, , drop = FALSE]
  terminals <- terminals[order(terminals$line1, terminals$col1), , drop = FALSE]

  bound <- unlist(lapply(as.list(exprs), collect_box_bindings))
  candidates <- intersect(bare_call_names(terminals), testthat_exports)
  sort(setdiff(candidates, c(bound, local_names(terminals))))
}

# test -------------------------------------------------------------------------
root <- test_path(".")
rel_paths <- list.files(root, pattern = "[.][Rr]$", recursive = TRUE)
if (length(rel_paths) == 0L) {
  fail(sprintf("No .R files found under '%s'; the audit path is wrong", root))
}
testthat_exports <- getNamespaceExports("testthat")
violations <- character(0L)
for (rel_path in rel_paths) {
  missing_names <- missing_testthat_imports(
    file.path(root, rel_path),
    testthat_exports
  )
  if (length(missing_names) > 0L) {
    violations <- c(
      violations,
      sprintf("%s: %s", rel_path, paste(missing_names, collapse = ", "))
    )
  }
}
if (length(violations) > 0L) {
  fail(paste(
    c(
      "testthat functions called without a box::use(testthat[...]) import:",
      paste0("- ", violations),
      "Add each missing name to that file's box::use(testthat[...]) block."
    ),
    collapse = "\n"
  ))
}
