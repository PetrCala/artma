# Measures test coverage of the whole package in a single traced test run:
# both the R/ surface and the box-managed `inst/artma` tree, which
# covr::package_coverage() cannot see (it only instruments R/ and src/).
#
# How it works: every inst/artma module is force-loaded through box so it
# lands in box's module registry, covr's tracer is then pointed at each
# module namespace environment plus the package namespace itself, and the
# testthat suite runs once with the instrumented functions recording line
# hits. Force-loading first keeps untested modules in the denominator
# instead of silently inflating the percentage. Tracing the package
# namespace works for `artma::fun()` calls too: covr's replace() mutates
# the closure objects in place (covr_reassign_function), so the exports
# environment and any attached copies see the traced versions without
# re-exporting. The trade-off versus covr::package_coverage() is that
# compiled src/ lines are not measured (gcov needs an instrumented
# install), bought back as one full test run instead of two.
#
# This leans on non-exported covr and box internals (guarded below with a
# clear error if a version bump removes them). Parallel testthat is disabled
# for the run: covr's counters live in this process, and parallel workers
# would record nothing.
#
# Run via `make coverage`, or directly with `Rscript scripts/R/inst_coverage.R`.
# Environment switches:
#   ARTMA_COVERAGE_COBERTURA=true  also write cobertura.xml (R/) and
#                                  cobertura-inst.xml (inst/artma) for CI upload
#   ARTMA_COVERAGE_STRICT=true     exit nonzero if any test fails (CI gate)
#   ARTMA_COVERAGE_RDS=<path>      also save the combined coverage object as RDS

Sys.setenv(TESTTHAT_PARALLEL = "false")

pkgload::load_all(quiet = TRUE)

#' Fetch a non-exported object from a namespace, aborting with a clear
#' message if it no longer exists (covr/box internals are not API).
#'
#' @param name *[character]* Object name to fetch.
#' @param ns_name *[character]* Namespace to fetch it from.
#' @return The object.
.ic_internal <- function(name, ns_name) {
  ns <- asNamespace(ns_name)
  if (!exists(name, envir = ns, inherits = FALSE)) {
    cli::cli_abort(c(
      "x" = "{.pkg {ns_name}} {utils::packageVersion(ns_name)} no longer provides the internal {.code {name}}.",
      "i" = "scripts/R/inst_coverage.R relies on non-exported {.pkg covr}/{.pkg box} internals; update the script for the new version."
    ))
  }
  get(name, envir = ns)
}

covr_replacement <- .ic_internal("replacement", "covr")
covr_replacements_box <- .ic_internal("replacements_box", "covr")
covr_replace <- .ic_internal("replace", "covr")
covr_reset_traces <- .ic_internal("reset_traces", "covr")
covr_clear_counters <- .ic_internal("clear_counters", "covr")
covr_as_coverage <- .ic_internal("as_coverage", "covr")
covr_counters <- .ic_internal(".counters", "covr")
covr_the <- .ic_internal("the", "covr")
covr_compact <- .ic_internal("compact", "covr")
box_loaded_mods <- .ic_internal("loaded_mods", "box")

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
inst_prefix <- paste0(repo_root, "/inst/artma/")
r_prefix <- paste0(repo_root, "/R/")

#' Force-load every inst/artma module through box so that modules no test
#' touches still count as 0% instead of vanishing from the denominator.
#'
#' @return *[character]* Relative paths of modules that failed to load.
.ic_force_load_modules <- function() {
  rel_paths <- list.files("inst/artma", pattern = "[.]R$", recursive = TRUE)
  failed <- character(0)
  for (i in seq_along(rel_paths)) {
    import_path <- paste0("artma/", sub("[.]R$", "", rel_paths[i]))
    stmt <- sprintf("box::use(ic_mod_%d = %s)", i, import_path)
    ok <- tryCatch(
      {
        eval(parse(text = stmt), envir = new.env(parent = globalenv()))
        TRUE
      },
      error = function(e) FALSE
    )
    if (!ok) {
      failed <- c(failed, file.path("inst/artma", rel_paths[i]))
    }
  }
  failed
}

#' Build covr replacements for every function in an environment, skipping
#' entries the tracer cannot handle instead of dropping the whole env.
#'
#' @param env *[environment]* A box module env or a package namespace.
#' @return *[list]* covr replacement objects.
.ic_env_replacements <- function(env) {
  box_reps <- tryCatch(covr_replacements_box(env), error = function(e) list())
  fun_reps <- lapply(
    ls(env, all.names = TRUE),
    function(nm) tryCatch(covr_replacement(nm, env = env), error = function(e) NULL)
  )
  covr_compact(c(box_reps, fun_reps))
}

cli::cli_alert_info("Force-loading all inst/artma modules...")
failed_mods <- .ic_force_load_modules()
if (length(failed_mods) > 0) {
  cli::cli_warn(c(
    "!" = "{length(failed_mods)} module{?s} failed to load and will be missing from the coverage denominator:",
    stats::setNames(failed_mods, rep("*", length(failed_mods)))
  ))
}

# Build the replacements for every loaded module env plus the package
# namespace with a SINGLE counter clear. covr's own trace_environment()
# clears counters on each call, which would wipe the previously traced
# modules' counters if used per-module.
cli::cli_alert_info("Instrumenting box module environments and the package namespace...")
covr_clear_counters()
all_replacements <- list()
for (key in ls(box_loaded_mods, all.names = TRUE)) {
  mod <- get(key, envir = box_loaded_mods)
  if (!is.environment(mod)) next
  all_replacements <- c(all_replacements, .ic_env_replacements(mod))
}
all_replacements <- c(all_replacements, .ic_env_replacements(asNamespace("artma")))
covr_the$replacements <- all_replacements
invisible(lapply(covr_the$replacements, covr_replace))
cli::cli_alert_success("Instrumented {length(all_replacements)} function{?s}.")

strict <- identical(Sys.getenv("ARTMA_COVERAGE_STRICT"), "true")
cli::cli_alert_info("Running the test suite (sequential; parallel is disabled under tracing)...")
invisible(withr::with_envvar(c(R_COVR = "true"), {
  testthat::test_dir(
    "tests/testthat",
    reporter = "progress",
    stop_on_failure = strict
  )
}))

coverage <- covr_as_coverage(covr_counters)
covr_reset_traces()
covr_clear_counters()

# The traced set also picks up tempdir test fixtures and the
# tests/testthat/modules scaffolding; keep only entries under the repo's
# R/ and inst/artma trees, split into the two reported units.
.ic_entry_path <- function(entry) {
  srcfile <- attr(entry$srcref, "srcfile")
  normalizePath(srcfile$filename, winslash = "/", mustWork = FALSE)
}
entry_paths <- vapply(coverage, .ic_entry_path, character(1))

#' Wrap a subset of traced entries as a covr coverage object.
#'
#' @param entries *[list]* Counter entries to keep.
#' @return A `coverage` object rooted at the repository.
.ic_as_coverage <- function(entries) {
  structure(
    entries,
    class = "coverage",
    root = repo_root,
    package = list(package = "artma", path = repo_root)
  )
}

inst_coverage <- .ic_as_coverage(coverage[startsWith(entry_paths, inst_prefix)])
# RcppExports.R is generated glue; covr::package_coverage() excludes it by
# default, so keep the R/ report aligned with what codecov saw before.
keep_pkg <- startsWith(entry_paths, r_prefix) &
  entry_paths != paste0(r_prefix, "RcppExports.R")
pkg_coverage <- .ic_as_coverage(coverage[keep_pkg])

if (length(inst_coverage) == 0) {
  cli::cli_abort("No inst/artma coverage was recorded; the tracer likely failed to attach.")
}
if (length(pkg_coverage) == 0) {
  cli::cli_abort("No R/ coverage was recorded; the namespace tracer likely failed to attach.")
}

#' Print a per-file coverage table plus the overall percentage.
#'
#' @param coverage A `coverage` object.
#' @param title *[character]* Heading for the section.
.ic_report <- function(coverage, title) {
  overall <- covr::percent_coverage(coverage)
  by_line <- covr::tally_coverage(coverage, by = "line")
  per_file <- aggregate(value ~ filename, data = by_line, FUN = function(x) round(mean(x > 0) * 100, 1))
  per_file <- per_file[order(per_file$value, per_file$filename), ]
  names(per_file) <- c("file", "coverage_pct")

  cli::cli_h1(title)
  print(per_file, row.names = FALSE, right = FALSE)
  cli::cli_alert_success("Overall {title}: {round(overall, 2)}%")
}

.ic_report(pkg_coverage, "R/ line coverage")
.ic_report(inst_coverage, "inst/artma line coverage")

if (identical(Sys.getenv("ARTMA_COVERAGE_COBERTURA"), "true")) {
  covr::to_cobertura(pkg_coverage, filename = "cobertura.xml")
  covr::to_cobertura(inst_coverage, filename = "cobertura-inst.xml")
  cli::cli_alert_success("Wrote {.file cobertura.xml} and {.file cobertura-inst.xml}")
}

rds_path <- Sys.getenv("ARTMA_COVERAGE_RDS")
if (nzchar(rds_path)) {
  combined <- .ic_as_coverage(coverage[startsWith(entry_paths, inst_prefix) | keep_pkg])
  saveRDS(combined, rds_path)
  cli::cli_alert_success("Saved coverage object to {.file {rds_path}}")
}
