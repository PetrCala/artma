box::use(
  testthat[expect_equal, expect_true, skip_if_not, test_that]
)

# The fixtures and other package modules must be loaded here separately to avoid linter issues
box::use(testing / fixtures / index[FIXTURES])

# This file guards against drift between the options template and the option
# keys the code actually consumes. Both directions are asserted:
#   1. every `artma.*` key read in the sources exists in the template, and
#   2. every template leaf is consumed somewhere in the sources.
#
# Consumption evidence is collected with a grep over the package sources:
#   - literal reads:        getOption("artma.<key>")
#   - constant-built reads: paste0(CONST$PACKAGE_NAME, ".<key>")
#   - group reads:          get_option_group("artma.<prefix>") counts for
#     every template leaf under <prefix>
# The bare namespace read `get_option_group("artma")` (used by the cache
# signature builder to enumerate all options generically) is deliberately not
# counted as evidence that any specific leaf is consumed.
#
# Keys the code reads at runtime that are deliberately absent from the
# template (the template reader strips the `temp` block, and the rest are
# session state never stored in an options file):
#   - temp.*            scratch values populated by options.load
#   - options_file_name populated when an options file is loaded
#   - welcome.shown     session flag set after the welcome screen is shown
# Note: data.config, data.source_path, and data.expected_schema_columns are
# also runtime-populated, but they remain template leaves, so they need no
# exemption here.
runtime_only_option_keys <- c("options_file_name", "welcome.shown")
runtime_only_option_prefixes <- c("temp.")

# Template leaves consumed outside the R options namespace:
#   - cli.editor is read straight from the YAML options file by
#     resolve_cli_editor() before any options are loaded
leaves_consumed_elsewhere <- c("cli.editor")

package_root <- function() {
  normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = FALSE)
}

parity_source_files <- function(root) {
  dirs <- c(file.path(root, "R"), file.path(root, "inst", "artma"))
  unlist(lapply(
    dirs,
    list.files,
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  ))
}

extract_keys <- function(lines, pattern) {
  hits <- unlist(
    regmatches(lines, gregexpr(pattern, lines, perl = TRUE)),
    use.names = FALSE
  )
  if (length(hits) == 0L) {
    return(character())
  }
  sub(pattern, "\\1", hits, perl = TRUE)
}

collect_consumed_option_keys <- function(files) {
  exact_pattern <- "getOption\\(\\s*\"artma\\.([A-Za-z0-9_.]+)\""
  const_pattern <- "paste0\\(CONST\\$PACKAGE_NAME,\\s*\"\\.([A-Za-z0-9_.]+)\"\\)"
  group_pattern <- "get_option_group\\(\\s*\"artma\\.([A-Za-z0-9_.]+)\""

  exact <- character()
  groups <- character()

  for (file in files) {
    lines <- readLines(file, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines)] # skip comments and roxygen docs

    exact <- c(
      exact,
      extract_keys(lines, exact_pattern),
      extract_keys(lines, const_pattern)
    )
    groups <- c(groups, extract_keys(lines, group_pattern))
  }

  list(
    exact = sort(unique(exact)),
    groups = sort(unique(groups))
  )
}

find_parity_violations <- function(leaves,
                                   exact_keys,
                                   group_prefixes,
                                   runtime_only_keys = runtime_only_option_keys,
                                   runtime_only_prefixes = runtime_only_option_prefixes,
                                   consumed_elsewhere = leaves_consumed_elsewhere) {
  is_runtime_only <- vapply(
    exact_keys,
    function(key) {
      key %in% runtime_only_keys ||
        any(startsWith(key, runtime_only_prefixes))
    },
    logical(1)
  )
  unknown_consumed <- setdiff(exact_keys[!is_runtime_only], leaves)

  group_has_leaves <- vapply(
    group_prefixes,
    function(prefix) any(leaves == prefix | startsWith(leaves, paste0(prefix, "."))),
    logical(1)
  )
  orphan_groups <- group_prefixes[!group_has_leaves]

  leaf_consumed <- vapply(
    leaves,
    function(leaf) {
      leaf %in% exact_keys ||
        leaf %in% consumed_elsewhere ||
        any(leaf == group_prefixes | startsWith(leaf, paste0(group_prefixes, ".")))
    },
    logical(1)
  )
  unconsumed_leaves <- leaves[!leaf_consumed]

  list(
    unknown_consumed = sort(unique(unknown_consumed)),
    orphan_groups = sort(unique(orphan_groups)),
    unconsumed_leaves = sort(unique(unconsumed_leaves))
  )
}

test_that("the parity checker flags drift in both directions", {
  leaves <- c("cache.max_age", "methods.demo.round_to", "output.dir")

  # an orphan consumed key, an orphan group read, and an unconsumed leaf
  violations <- find_parity_violations(
    leaves = leaves,
    exact_keys = c("cache.max_age", "cache.legacy_age"),
    group_prefixes = c("methods.demo", "methods.ghost"),
    runtime_only_keys = character(),
    runtime_only_prefixes = character(),
    consumed_elsewhere = character()
  )

  expect_equal(violations$unknown_consumed, "cache.legacy_age")
  expect_equal(violations$orphan_groups, "methods.ghost")
  expect_equal(violations$unconsumed_leaves, "output.dir")

  # full coverage in both directions reports no violations
  clean <- find_parity_violations(
    leaves = leaves,
    exact_keys = c("cache.max_age", "output.dir"),
    group_prefixes = "methods.demo",
    runtime_only_keys = character(),
    runtime_only_prefixes = character(),
    consumed_elsewhere = character()
  )

  expect_equal(clean$unknown_consumed, character(0))
  expect_equal(clean$orphan_groups, character(0))
  expect_equal(clean$unconsumed_leaves, character(0))
})

test_that("template leaves and consumed option keys stay in parity", {
  box::use(artma / options / template[collect_leaf_paths])

  root <- package_root()
  template_path <- file.path(
    root, "inst", "artma", "options", "templates", "options_template.yaml"
  )

  skip_if_not(
    dir.exists(file.path(root, "R")) && file.exists(template_path),
    "package sources are not available"
  )

  leaves <- collect_leaf_paths(template_path)
  consumed <- collect_consumed_option_keys(parity_source_files(root))

  violations <- find_parity_violations(
    leaves = leaves,
    exact_keys = consumed$exact,
    group_prefixes = consumed$groups
  )

  expect_equal(violations$unknown_consumed, character(0))
  expect_equal(violations$orphan_groups, character(0))
  expect_equal(violations$unconsumed_leaves, character(0))
})

test_that("options files predating the cache.max_age rename reconcile to the default", {
  box::use(artma[options.load, options.validate])

  FIXTURES$local_cli_silence()

  # pin the template to this checkout so the test does not depend on the
  # template shipped with whichever artma build happens to be installed
  template_path <- file.path(
    package_root(), "inst", "artma", "options", "templates", "options_template.yaml"
  )
  skip_if_not(file.exists(template_path), "package sources are not available")

  tmp_dir <- withr::local_tempdir()
  # The legacy file predates the cache.max_age rename (it still carries
  # cache.cache_age). The required-without-default options are supplied so the
  # pure load does not abort on them; the point here is the cache.max_age default.
  legacy <- list(
    cache = list(cache_age = 100L),
    data = list(
      source_path = "data.csv",
      colnames = list(
        study_id = "study",
        effect = "effect",
        se = "se",
        n_obs = "n_obs"
      )
    )
  )
  yaml::write_yaml(legacy, file.path(tmp_dir, "legacy.yaml"))

  # validation surfaces the renamed key as missing
  errors <- options.validate(
    options_file_name = "legacy.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    failure_action = "return_errors_quiet"
  )
  missing_names <- vapply(
    Filter(function(err) identical(err$type, "missing_option"), errors),
    function(err) err$opt_def$name,
    character(1)
  )
  expect_true("cache.max_age" %in% missing_names)

  # loading applies the template default; the legacy value does not leak in
  loaded <- options.load(
    options_file_name = "legacy.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    should_validate = TRUE,
    should_return = TRUE
  )
  expect_equal(loaded$`artma.cache.max_age`, 3600)
})
