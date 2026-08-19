box::use(
  testthat[expect_error, expect_identical, expect_true, skip, test_that]
)

pkg_root <- function() testthat::test_path("..", "..")

# `.chglog/config.yml` and `scripts/` are .Rbuildignore'd, so these tests only
# run against a checkout, not against an installed or built package.
skip_unless_exists <- function(path, label) {
  if (!file.exists(path)) {
    skip(sprintf("%s is not available (expected when testing an installed/built package)", label))
  }
  path
}

source_release_notes <- function() {
  script_path <- skip_unless_exists(
    file.path(pkg_root(), "scripts", "R", "get_release_notes.R"),
    "scripts/R/get_release_notes.R"
  )
  env <- new.env()
  source(script_path, local = env) # nolint: undesirable_function_linter.
  env
}

news_section <- function(tag, ..., date = "2026-01-01") {
  c(
    "",
    sprintf("<a name=\"%s\"></a>", tag),
    "",
    sprintf("## [%s](https://github.com/PetrCala/artma/compare/v0.0.0...%s)", tag, tag),
    "",
    sprintf("> %s", date),
    "",
    ...
  )
}

test_that("the git-chglog header pattern captures scoped conventional commits", {
  config_path <- skip_unless_exists(
    file.path(pkg_root(), ".chglog", "config.yml"),
    ".chglog/config.yml"
  )

  config <- yaml::read_yaml(config_path)
  pattern <- config$options$header$pattern

  expect_identical(config$options$header$pattern_maps, c("Type", "Scope", "Subject"))

  # The scope group is what broke the changelog historically: a pattern
  # without it silently drops every `type(scope): subject` commit, which is
  # nearly all of them.
  cases <- list(
    list(header = "fix: keep lintr in Suggests", type = "fix", scope = "", subject = "keep lintr in Suggests"),
    list(header = "fix(rng): strip the reseed", type = "fix", scope = "rng", subject = "strip the reseed"),
    list(header = "feat(p_hacking_tests): add chunking", type = "feat", scope = "p_hacking_tests", subject = "add chunking"),
    list(header = "refactor(p-hacking): extract helpers", type = "refactor", scope = "p-hacking", subject = "extract helpers"),
    list(header = "perf(linear)!: drop the loop", type = "perf", scope = "linear", subject = "drop the loop")
  )

  for (case in cases) {
    groups <- regmatches(case$header, regexec(pattern, case$header, perl = TRUE))[[1]]
    expect_identical(length(groups), 4L, info = case$header)
    expect_identical(groups[[2]], case$type, info = case$header)
    expect_identical(groups[[3]], case$scope, info = case$header)
    expect_identical(groups[[4]], case$subject, info = case$header)
  }
})

test_that("extract_release_notes returns the requested section without its heading", {
  env <- source_release_notes()

  lines <- c(
    news_section("v1.1.0", "### Features", "", "* **api:** add a thing", ""),
    news_section("v1.0.0", "### Bug Fixes", "", "* fix a thing", "")
  )

  expect_identical(
    env$extract_release_notes(lines, "v1.1.0"),
    c("### Features", "", "* **api:** add a thing")
  )
  expect_identical(
    env$extract_release_notes(lines, "v1.0.0"),
    c("### Bug Fixes", "", "* fix a thing")
  )
})

test_that("extract_release_notes fails loudly on an empty section", {
  env <- source_release_notes()

  lines <- c(news_section("v1.1.0", "", ""), news_section("v1.0.0", "### Bug Fixes", "", "* fix a thing"))

  expect_error(env$extract_release_notes(lines, "v1.1.0"), "No release notes found")
  expect_error(env$extract_release_notes(lines, "v9.9.9"), "Could not find anchor tag")
})

test_that("NEWS.md carries release notes for the current package version", {
  news_path <- skip_unless_exists(file.path(pkg_root(), "NEWS.md"), "NEWS.md")
  env <- source_release_notes()

  version <- unname(read.dcf(file.path(pkg_root(), "DESCRIPTION"), fields = "Version")[1, 1])
  notes <- env$extract_release_notes(readLines(news_path, warn = FALSE), paste0("v", version))

  expect_true(any(grepl("^\\*", notes)))
})
