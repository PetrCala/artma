box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_true,
    expect_match,
    expect_named,
    expect_null,
    test_that
  ]
)

# Drive cli.run in-process, capturing both streams and the returned exit code.
run_cli <- function(args) {
  outcon <- textConnection("outbuf", "w", local = TRUE)
  errcon <- textConnection("errbuf", "w", local = TRUE)
  sink(outcon, type = "output")
  sink(errcon, type = "message")
  code <- tryCatch(
    artma::cli.run(args),
    finally = {
      sink(type = "message")
      sink(type = "output")
    }
  )
  close(outcon)
  close(errcon)
  list(
    code = code,
    stdout = paste(outbuf, collapse = "\n"),
    stderr = paste(errbuf, collapse = "\n")
  )
}

# --- Parser ----------------------------------------------------------------

test_that("cli_parse resolves subcommands, sub-actions and flag forms", {
  box::use(artma / cli / parser[cli_parse])

  # --key value and --key=value are equivalent.
  a <- cli_parse(c("run", "--options", "x.yaml", "--verbose", "4"))
  b <- cli_parse(c("run", "--options=x.yaml", "--verbose=4"))
  expect_equal(a$action, "dispatch")
  expect_equal(a$subcommand, "run")
  expect_equal(a$flags[["options"]], "x.yaml")
  expect_equal(a$flags[["verbose"]], 4L)
  expect_equal(a$flags, b$flags)

  # --methods is a comma-separated list.
  m <- cli_parse(c("run", "--methods", "funnel_plot,effect_summary_stats"))
  expect_equal(m$flags[["methods"]], c("funnel_plot", "effect_summary_stats"))

  # boolean flags take no value.
  f <- cli_parse(c("run", "--no-cache", "--json"))
  expect_true(f$flags[["no-cache"]])
  expect_true(f$flags[["json"]])

  # options requires a valid sub-action.
  ok <- cli_parse(c("options", "validate", "--options", "x.yaml"))
  expect_equal(ok$action, "dispatch")
  expect_equal(ok$subaction, "validate")
})

test_that("cli_parse reports usage errors without throwing", {
  box::use(artma / cli / parser[cli_parse])

  expect_equal(cli_parse(c("frobnicate"))$action, "error")
  expect_equal(cli_parse(c("run", "--nope"))$action, "error")
  expect_equal(cli_parse(c("run", "--verbose", "high"))$action, "error")
  expect_equal(cli_parse(c("run", "--no-cache=1"))$action, "error")
  expect_equal(cli_parse(c("run", "--options"))$action, "error") # missing value
  expect_equal(cli_parse(c("options", "frobnicate"))$action, "error") # bad sub-action
  expect_equal(cli_parse(c("options"))$action, "error") # missing sub-action
})

test_that("cli_parse treats --help anywhere as a help request", {
  box::use(artma / cli / parser[cli_parse])

  expect_equal(cli_parse(character(0))$action, "help")
  expect_equal(cli_parse("--help")$action, "help")
  top <- cli_parse("--help")
  expect_null(top$subcommand)
  sub <- cli_parse(c("run", "--help"))
  expect_equal(sub$action, "help")
  expect_equal(sub$subcommand, "run")
})

# --- Help ------------------------------------------------------------------

test_that("cli_help_text documents every subcommand and its flags", {
  box::use(artma / cli / parser[cli_help_text, cli_subcommands])

  top <- paste(cli_help_text(NULL), collapse = "\n")
  for (nm in names(cli_subcommands())) {
    expect_match(top, nm, fixed = TRUE)
  }

  run_help <- paste(cli_help_text("run"), collapse = "\n")
  for (flag in c("--options", "--data", "--methods", "--no-cache", "--report", "--json")) {
    expect_match(run_help, flag, fixed = TRUE)
  }
})

test_that("cli.run prints help to stdout and returns 0 for each subcommand", {
  for (sc in c("--help", "run", "methods", "options", "version")) {
    res <- run_cli(c(sc, "--help"))
    expect_equal(res$code, 0L)
    expect_true(nchar(res$stdout) > 0)
  }
})

# --- Exit-code contract ----------------------------------------------------

test_that("cli.run returns 2 and prints usage to stderr on a usage error", {
  res <- run_cli(c("run", "--bogus"))
  expect_equal(res$code, 2L)
  expect_match(res$stderr, "Unknown flag", fixed = TRUE)
  expect_equal(nchar(res$stdout), 0L)
})

# --- Overlay ---------------------------------------------------------------

test_that("build_option_overlay maps run flags to artma.* options", {
  box::use(artma / cli / parser[build_option_overlay])

  overlay <- build_option_overlay(list(
    data = "/tmp/data.csv",
    `output-dir` = "/tmp/out",
    verbose = 2L,
    `no-cache` = TRUE,
    report = TRUE
  ))

  expect_equal(overlay[["artma.data.source_path"]], "/tmp/data.csv")
  expect_equal(overlay[["artma.output.dir"]], "/tmp/out")
  expect_equal(overlay[["artma.verbose"]], 2L)
  expect_false(overlay[["artma.cache.use_cache"]])
  expect_true(overlay[["artma.output.report"]])
})

test_that("the --no-cache overlay is scoped to the call and unset outside", {
  box::use(artma / cli / parser[build_option_overlay])

  withr::local_options(list(artma.cache.use_cache = NULL))
  overlay <- build_option_overlay(list(`no-cache` = TRUE))

  inside <- withr::with_options(overlay, getOption("artma.cache.use_cache", TRUE))
  expect_false(inside)
  # Outside the overlay the option is untouched (back to its default sentinel).
  expect_null(getOption("artma.cache.use_cache"))
})

test_that("an empty flag set yields an empty overlay", {
  box::use(artma / cli / parser[build_option_overlay])
  expect_equal(build_option_overlay(list()), list())
})

# --- Dispatch --------------------------------------------------------------

test_that("cli.run version prints the package version to stdout", {
  res <- run_cli("version")
  expect_equal(res$code, 0L)
  expect_match(res$stdout, as.character(utils::packageVersion("artma")), fixed = TRUE)
})

test_that("cli.run methods dispatches to methods.list without error", {
  res <- run_cli("methods")
  expect_equal(res$code, 0L)
})
