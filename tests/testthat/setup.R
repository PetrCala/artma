# Make box imports resolve correctly during tests, ahead of any box.path
# inherited from the environment (e.g. an .Rprofile pointing at a different
# checkout). Runs in every testthat worker before test files load, so the tests
# always exercise the code they sit next to.
#
# Two roots are prepended:
#   * this checkout's inst/ directory, so `artma/...` imports resolve to the
#     code under test (used by `make test` / devtools; under R CMD check the
#     installed package resolves these instead and this directory is absent).
#   * tests/testthat/modules, the home of the test-only scaffolding
#     (`testing/...`) that no longer ships inside the installed package. This
#     directory travels with tests/ into the R CMD check tarball, so the
#     scaffolding resolves there in that context too.
local({
  candidate_paths <- c(
    testthat::test_path("..", "..", "inst"),
    testthat::test_path("modules")
  )
  extra_paths <- normalizePath(candidate_paths, winslash = "/", mustWork = FALSE)
  extra_paths <- extra_paths[dir.exists(extra_paths)]
  if (length(extra_paths) > 0) {
    options(box.path = unique(c(extra_paths, getOption("box.path"))))
  }
})

# Suppress incidental console output for the whole parallel test run (`make
# test`), so it shows only testthat's own per-file/per-run summary - no
# interleaved `> file: text` lines. Every genuine test signal survives this
# untouched: testthat detects and reports failures, errors, and uncaught
# warnings via a structured protocol between the worker and the parent
# process, not by writing to stdout/stderr, and
# expect_message()/expect_warning()/capture_messages()/capture_warnings()
# install their own inner condition handlers, which R always consults before
# these sinks ever come into play - so assertions on message/warning content
# keep working exactly as before (verified directly, not just by reading the
# docs).
#
# Gated to parallel workers only. `devtools::test_active_file()` /
# `make test-file` run in the *current* R session rather than a spawned
# worker, so testthat's own progress/summary output shares this session's
# stdout/stderr too - sinking it there would silence the pass/fail summary
# along with everything else, not just the noise. `callr_startup_hook` only
# ever appears on the call stack inside a callr-spawned worker process
# (confirmed by dumping sys.calls() from both a `make test` worker and a
# `test_active_file()` run), so it reliably tells the two apart; there is no
# public testthat API for this and the env var that marks "am I parallel"
# (`TESTTHAT_IS_PARALLEL`) isn't set yet at the point setup.R runs.
if (any(vapply(sys.calls(), function(call) identical(as.character(call[[1]]), "callr_startup_hook"), logical(1)))) {
  # Two separate sinks, because "output" and "message" behave differently:
  #   * "output" (print()/cat()-based content, e.g. cli::cat_print()) properly
  #     stacks - nested capture.output()/sink() calls elsewhere in the suite
  #     push and pop their own layer on top of this one without disturbing it.
  #   * "message" (message()/warning()-based content, which is how
  #     essentially all cli:: output and every base R warning ultimately gets
  #     printed) does NOT stack - R supports only one active diversion at a
  #     time (see `?sink`, "type = 'message'"). A second `sink(type =
  #     "message")` call anywhere - including a bare `capture.output(x, type
  #     = "message")` - silently replaces this one instead of layering on it,
  #     and popping that second one then removes this one too rather than
  #     restoring it. That ruled out `capture.output(fn(), type = "message")`
  #     as a per-test-file suppression technique (an earlier version of this
  #     branch leaned on it): it works in isolation but corrupts this sink
  #     the first time any test file happens to run afterward in the same
  #     worker session, permanently reverting the rest of that worker's
  #     queue to raw console output.
  #
  # Condition-based alternatives (suppressMessages(), capture_messages(),
  # testthat's own expect_message()/expect_warning()) don't call sink() at
  # all, so they compose safely with this - use those instead of
  # `capture.output(..., type = "message")` in new tests. The one exception
  # is `tests/testthat/test-cli.R`, which legitimately needs to capture raw
  # stdout/stderr to test `cli_run()`'s actual command-line behavior; it
  # explicitly suspends and restores this sink around its own capture (see
  # the comment there) rather than nesting inside it.
  sink(file(nullfile(), open = "wt"), type = "output")

  .artma_test_null_message <- file(nullfile(), open = "wt")
  sink(.artma_test_null_message, type = "message")
  options(artma.test.null_message_con = .artma_test_null_message)
}
