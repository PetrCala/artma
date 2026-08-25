box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_null,
    expect_true,
    skip_if_not_installed,
    test_that
  ]
)

box::use(
  artma / data / external_mapping[
    build_mapping_request,
    external_mapper_command,
    external_mapping_proposals,
    parse_mapper_response,
    verify_external_proposal
  ],
  artma / data / interactive_mapping[interactive_column_mapping]
)

# The opt-in external mapping hook. With no command configured none of this
# runs; with one configured, nothing it proposes is trusted before it passes
# the same checks an auto-detected candidate faces.

#' A dataset whose effect column carries no name signal, next to the
#' identifier-flavored decoys a proposal must not be allowed to pick.
make_hook_df <- function(n_studies = 20, k = 5) {
  study <- rep(seq_len(n_studies), each = k)
  n <- length(study)
  effect <- round(stats::rnorm(n, 0.2, 0.5), 3)

  data.frame(
    study = rep(sprintf("Author%02d", seq_len(n_studies)), each = k),
    idcoeff = rep(seq_len(k), times = n_studies),
    eis = effect,
    se = pmax(round(exp(stats::rnorm(n, log(0.15), 0.5)), 3), 0.001),
    nobs = rep(sample(80:2000, n_studies, replace = TRUE), each = k)
  )
}

#' The mapping recognition arrives at on that dataset: everything but effect.
hook_mapping <- function() {
  list(se = "se", study_id = "study", n_obs = "nobs")
}

#' Write an Rscript stub that reads the payload on stdin and behaves as `body`
#' says. Returns the command vector to configure the hook with.
write_stub <- function(dir, name, body) {
  path <- file.path(dir, name)
  writeLines(c('input <- paste(readLines(file("stdin"), warn = FALSE), collapse = "")', body), path)
  c(file.path(R.home("bin"), "Rscript"), "--vanilla", path)
}

#' Run the hook against a stub command and return the roles it accepted.
propose_with <- function(df, command) {
  external_mapping_proposals(
    df = df,
    mapping = hook_mapping(),
    roles = "effect",
    declined = list(effect = list(role = "effect", reason = "no candidate cleared the bar", candidates = list())),
    command = command,
    timeout = 60
  )
}


test_that("nothing runs when no external command is configured", {
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  df <- make_hook_df()

  expect_null(external_mapper_command(NULL))
  expect_null(external_mapper_command(NA_character_))
  expect_null(external_mapper_command(" "))

  proposals <- external_mapping_proposals(
    df = df,
    mapping = hook_mapping(),
    roles = "effect",
    command = external_mapper_command(NULL),
    run_fn = function(...) stop("the hook must not run a command when it is off")
  )
  expect_equal(proposals, list())

  # And the mapping flow is untouched: the declined role stays unmapped.
  result <- interactive_column_mapping(
    df = df,
    auto_mapping = hook_mapping(),
    is_interactive = FALSE
  )
  expect_false("effect" %in% names(result))
})


test_that("a command line is split into an executable and its arguments", {
  expect_equal(external_mapper_command("Rscript --vanilla mapper.R"), c("Rscript", "--vanilla", "mapper.R"))
  expect_equal(external_mapper_command('"/opt/my tools/Rscript" mapper.R'), c("/opt/my tools/Rscript", "mapper.R"))
  # Already split by the caller: taken as is.
  expect_equal(external_mapper_command(c("Rscript", "mapper.R")), c("Rscript", "mapper.R"))

  # And it is the option that turns the hook on.
  withr::local_options(list("artma.data.mapping.external_command" = "Rscript mapper.R"))
  expect_equal(external_mapper_command(), c("Rscript", "mapper.R"))
})


test_that("a verified proposal is accepted", {
  skip_if_not_installed("jsonlite")
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  dir <- withr::local_tempdir()
  df <- make_hook_df()

  command <- write_stub(dir, "good.R", 'cat(\'{"schema":"artma.column_mapping_response/1","mappings":{"effect":"eis"}}\')')
  proposals <- propose_with(df, command)

  expect_equal(names(proposals), "effect")
  expect_equal(proposals$effect$column, "eis")
  expect_equal(proposals$effect$source, "external")
  # It read the payload rather than ignoring stdin.
  expect_true(proposals$effect$evidence > 0.5)
})


test_that("an implausible proposal is rejected", {
  skip_if_not_installed("jsonlite")
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  dir <- withr::local_tempdir()
  df <- make_hook_df()

  # idcoeff is a per-study counter: whole numbers, no measured values in it.
  command <- write_stub(dir, "bad.R", 'cat(\'{"mappings":{"effect":"idcoeff"}}\')')

  expect_equal(propose_with(df, command), list())
})


test_that("malformed output falls back to the decline", {
  skip_if_not_installed("jsonlite")
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  dir <- withr::local_tempdir()
  df <- make_hook_df()

  command <- write_stub(dir, "malformed.R", 'cat("effect -> eis, probably")')

  expect_equal(propose_with(df, command), list())
})


test_that("a nonzero exit falls back to the decline", {
  skip_if_not_installed("jsonlite")
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  dir <- withr::local_tempdir()
  df <- make_hook_df()

  command <- write_stub(dir, "fails.R", c('cat("no api key\\n", file = stderr())', "quit(status = 3)"))

  expect_equal(propose_with(df, command), list())
})


test_that("a timeout falls back to the decline", {
  skip_if_not_installed("jsonlite")
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  dir <- withr::local_tempdir()
  df <- make_hook_df()

  command <- write_stub(dir, "slow.R", c("Sys.sleep(30)", 'cat(\'{"mappings":{"effect":"eis"}}\')'))

  proposals <- external_mapping_proposals(
    df = df,
    mapping = hook_mapping(),
    roles = "effect",
    command = command,
    timeout = 1
  )
  expect_equal(proposals, list())
})


test_that("the payload carries column names and summaries, never raw values", {
  skip_if_not_installed("jsonlite")
  withr::local_seed(11)
  df <- make_hook_df()

  request <- build_mapping_request(
    df = df,
    mapping = hook_mapping(),
    declined = list(effect = list(role = "effect", reason = "no candidate cleared the bar")),
    roles = "effect"
  )

  expect_equal(request$schema, "artma.column_mapping_request/1")
  expect_equal(request$requested_roles, "effect")
  expect_equal(request$dataset$n_rows, nrow(df))
  expect_equal(vapply(request$columns, function(col) col$name, character(1)), names(df))
  expect_equal(names(request$declined), "effect")

  eis <- Filter(function(col) identical(col$name, "eis"), request$columns)[[1]]
  expect_equal(eis$n, nrow(df))
  expect_equal(eis$coverage, 1)
  expect_true(eis$numeric)
  expect_equal(eis$quantiles$median, stats::median(df$eis))

  # No column of the payload holds the values themselves: a summary is at most
  # a handful of numbers, never a row per observation.
  json <- as.character(jsonlite::toJSON(request, auto_unbox = TRUE, na = "null"))
  for (value in as.character(df$study)) {
    expect_false(grepl(sprintf('"%s"', value), json, fixed = TRUE))
  }
})


test_that("proposals are verified against the data, not taken on trust", {
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  df <- make_hook_df()
  mapping <- hook_mapping()

  expect_true(verify_external_proposal(df, "effect", "eis", mapping)$ok)

  unknown <- verify_external_proposal(df, "effect", "not_a_column", mapping)
  expect_false(unknown$ok)
  expect_true(grepl("no column named", unknown$reason))

  counter <- verify_external_proposal(df, "effect", "idcoeff", mapping)
  expect_false(counter$ok)
  expect_true(grepl("value plausibility", counter$reason))

  taken <- verify_external_proposal(df, "effect", "se", mapping)
  expect_false(taken$ok)
  expect_true(grepl("already mapped", taken$reason))

  # A column of plausible measurements that does not pair with the mapped se.
  df$noise <- round(stats::rnorm(nrow(df), 0, 1e6), 3)
  pair <- verify_external_proposal(df, "effect", "noise", mapping)
  expect_false(pair$ok)
  expect_true(grepl("pair consistency", pair$reason))
})


test_that("both documented response shapes are read", {
  skip_if_not_installed("jsonlite")
  withr::local_options(list("artma.verbose" = 1))

  expect_equal(
    parse_mapper_response('{"mappings":{"effect":"eis"}}'),
    c(effect = "eis")
  )
  expect_equal(
    parse_mapper_response('{"effect":"eis","n_obs":"nobs"}'),
    c(effect = "eis", n_obs = "nobs")
  )
  expect_equal(
    parse_mapper_response('{"mappings":{"effect":{"column":"eis","confidence":0.8}}}'),
    c(effect = "eis")
  )
  expect_null(parse_mapper_response("not json at all"))
  expect_null(parse_mapper_response('{"mappings":{}}'))
})


test_that("a verified proposal is applied non-interactively and confirmed interactively", {
  withr::local_seed(11)
  withr::local_options(list("artma.verbose" = 1))
  df <- make_hook_df()
  stub <- function(...) '{"mappings":{"effect":"eis"}}'

  hook <- function(df, mapping, roles, declined) {
    external_mapping_proposals(
      df = df,
      mapping = mapping,
      roles = roles,
      declined = declined,
      command = c("mapper"),
      run_fn = stub
    )
  }

  # Non-interactive: configuring the command is the consent.
  applied <- interactive_column_mapping(
    df = df,
    auto_mapping = hook_mapping(),
    is_interactive = FALSE,
    external_fn = hook
  )
  expect_equal(applied$effect, "eis")

  # Interactive: the same proposal becomes one confirmation question.
  asked <- list()
  confirmed <- interactive_column_mapping(
    df = df,
    auto_mapping = hook_mapping(),
    is_interactive = TRUE,
    external_fn = hook,
    select_fn = function(choices, prompt) {
      asked[[length(asked) + 1]] <<- prompt
      choices[1]
    }
  )
  expect_equal(confirmed$effect, "eis")
  expect_true(any(grepl("effect", unlist(asked), fixed = TRUE)))
})
