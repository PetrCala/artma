box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_length,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(
  artma / interactive / hub[
    adjustable_option_defs,
    compose_hub_choices,
    count_studies,
    describe_option_state,
    hub_menu_items,
    merge_run_results,
    option_affects_data,
    run_adjust_options,
    run_session_hub
  ]
)

# A prepared frame small enough to summarize in tests.
hub_df <- function() {
  data.frame(
    study_id = c("s1", "s1", "s2"),
    effect = c(0.5, 0.3, 0.7),
    se = c(0.1, 0.15, 0.12),
    stringsAsFactors = FALSE
  )
}

# The columns of a build_methods_table(available_for = ...) frame the picker
# reads.
hub_methods_frame <- function() {
  data.frame(
    method = c("bma", "funnel_plot"),
    description = c("Bayesian model averaging", "Funnel plot"),
    missing_packages = c("", ""),
    opt_in = c(FALSE, FALSE),
    missing_columns = c("", ""),
    stringsAsFactors = FALSE
  )
}

# Sequenced single-select backend: each call consumes the next entry of the
# script and returns the first label containing it; NA cancels the menu.
make_select_fn <- function(script) {
  index <- 0L
  function(choices, prompt, selected = NULL) {
    index <<- index + 1L
    stopifnot(index <= length(script))
    pattern <- script[[index]]
    if (is.na(pattern)) {
      return(character(0))
    }
    matches <- choices[grepl(pattern, cli::ansi_strip(choices), fixed = TRUE)]
    stopifnot(length(matches) >= 1L)
    matches[[1]]
  }
}

# Checkbox backend selecting methods by name (the first token of each label);
# counts its invocations so tests can assert the picker was not reopened.
make_checkbox_fn <- function(methods, counter = new.env()) {
  counter$calls <- 0L
  fn <- function(choices, prompt, selected, allow_select_all) {
    counter$calls <- counter$calls + 1L
    first_tokens <- sub(" .*", "", cli::ansi_strip(choices))
    choices[first_tokens %in% methods]
  }
  list(fn = fn, counter = counter)
}

# Stubbed run pipeline: records the selections it was handed and returns one
# "<method>-result" per method with the run_info attribute execute_run() sets.
make_run_methods <- function(log = new.env()) {
  log$calls <- list()
  fn <- function(methods) {
    log$calls <- c(log$calls, list(methods))
    results <- stats::setNames(
      lapply(methods, function(m) paste0(m, "-result")),
      methods
    )
    attr(results, "run_info") <- list(
      methods_requested = methods,
      seed = 42L,
      output_files = list()
    )
    results
  }
  list(fn = fn, log = log)
}

abort_if_called <- function(what) {
  function(...) stop(what, " must not be called")
}

test_that("exit before any run returns an empty result list cleanly", {
  withr::local_options(list(artma.temp.last_methods = NULL))

  returned <- withVisible(suppressMessages(run_session_hub(
    df = hub_df(),
    run_methods = abort_if_called("run_methods"),
    methods_table = hub_methods_frame(),
    select_fn = make_select_fn("Exit"),
    checkbox_fn = abort_if_called("checkbox_fn"),
    width = 100
  )))

  expect_false(returned$visible)
  expect_equal(unclass(returned$value), list(), ignore_attr = TRUE)
  expect_equal(attr(returned$value, "runs"), list())
})

test_that("a cancelled menu behaves like Exit", {
  withr::local_options(list(artma.temp.last_methods = NULL))

  results <- suppressMessages(run_session_hub(
    df = hub_df(),
    run_methods = abort_if_called("run_methods"),
    methods_table = hub_methods_frame(),
    select_fn = make_select_fn(NA_character_),
    checkbox_fn = abort_if_called("checkbox_fn"),
    width = 100
  ))

  expect_length(results, 0L)
  expect_equal(attr(results, "runs"), list())
})

test_that("run then exit returns the results with the runs attribute", {
  withr::local_options(list(artma.temp.last_methods = NULL))
  checkbox <- make_checkbox_fn("bma")
  run <- make_run_methods()

  results <- suppressMessages(run_session_hub(
    df = hub_df(),
    run_methods = run$fn,
    methods_table = hub_methods_frame(),
    select_fn = make_select_fn(c("Run methods", "Exit")),
    checkbox_fn = checkbox$fn,
    width = 100
  ))

  expect_equal(results$bma, "bma-result")
  expect_equal(run$log$calls, list("bma"))

  runs <- attr(results, "runs")
  expect_length(runs, 1L)
  expect_equal(runs[[1]]$methods, "bma")
  expect_equal(runs[[1]]$seed, 42L)
  expect_true(inherits(runs[[1]]$timestamp, "POSIXct"))

  # The confirmed selection is remembered for the session's next pick.
  expect_equal(getOption("artma.temp.last_methods"), "bma")
})

test_that("re-run reuses the last selection without reopening the picker", {
  withr::local_options(list(artma.temp.last_methods = NULL))
  checkbox <- make_checkbox_fn(c("bma", "funnel_plot"))
  run <- make_run_methods()

  results <- suppressMessages(run_session_hub(
    df = hub_df(),
    run_methods = run$fn,
    methods_table = hub_methods_frame(),
    select_fn = make_select_fn(c("Run methods", "Re-run last selection", "Exit")),
    checkbox_fn = checkbox$fn,
    width = 100
  ))

  expect_equal(checkbox$counter$calls, 1L)
  expect_equal(run$log$calls, list(c("bma", "funnel_plot"), c("bma", "funnel_plot")))
  expect_length(attr(results, "runs"), 2L)
})

test_that("a failing run keeps the hub alive and the results empty", {
  withr::local_options(list(artma.temp.last_methods = NULL))
  checkbox <- make_checkbox_fn("bma")

  messages <- testthat::capture_messages(
    results <- run_session_hub(
      df = hub_df(),
      run_methods = function(methods) stop("boom"),
      methods_table = hub_methods_frame(),
      select_fn = make_select_fn(c("Run methods", "Exit")),
      checkbox_fn = checkbox$fn,
      width = 100
    )
  )

  expect_length(results, 0L)
  expect_equal(attr(results, "runs"), list())
  expect_true(any(grepl("The run failed", messages)))
})

test_that("an empty picker confirmation returns to the menu without running", {
  withr::local_options(list(artma.temp.last_methods = NULL))
  checkbox <- make_checkbox_fn(character(0))

  messages <- testthat::capture_messages(
    results <- run_session_hub(
      df = hub_df(),
      run_methods = abort_if_called("run_methods"),
      methods_table = hub_methods_frame(),
      select_fn = make_select_fn(c("Run methods", "Exit")),
      checkbox_fn = checkbox$fn,
      width = 100
    )
  )

  expect_length(results, 0L)
  expect_true(any(grepl("No methods selected", messages)))
})

test_that("the results submenu is a friendly no-op before the first run", {
  withr::local_options(list(artma.temp.last_methods = NULL))

  messages <- testthat::capture_messages(
    run_session_hub(
      df = hub_df(),
      run_methods = abort_if_called("run_methods"),
      methods_table = hub_methods_frame(),
      open_results = abort_if_called("open_results"),
      render_report = abort_if_called("render_report"),
      select_fn = make_select_fn(c("Results", "Open results folder", "Exit")),
      checkbox_fn = abort_if_called("checkbox_fn"),
      width = 100
    )
  )

  expect_true(any(grepl("run methods first", messages)))
})

test_that("the results submenu dispatches to the injected handlers after a run", {
  withr::local_options(list(artma.temp.last_methods = NULL))
  checkbox <- make_checkbox_fn("bma")
  run <- make_run_methods()
  opened <- FALSE
  reported <- NULL

  suppressMessages(run_session_hub(
    df = hub_df(),
    run_methods = run$fn,
    methods_table = hub_methods_frame(),
    open_results = function() opened <<- TRUE,
    render_report = function(results) reported <<- results,
    select_fn = make_select_fn(c(
      "Run methods",
      "Results", "Open results folder",
      "Results", "Render HTML report",
      "Exit"
    )),
    checkbox_fn = checkbox$fn,
    width = 100
  ))

  expect_true(opened)
  expect_equal(reported$bma, "bma-result")
})

test_that("preview prints the textual data summary", {
  withr::local_options(list(artma.temp.last_methods = NULL))

  messages <- testthat::capture_messages(
    run_session_hub(
      df = hub_df(),
      run_methods = abort_if_called("run_methods"),
      methods_table = hub_methods_frame(),
      view_data = abort_if_called("view_data"),
      select_fn = make_select_fn(c("Preview data", "Exit")),
      checkbox_fn = abort_if_called("checkbox_fn"),
      width = 100
    )
  )

  expect_true(any(grepl("3 rows, 3 columns", messages)))
  expect_true(any(grepl("2 studies", messages)))
  expect_true(any(grepl("Effect range", messages)))
  expect_true(any(grepl("SE range", messages)))
})

test_that("merge_run_results keeps the latest result and status per method", {
  accumulated <- list(bma = "old-bma", funnel_plot = "fp")
  attr(accumulated, "failed_methods") <- c(pub_bias = "boom")
  attr(accumulated, "skipped_methods") <- c(robma = "missing RoBMA")

  results <- list(bma = "new-bma", pub_bias = "pb-ok")
  attr(results, "run_info") <- list(
    methods_requested = c("bma", "pub_bias", "robma"),
    seed = 7L
  )
  attr(results, "skipped_methods") <- c(robma = "still missing")

  merged <- merge_run_results(accumulated, results)

  expect_equal(merged$bma, "new-bma")
  expect_equal(merged$funnel_plot, "fp")
  expect_equal(merged$pub_bias, "pb-ok")
  # pub_bias was re-requested and succeeded, so its old failure is dropped.
  expect_null(attr(merged, "failed_methods"))
  expect_equal(attr(merged, "skipped_methods"), c(robma = "still missing"))
  expect_equal(attr(merged, "run_info")$seed, 7L)
})

test_that("hub_menu_items hides Re-run until a run happened", {
  before <- vapply(hub_menu_items(FALSE, character(0)), function(item) item$value, character(1))
  expect_false("rerun" %in% before)

  after_items <- hub_menu_items(TRUE, c("bma", "funnel_plot"))
  after <- vapply(after_items, function(item) item$value, character(1))
  expect_true("rerun" %in% after)
  rerun <- after_items[[which(after == "rerun")]]
  expect_equal(rerun$description, "bma, funnel_plot")
})

test_that("compose_hub_choices is value-keyed with decorated labels", {
  choices <- compose_hub_choices(hub_menu_items(FALSE, character(0)), width = 100)

  expect_equal(unname(choices), c("run", "options", "preview", "results", "exit"))
  labels <- cli::ansi_strip(names(choices))
  expect_true(grepl("^Run methods +pick and run", labels[[1]]))
})

test_that("the Re-run description names the options changed since the run", {
  items <- hub_menu_items(TRUE, "bma", options_changed = c("data.na_handling", "general.seed"))
  values <- vapply(items, function(item) item$value, character(1))
  rerun <- items[[which(values == "rerun")]]
  expect_equal(rerun$description, "bma (changed: data.na_handling, general.seed)")
})

test_that("count_studies is NA without the study column", {
  expect_equal(count_studies(hub_df()), 2L)
  expect_true(is.na(count_studies(data.frame(x = 1:3))))
})

test_that("adjustable_option_defs lists the curated knobs, then method groups", {
  curated <- c(
    "data.winsorization_level",
    "data.na_handling",
    "calc.precision_type",
    "general.seed",
    "output.number_of_decimals",
    "output.report"
  )

  expect_equal(names(adjustable_option_defs(character(0))), curated)

  with_method <- names(adjustable_option_defs("linear_tests"))
  expect_equal(with_method[seq_along(curated)], curated)
  expect_true("methods.linear_tests.bootstrap_replications" %in% with_method)
  expect_true(all(startsWith(setdiff(with_method, curated), "methods.linear_tests.")))

  # A method without template options contributes nothing.
  expect_equal(names(adjustable_option_defs("no_such_method")), curated)
})

test_that("describe_option_state shows the current value and default deviation", {
  defs <- adjustable_option_defs(character(0))

  withr::local_options(list(
    artma.data.winsorization_level = 0.05,
    artma.output.number_of_decimals = 3,
    artma.output.report = NULL
  ))

  expect_equal(
    describe_option_state(defs[["data.winsorization_level"]]),
    "current: 0.05 (default: 0.01)"
  )
  expect_equal(
    describe_option_state(defs[["output.number_of_decimals"]]),
    "current: 3 (default)"
  )
  expect_equal(
    describe_option_state(defs[["output.report"]]),
    "current: (unset) (default: false)"
  )
})

test_that("option_affects_data tags exactly the data group", {
  expect_true(option_affects_data("data.winsorization_level"))
  expect_true(option_affects_data("data.na_handling"))
  expect_false(option_affects_data("output.report"))
  expect_false(option_affects_data("calc.precision_type"))
})

test_that("a declined save keeps a data edit session-only and flags staleness", {
  withr::local_options(list(artma.data.winsorization_level = 0.01))
  saves <- list()

  outcome <- suppressMessages(run_adjust_options(
    select_fn = make_select_fn(c("data.winsorization_level", "Back")),
    edit_option = function(def) 0.05,
    save_preference = function(name, value) {
      saves[[name]] <<- value
      FALSE
    },
    width = 100
  ))

  expect_equal(outcome$changed, "data.winsorization_level")
  expect_true(outcome$data_changed)
  expect_equal(getOption("artma.data.winsorization_level"), 0.05)
  expect_equal(saves, list(data.winsorization_level = 0.05))
})

test_that("an accepted save still applies the edit session-wide, without staleness", {
  withr::local_options(list(artma.output.number_of_decimals = 3))

  outcome <- suppressMessages(run_adjust_options(
    select_fn = make_select_fn(c("output.number_of_decimals", "Back")),
    edit_option = function(def) 4L,
    save_preference = function(name, value) TRUE,
    width = 100
  ))

  expect_equal(outcome$changed, "output.number_of_decimals")
  expect_false(outcome$data_changed)
  expect_equal(getOption("artma.output.number_of_decimals"), 4L)
})

test_that("an unchanged value records nothing and never asks to persist", {
  withr::local_options(list(artma.general.seed = 42))

  outcome <- suppressMessages(run_adjust_options(
    select_fn = make_select_fn(c("general.seed", "Back")),
    edit_option = function(def) 42,
    save_preference = abort_if_called("save_preference"),
    width = 100
  ))

  expect_equal(outcome$changed, character(0))
  expect_false(outcome$data_changed)
})

test_that("browse-all walks the template tree to any option", {
  withr::local_options(list(artma.visualization.theme = "blue"))

  outcome <- suppressMessages(run_adjust_options(
    select_fn = make_select_fn(c(
      "Browse all options", "visualization", "visualization.theme", "Back"
    )),
    edit_option = function(def) "red",
    save_preference = function(name, value) FALSE,
    width = 100
  ))

  expect_equal(outcome$changed, "visualization.theme")
  expect_false(outcome$data_changed)
  expect_equal(getOption("artma.visualization.theme"), "red")
})

test_that("a data edit re-prepares the data once, before the next run", {
  withr::local_options(list(
    artma.temp.last_methods = NULL,
    artma.data.winsorization_level = 0.01
  ))
  checkbox <- make_checkbox_fn("bma")
  run <- make_run_methods()
  rebuilds <- list()

  results <- suppressMessages(run_session_hub(
    df = hub_df(),
    run_methods = run$fn,
    rebuild_data = function(selection) {
      rebuilds <<- c(rebuilds, list(selection))
      hub_df()[1:2, ]
    },
    methods_table = hub_methods_frame(),
    select_fn = make_select_fn(c(
      "Adjust options", "data.winsorization_level", "Back",
      "Run methods",
      "Re-run last selection",
      "Exit"
    )),
    checkbox_fn = checkbox$fn,
    edit_option = function(def) 0.05,
    save_preference = function(name, value) FALSE,
    width = 100
  ))

  # Rebuilt exactly once, lazily, with the run's selection; the re-run reuses
  # the fresh frame.
  expect_equal(rebuilds, list("bma"))

  runs <- attr(results, "runs")
  expect_length(runs, 2L)
  expect_equal(runs[[1]]$options_changed, "data.winsorization_level")
  expect_equal(runs[[2]]$options_changed, character(0))
})

test_that("a non-data edit never triggers a data rebuild", {
  withr::local_options(list(
    artma.temp.last_methods = NULL,
    artma.output.number_of_decimals = 3
  ))
  checkbox <- make_checkbox_fn("bma")
  run <- make_run_methods()

  results <- suppressMessages(run_session_hub(
    df = hub_df(),
    run_methods = run$fn,
    rebuild_data = abort_if_called("rebuild_data"),
    methods_table = hub_methods_frame(),
    select_fn = make_select_fn(c(
      "Adjust options", "output.number_of_decimals", "Back",
      "Run methods",
      "Exit"
    )),
    checkbox_fn = checkbox$fn,
    edit_option = function(def) 4L,
    save_preference = function(name, value) FALSE,
    width = 100
  ))

  runs <- attr(results, "runs")
  expect_length(runs, 1L)
  expect_equal(runs[[1]]$options_changed, "output.number_of_decimals")
})
