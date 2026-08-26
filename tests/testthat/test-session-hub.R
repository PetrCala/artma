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
    help_menu_items,
    hub_menu_items,
    merge_run_results,
    option_affects_data,
    options_file_items,
    run_adjust_options,
    run_session_hub,
    settings_menu_items
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
# and the Help submenu's methods overview read.
hub_methods_frame <- function() {
  data.frame(
    method = c("bma", "funnel_plot"),
    description = c("Bayesian model averaging", "Funnel plot"),
    required_columns = c("effect, se", "effect, se"),
    depends_on = c("", ""),
    suggests = c("BMS", ""),
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

test_that("hub_menu_items hides the switch item without a handler", {
  with_switch <- vapply(hub_menu_items(FALSE, character(0)), function(item) item$value, character(1))
  expect_true(all(c("settings", "switch", "help") %in% with_switch))

  without_switch <- vapply(
    hub_menu_items(FALSE, character(0), can_switch = FALSE),
    function(item) item$value, character(1)
  )
  expect_false("switch" %in% without_switch)
  expect_true(all(c("settings", "help") %in% without_switch))
})

test_that("compose_hub_choices is value-keyed with decorated labels", {
  choices <- compose_hub_choices(hub_menu_items(FALSE, character(0)), width = 100)

  expect_equal(
    unname(choices),
    c("run", "options", "preview", "results", "settings", "switch", "help", "exit")
  )
  labels <- cli::ansi_strip(names(choices))
  expect_true(grepl("^Run methods +pick and run", labels[[1]]))
})

test_that("the Re-run description names the options changed since the run", {
  items <- hub_menu_items(TRUE, "bma", options_changed = c("data.na_handling", "general.seed"))
  values <- vapply(items, function(item) item$value, character(1))
  rerun <- items[[which(values == "rerun")]]
  expect_equal(rerun$description, "bma (changed: data.na_handling, general.seed)")
})

test_that("the settings and help submenus are value-keyed", {
  settings <- vapply(settings_menu_items(), function(item) item$value, character(1))
  expect_equal(settings, c("theme", "verbosity", "autonomy", "cache", "back"))

  help <- vapply(help_menu_items(), function(item) item$value, character(1))
  expect_equal(help, c(
    "methods", "options", "vignette_getting_started",
    "vignette_options_files", "website", "back"
  ))
})

test_that("options_file_items decorates each file and marks the current one", {
  details <- data.frame(
    file = c("a.yaml", "b.yaml"),
    data_source_path = c("/data/a.csv", NA_character_),
    modified = as.POSIXct(c("2026-08-01 10:00:00", "2026-08-02 10:00:00")),
    last_run = as.POSIXct(c("2026-08-20 09:30:00", NA)),
    n_non_default = c(3L, 0L),
    stringsAsFactors = FALSE
  )

  items <- options_file_items(details, current_file = "a.yaml")

  expect_equal(vapply(items, function(item) item$value, character(1)), c("a.yaml", "b.yaml"))
  expect_true(grepl("current", items[[1]]$description, fixed = TRUE))
  expect_true(grepl("a.csv", items[[1]]$description, fixed = TRUE))
  expect_true(grepl("last run 2026-08-20", items[[1]]$description, fixed = TRUE))
  expect_true(grepl("3 non-default", items[[1]]$description, fixed = TRUE))
  expect_false(grepl("current", items[[2]]$description, fixed = TRUE))
  expect_true(grepl("no data source", items[[2]]$description, fixed = TRUE))
  expect_true(grepl("never run", items[[2]]$description, fixed = TRUE))
  expect_true(grepl("all defaults", items[[2]]$description, fixed = TRUE))
})

test_that("settings toggles take effect for the session", {
  withr::local_options(list(
    artma.temp.last_methods = NULL,
    artma.verbose = 3,
    artma.cache.use_cache = TRUE
  ))
  themes_set <- character(0)
  autonomy_levels_set <- character(0)

  suppressMessages(run_session_hub(
    df = hub_df(),
    run_methods = abort_if_called("run_methods"),
    methods_table = hub_methods_frame(),
    set_theme = function(theme) themes_set <<- c(themes_set, theme),
    set_autonomy = function(level) autonomy_levels_set <<- c(autonomy_levels_set, level),
    select_fn = make_select_fn(c(
      "Settings", "Visualization theme", "red",
      "Settings", "Verbosity", "4 - debug",
      "Settings", "Autonomy level", "balanced",
      "Settings", "Result caching", "No",
      "Exit"
    )),
    checkbox_fn = abort_if_called("checkbox_fn"),
    width = 100
  ))

  expect_equal(themes_set, "red")
  expect_equal(autonomy_levels_set, "balanced")
  expect_equal(getOption("artma.verbose"), 4L)
  expect_false(getOption("artma.cache.use_cache"))
})

test_that("a cancelled settings submenu returns to the hub without changes", {
  withr::local_options(list(artma.temp.last_methods = NULL, artma.verbose = 3))

  suppressMessages(run_session_hub(
    df = hub_df(),
    run_methods = abort_if_called("run_methods"),
    methods_table = hub_methods_frame(),
    set_theme = abort_if_called("set_theme"),
    set_autonomy = abort_if_called("set_autonomy"),
    select_fn = make_select_fn(c("Settings", NA_character_, "Exit")),
    checkbox_fn = abort_if_called("checkbox_fn"),
    width = 100
  ))

  expect_equal(getOption("artma.verbose"), 3)
})

test_that("the help submenu prints overviews and opens the docs links", {
  withr::local_options(list(artma.temp.last_methods = NULL))
  urls <- character(0)
  options_help_calls <- 0L

  messages <- testthat::capture_messages(
    run_session_hub(
      df = hub_df(),
      run_methods = abort_if_called("run_methods"),
      methods_table = hub_methods_frame(),
      show_options_help = function() options_help_calls <<- options_help_calls + 1L,
      open_url = function(url, description) urls <<- c(urls, url),
      select_fn = make_select_fn(c(
        "Help", "Methods overview",
        "Help", "Options overview",
        "Help", "Getting Started vignette",
        "Help", "Package website",
        "Exit"
      )),
      checkbox_fn = abort_if_called("checkbox_fn"),
      width = 100
    )
  )

  # The methods overview prints the injected frame's rows.
  expect_true(any(grepl("Bayesian model averaging", messages)))
  expect_equal(options_help_calls, 1L)
  expect_length(urls, 2L)
  expect_true(grepl("getting-started", urls[[1]], fixed = TRUE))
  expect_false(grepl("getting-started", urls[[2]], fixed = TRUE))
})

test_that("switching the options file reloads options and re-prepares data", {
  withr::local_options(list(
    artma.temp.last_methods = NULL,
    artma.temp.file_name = "a.yaml",
    artma.temp.dir_name = NULL,
    artma.general.name = NULL,
    artma.data.source_path = NULL,
    artma.data.columns = NULL,
    artma.methods.bma.iter = NULL,
    artma.methods.bma.burn = NULL,
    artma.methods.linear_tests.conf_level = NULL
  ))

  tmp_dir <- withr::local_tempdir()
  template_path <- file.path(tmp_dir, "template.yaml")
  yaml::write_yaml(
    list(
      general = list(name = list(type = "character", default = "Default Config", help = "Name")),
      data = list(
        source_path = list(type = "character", help = "Path to the data file"),
        columns = list(type = "list", default = list(), help = "The unified column store")
      ),
      methods = list(
        bma = list(
          iter = list(type = "integer", default = 100L, help = "Iterations"),
          burn = list(type = "integer", default = 10L, help = "Burn-in")
        ),
        linear_tests = list(
          conf_level = list(type = "numeric", default = 0.95, help = "Confidence level")
        )
      )
    ),
    template_path
  )
  yaml::write_yaml(
    list(general = list(name = "A"), data = list(source_path = "/data/a.csv", columns = list())),
    file.path(tmp_dir, "a.yaml")
  )
  yaml::write_yaml(
    list(
      general = list(name = "B"),
      data = list(source_path = "/data/b.csv", columns = list()),
      methods = list(bma = list(iter = 500L))
    ),
    file.path(tmp_dir, "b.yaml")
  )

  # The re-prepared frame differs from the initial one, so the header proves
  # which frame the hub holds.
  fresh_df <- data.frame(
    study_id = c("s9", "s9"),
    effect = c(1.5, 1.6),
    se = c(0.2, 0.3),
    stringsAsFactors = FALSE
  )

  seen_iters <- integer(0)
  run_methods <- function(methods) {
    seen_iters <<- c(seen_iters, getOption("artma.methods.bma.iter"))
    results <- stats::setNames(
      lapply(methods, function(m) paste0(m, "-result")),
      methods
    )
    attr(results, "run_info") <- list(
      methods_requested = methods,
      seed = 1L,
      output_files = list()
    )
    results
  }
  checkbox <- make_checkbox_fn("bma")

  messages <- testthat::capture_messages(
    results <- run_session_hub(
      df = hub_df(),
      run_methods = run_methods,
      methods_table = hub_methods_frame(),
      list_options = function() {
        artma::options_list(options_dir = tmp_dir, details = TRUE, template_path = template_path)
      },
      switch_options = function(file_name) {
        loaded <- artma::options_load(
          options_file_name = file_name,
          options_dir = tmp_dir,
          template_path = template_path,
          should_validate = TRUE,
          should_add_temp_options = TRUE
        )
        # Base options(), matching the artma() wiring: the values must
        # survive this call for the rest of the hub session.
        options(loaded)
        fresh_df
      },
      select_fn = make_select_fn(c(
        "Switch options file", "b.yaml",
        "Run methods",
        "Exit"
      )),
      checkbox_fn = checkbox$fn,
      width = 100
    )
  )

  # The run after the switch saw the new file's options.
  expect_equal(seen_iters, 500L)
  expect_equal(results$bma, "bma-result")
  # The session now runs on the new file and the freshly prepared frame.
  expect_equal(getOption("artma.temp.file_name"), "b.yaml")
  expect_equal(getOption("artma.general.name"), "B")
  expect_true(any(grepl("Switched to", messages)))
  expect_true(any(grepl("2 rows", messages)))
})

test_that("a failed options switch keeps the session on the current file", {
  withr::local_options(list(
    artma.temp.last_methods = NULL,
    artma.temp.file_name = "a.yaml"
  ))

  details <- data.frame(
    file = c("a.yaml", "b.yaml"),
    data_source_path = c("/data/a.csv", "/data/b.csv"),
    modified = as.POSIXct(c("2026-08-01 10:00:00", "2026-08-02 10:00:00")),
    last_run = as.POSIXct(c(NA, NA)),
    n_non_default = c(0L, 0L),
    stringsAsFactors = FALSE
  )

  messages <- testthat::capture_messages(
    run_session_hub(
      df = hub_df(),
      run_methods = abort_if_called("run_methods"),
      methods_table = hub_methods_frame(),
      list_options = function() details,
      switch_options = function(file_name) stop("no such file"),
      select_fn = make_select_fn(c(
        "Switch options file", "b.yaml",
        "Preview data",
        "Exit"
      )),
      checkbox_fn = abort_if_called("checkbox_fn"),
      width = 100
    )
  )

  expect_true(any(grepl("Could not switch", messages)))
  expect_equal(getOption("artma.temp.file_name"), "a.yaml")
  # The original frame is still the session's data.
  expect_true(any(grepl("3 rows, 3 columns", messages)))
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
