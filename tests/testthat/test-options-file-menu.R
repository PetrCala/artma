box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(
  artma / interactive / options_file_menu[
    options_file_items,
    options_file_menu_items,
    run_options_file_menu,
    run_unbound_entry
  ]
)

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

# An options_list(details = TRUE) frame.
details_frame <- function(files = c("a.yaml", "b.yaml")) {
  data.frame(
    file = files,
    data_source_path = paste0("/data/", tools::file_path_sans_ext(files), ".csv"),
    modified = as.POSIXct(rep("2026-08-01 10:00:00", length(files))),
    last_run = as.POSIXct(rep(NA_character_, length(files))),
    n_non_default = rep(0L, length(files)),
    stringsAsFactors = FALSE
  )
}

abort_if_called <- function(what) {
  function(...) stop(what, " must not be called")
}

# Only the listing is real; every management action fails loudly unless the
# test overrode it.
file_actions <- function(overrides = list(), files = c("a.yaml", "b.yaml")) {
  defaults <- list(
    list = function() details_frame(files),
    create = abort_if_called("create"),
    duplicate = abort_if_called("duplicate"),
    edit = abort_if_called("edit"),
    repair = abort_if_called("repair"),
    compare = abort_if_called("compare"),
    open = abort_if_called("open"),
    delete = abort_if_called("delete")
  )
  utils::modifyList(defaults, overrides)
}

item_values <- function(items) {
  vapply(items, function(item) item$value, character(1))
}

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

  expect_equal(item_values(items), c("a.yaml", "b.yaml"))
  expect_true(grepl("current", items[[1]]$description, fixed = TRUE))
  expect_true(grepl("a.csv", items[[1]]$description, fixed = TRUE))
  expect_true(grepl("last run 2026-08-20", items[[1]]$description, fixed = TRUE))
  expect_true(grepl("3 non-default", items[[1]]$description, fixed = TRUE))
  expect_false(grepl("current", items[[2]]$description, fixed = TRUE))
  expect_true(grepl("no data source", items[[2]]$description, fixed = TRUE))
  expect_true(grepl("never run", items[[2]]$description, fixed = TRUE))
  expect_true(grepl("all defaults", items[[2]]$description, fixed = TRUE))
})

test_that("the menu reads differently with and without a current file", {
  unbound <- options_file_menu_items(current_file = NULL, n_files = 2L)
  expect_equal(unbound[[1]]$value, "select")
  expect_equal(unbound[[1]]$name, "Select a file")

  bound <- options_file_menu_items(current_file = "a.yaml", n_files = 2L)
  expect_equal(bound[[1]]$name, "Switch file")
  expect_true(grepl("a.yaml", bound[[1]]$description, fixed = TRUE))
})

test_that("with no options files yet, only creating one is offered", {
  items <- options_file_menu_items(current_file = NULL, n_files = 0L)
  expect_equal(item_values(items), c("create", "__back__"))
})

test_that("selecting a file binds it and leaves the menu", {
  bound <- character(0)

  outcome <- suppressMessages(run_options_file_menu(
    bind_options = function(file_name) bound <<- c(bound, file_name),
    current_file = NULL,
    select_fn = make_select_fn(c("Select a file", "b.yaml")),
    file_actions = file_actions(),
    width = 100
  ))

  expect_equal(bound, "b.yaml")
  expect_equal(outcome$file, "b.yaml")
  expect_true(outcome$changed)
})

test_that("backing out leaves the session where it was", {
  outcome <- suppressMessages(run_options_file_menu(
    bind_options = abort_if_called("bind_options"),
    current_file = "a.yaml",
    select_fn = make_select_fn(c("Switch file", "Back", "Back")),
    file_actions = file_actions(),
    width = 100
  ))

  expect_equal(outcome$file, "a.yaml")
  expect_false(outcome$changed)
})

test_that("a failed bind keeps the current file and the menu open", {
  messages <- testthat::capture_messages(
    outcome <- run_options_file_menu(
      bind_options = function(file_name) stop("unreadable"),
      current_file = "a.yaml",
      select_fn = make_select_fn(c("Switch file", "b.yaml", "Back")),
      file_actions = file_actions(),
      width = 100
    )
  )

  expect_true(any(grepl("Could not load", messages)))
  expect_equal(outcome$file, "a.yaml")
  expect_false(outcome$changed)
})

test_that("selecting the file already loaded is a no-op", {
  messages <- testthat::capture_messages(
    outcome <- run_options_file_menu(
      bind_options = abort_if_called("bind_options"),
      current_file = "a.yaml",
      select_fn = make_select_fn(c("Switch file", "a.yaml", "Back")),
      file_actions = file_actions(),
      width = 100
    )
  )

  expect_true(any(grepl("Already running on", messages)))
  expect_false(outcome$changed)
})

test_that("a new file is loaded straight away when the session has none", {
  bound <- character(0)

  outcome <- suppressMessages(run_options_file_menu(
    bind_options = function(file_name) bound <<- c(bound, file_name),
    current_file = NULL,
    select_fn = make_select_fn("Create a new file"),
    file_actions = file_actions(list(create = function() "new.yaml")),
    width = 100
  ))

  expect_equal(bound, "new.yaml")
  expect_equal(outcome$file, "new.yaml")
})

test_that("a bound session is asked before a new file takes over", {
  bound <- character(0)

  outcome <- suppressMessages(run_options_file_menu(
    bind_options = function(file_name) bound <<- c(bound, file_name),
    current_file = "a.yaml",
    select_fn = make_select_fn(c("Create a new file", "No", "Back")),
    file_actions = file_actions(list(create = function() "new.yaml")),
    width = 100
  ))

  expect_equal(bound, character(0))
  expect_equal(outcome$file, "a.yaml")
  expect_false(outcome$changed)
})

test_that("editing the loaded file reloads it", {
  edited <- character(0)
  bound <- character(0)

  outcome <- suppressMessages(run_options_file_menu(
    bind_options = function(file_name) bound <<- c(bound, file_name),
    current_file = "a.yaml",
    select_fn = make_select_fn(c("Edit a file", "Back")),
    file_actions = file_actions(list(
      edit = function(file) edited <<- c(edited, file)
    )),
    width = 100
  ))

  # The session's own file is the one edited, and the session picks up what
  # the edit wrote rather than keeping the values it loaded earlier.
  expect_equal(edited, "a.yaml")
  expect_equal(bound, "a.yaml")
  expect_true(outcome$changed)
})

test_that("an unbound session lets the editor prompt for the file", {
  targets <- list()

  suppressMessages(run_options_file_menu(
    bind_options = abort_if_called("bind_options"),
    current_file = NULL,
    select_fn = make_select_fn(c("Edit a file", "Back")),
    file_actions = file_actions(list(
      edit = function(file) targets <<- c(targets, list(file))
    )),
    width = 100
  ))

  expect_equal(targets, list(NULL))
})

test_that("a failing management action reports and keeps the menu open", {
  messages <- testthat::capture_messages(
    outcome <- run_options_file_menu(
      bind_options = abort_if_called("bind_options"),
      current_file = "a.yaml",
      select_fn = make_select_fn(c("Compare two files", "Back")),
      file_actions = file_actions(list(compare = function() stop("nope"))),
      width = 100
    )
  )

  expect_true(any(grepl("Comparing options files failed", messages)))
  expect_false(outcome$changed)
})

test_that("deleting the loaded file leaves the session unbound", {
  remaining <- c("a.yaml", "b.yaml")

  messages <- testthat::capture_messages(
    outcome <- run_options_file_menu(
      bind_options = abort_if_called("bind_options"),
      current_file = "a.yaml",
      select_fn = make_select_fn("Delete files"),
      file_actions = list(
        list = function() details_frame(remaining),
        delete = function() remaining <<- "b.yaml"
      ),
      width = 100
    )
  )

  expect_null(outcome$file)
  expect_true(outcome$changed)
  expect_true(any(grepl("select or create an options file", messages)))
})

test_that("binding a file records it as the last used one", {
  box::use(artma / options / last_used[read_last_used_file, write_last_used_file])
  dir <- withr::local_tempdir()

  suppressMessages(run_options_file_menu(
    bind_options = function(file_name) invisible(TRUE),
    current_file = NULL,
    select_fn = make_select_fn(c("Select a file", "b.yaml")),
    file_actions = file_actions(list(
      remember_last_used = function(file_name) {
        write_last_used_file(file_name, options_dir = dir)
      }
    )),
    width = 100
  ))

  expect_equal(read_last_used_file(options_dir = dir), "b.yaml")
})

test_that("a failed bind leaves the last-used marker untouched", {
  suppressMessages(run_options_file_menu(
    bind_options = function(file_name) stop("unreadable"),
    current_file = NULL,
    select_fn = make_select_fn(c("Select a file", "b.yaml", "Back", "Back")),
    file_actions = file_actions(list(
      remember_last_used = abort_if_called("remember_last_used")
    )),
    width = 100
  ))

  testthat::succeed()
})

test_that("deleting the file the marker names clears the marker", {
  box::use(artma / options / last_used[
    prune_last_used_file, read_last_used_file, write_last_used_file
  ])
  dir <- withr::local_tempdir()
  write_last_used_file("a.yaml", options_dir = dir)
  remaining <- c("a.yaml", "b.yaml")

  suppressMessages(run_options_file_menu(
    bind_options = abort_if_called("bind_options"),
    current_file = "a.yaml",
    select_fn = make_select_fn("Delete files"),
    file_actions = list(
      list = function() details_frame(remaining),
      delete = function() remaining <<- "b.yaml",
      prune_last_used = function(existing_files) {
        prune_last_used_file(existing_files, options_dir = dir)
      }
    ),
    width = 100
  ))

  expect_null(read_last_used_file(options_dir = dir))
})

test_that("deleting a non-current file the marker names clears the marker too", {
  box::use(artma / options / last_used[
    prune_last_used_file, read_last_used_file, write_last_used_file
  ])
  dir <- withr::local_tempdir()
  write_last_used_file("b.yaml", options_dir = dir)
  remaining <- c("a.yaml", "b.yaml")

  suppressMessages(run_options_file_menu(
    bind_options = abort_if_called("bind_options"),
    current_file = "a.yaml",
    select_fn = make_select_fn(c("Delete files", "Back")),
    file_actions = list(
      list = function() details_frame(remaining),
      delete = function() remaining <<- "a.yaml",
      prune_last_used = function(existing_files) {
        prune_last_used_file(existing_files, options_dir = dir)
      }
    ),
    width = 100
  ))

  expect_null(read_last_used_file(options_dir = dir))
})

test_that("deleting unrelated files keeps the marker", {
  box::use(artma / options / last_used[
    prune_last_used_file, read_last_used_file, write_last_used_file
  ])
  dir <- withr::local_tempdir()
  write_last_used_file("a.yaml", options_dir = dir)
  remaining <- c("a.yaml", "b.yaml")

  suppressMessages(run_options_file_menu(
    bind_options = abort_if_called("bind_options"),
    current_file = "a.yaml",
    select_fn = make_select_fn(c("Delete files", "Back")),
    file_actions = list(
      list = function() details_frame(remaining),
      delete = function() remaining <<- "a.yaml",
      prune_last_used = function(existing_files) {
        prune_last_used_file(existing_files, options_dir = dir)
      }
    ),
    width = 100
  ))

  expect_equal(read_last_used_file(options_dir = dir), "a.yaml")
})

test_that("the unbound entry runs the create flow when no files exist", {
  bound <- character(0)

  messages <- testthat::capture_messages(
    outcome <- run_unbound_entry(
      bind_options = function(file_name) bound <<- c(bound, file_name),
      select_fn = abort_if_called("select_fn"),
      file_actions = file_actions(
        overrides = list(create = function() "new.yaml"),
        files = character(0)
      ),
      width = 100
    )
  )

  expect_true(any(grepl("No options files exist yet", messages)))
  expect_equal(bound, "new.yaml")
  expect_equal(outcome$file, "new.yaml")
  expect_true(outcome$changed)
})

test_that("a cancelled first-time create leaves the entry unbound", {
  outcome <- suppressMessages(run_unbound_entry(
    bind_options = abort_if_called("bind_options"),
    select_fn = abort_if_called("select_fn"),
    file_actions = file_actions(
      overrides = list(create = function() NULL),
      files = character(0)
    ),
    width = 100
  ))

  expect_null(outcome$file)
  expect_false(outcome$changed)
})

test_that("a failing first-time create reports and leaves the entry unbound", {
  messages <- testthat::capture_messages(
    outcome <- run_unbound_entry(
      bind_options = abort_if_called("bind_options"),
      select_fn = abort_if_called("select_fn"),
      file_actions = file_actions(
        overrides = list(create = function() stop("nope")),
        files = character(0)
      ),
      width = 100
    )
  )

  expect_true(any(grepl("Creating an options file failed", messages)))
  expect_false(outcome$changed)
})

test_that("the unbound entry opens the picker when files exist", {
  bound <- character(0)

  outcome <- suppressMessages(run_unbound_entry(
    bind_options = function(file_name) bound <<- c(bound, file_name),
    select_fn = make_select_fn("b.yaml"),
    file_actions = file_actions(),
    width = 100
  ))

  expect_equal(bound, "b.yaml")
  expect_equal(outcome$file, "b.yaml")
  expect_true(outcome$changed)
})

test_that("backing out of the entry picker leaves the session unbound", {
  outcome <- suppressMessages(run_unbound_entry(
    bind_options = abort_if_called("bind_options"),
    select_fn = make_select_fn("Back"),
    file_actions = file_actions(),
    width = 100
  ))

  expect_null(outcome$file)
  expect_false(outcome$changed)
})

test_that("the entry picker's create entry binds the new file", {
  bound <- character(0)

  outcome <- suppressMessages(run_unbound_entry(
    bind_options = function(file_name) bound <<- c(bound, file_name),
    select_fn = make_select_fn("Create a new file"),
    file_actions = file_actions(overrides = list(create = function() "new.yaml")),
    width = 100
  ))

  expect_equal(bound, "new.yaml")
  expect_equal(outcome$file, "new.yaml")
  expect_true(outcome$changed)
})

test_that("a failed bind at the entry leaves the session unbound", {
  messages <- testthat::capture_messages(
    outcome <- run_unbound_entry(
      bind_options = function(file_name) stop("unreadable"),
      select_fn = make_select_fn("b.yaml"),
      file_actions = file_actions(),
      width = 100
    )
  )

  expect_true(any(grepl("Could not load", messages)))
  expect_null(outcome$file)
  expect_false(outcome$changed)
})

test_that("deleting other files keeps the session on its own", {
  remaining <- c("a.yaml", "b.yaml")

  outcome <- suppressMessages(run_options_file_menu(
    bind_options = abort_if_called("bind_options"),
    current_file = "a.yaml",
    select_fn = make_select_fn(c("Delete files", "Back")),
    file_actions = list(
      list = function() details_frame(remaining),
      delete = function() remaining <<- "a.yaml"
    ),
    width = 100
  ))

  expect_equal(outcome$file, "a.yaml")
  expect_false(outcome$changed)
})
