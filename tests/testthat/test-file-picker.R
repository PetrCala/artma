box::use(
  testthat[expect_equal, expect_error, expect_false, expect_identical, expect_match, expect_true, test_that]
)

box::use(
  artma / libs / core / file_picker[
    build_choose_script,
    choose_path_interactively,
    parse_osascript_output,
    path_picker_available,
    path_picker_backend
  ]
)

test_that("no picker is offered in a non-interactive session", {
  expect_identical(path_picker_backend(is_interactive = FALSE), "none")
  expect_false(path_picker_available(is_interactive = FALSE))
})

test_that("macOS never routes through Tcl/Tk", {
  # Loading the tcltk namespace initializes Aqua Tk in-process, which aborts the
  # whole R session on macOS when Tk and the front-end disagree about AppKit.
  backend <- path_picker_backend(os_type = "unix", sysname = "Darwin", is_interactive = TRUE)
  expect_true(backend %in% c("macos", "none"))
})

test_that("Windows uses its native chooser", {
  expect_identical(
    path_picker_backend(os_type = "windows", sysname = "Windows", is_interactive = TRUE),
    "windows"
  )
})

test_that("choose_path_interactively refuses to pretend when no picker exists", {
  # Tests always run non-interactively, so no backend is available here.
  expect_error(choose_path_interactively("file"))
})

test_that("the AppleScript picks the chooser matching the requested type", {
  expect_match(build_choose_script("file", "Select file", tempdir()), "choose file", fixed = TRUE)
  expect_match(build_choose_script("directory", "Select directory", tempdir()), "choose folder", fixed = TRUE)
})

test_that("the AppleScript omits a default location that does not exist", {
  script <- build_choose_script("file", "Select file", file.path(tempdir(), "no-such-dir"))
  expect_false(grepl("default location", script, fixed = TRUE))
})

test_that("a quote in the caption cannot break out of the AppleScript literal", {
  script <- build_choose_script("file", "Say \"hi\"", "")
  expect_match(script, "with prompt \"Say \\\"hi\\\"\"", fixed = TRUE)
})

test_that("osascript output is read as a single path", {
  expect_identical(parse_osascript_output("/tmp/data.csv\n"), "/tmp/data.csv")
  expect_identical(parse_osascript_output(c("noise", "/tmp/data.csv")), "/tmp/data.csv")
})

test_that("a cancelled or unusable picker reads as an empty answer", {
  expect_identical(parse_osascript_output(""), "")
  expect_identical(parse_osascript_output(character(0)), "")
  expect_identical(parse_osascript_output(structure("/tmp/data.csv", status = 1L)), "")
})

test_that("the AppleScript compiles", {
  testthat::skip_if_not(identical(Sys.info()[["sysname"]], "Darwin"), "macOS only")
  testthat::skip_if_not(nzchar(Sys.which("osacompile")), "osacompile not available")

  script_file <- withr::local_tempfile(fileext = ".applescript")
  writeLines(build_choose_script("file", "Select file", tempdir()), script_file)
  compiled <- withr::local_tempfile(fileext = ".scpt")
  status <- system2("osacompile", c("-o", shQuote(compiled), shQuote(script_file)), stdout = FALSE, stderr = FALSE)
  expect_equal(status, 0L)
})
