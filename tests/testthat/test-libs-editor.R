test_that("extract_editor_exe parses common editor command formats", {
  box::use(artma / libs / editor[extract_editor_exe])

  expect_equal(extract_editor_exe("code -w"), "code")
  expect_equal(extract_editor_exe("  nano  "), "nano")
  expect_equal(extract_editor_exe("\"/path with spaces/vim\" -g"), "/path with spaces/vim")
  expect_null(extract_editor_exe(character()))
  expect_null(extract_editor_exe(""))
})


create_fake_executable <- function(dir, name) {
  if (.Platform$OS.type == "windows") {
    path <- file.path(dir, paste0(name, ".bat"))
    writeLines(c("@echo off", "exit /b 0"), path)
    return(invisible(path))
  }

  path <- file.path(dir, name)
  writeLines(c("#!/bin/sh", "exit 0"), path)
  Sys.chmod(path, mode = "0755")
  invisible(path)
}


test_that("editor_available detects fake executables on PATH", {
  box::use(artma / libs / editor[editor_available])

  bin_dir <- tempfile("artma-bin-")
  dir.create(bin_dir)
  create_fake_executable(bin_dir, "myeditor")

  withr::local_envvar(PATH = paste(bin_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  expect_true(editor_available("myeditor"))
  expect_true(editor_available("myeditor -w"))
  expect_false(editor_available("definitely_not_an_editor_command"))
})


test_that("detect_editor respects VISUAL then EDITOR", {
  box::use(artma / libs / editor[detect_editor])

  bin_dir <- tempfile("artma-bin-")
  dir.create(bin_dir)
  create_fake_executable(bin_dir, "visedit")
  create_fake_executable(bin_dir, "ededit")

  withr::local_envvar(PATH = paste(bin_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  withr::local_envvar(VISUAL = "visedit -w", EDITOR = "ededit")

  detected <- detect_editor()
  expect_equal(detected$source, "env")
  expect_equal(detected$cmd, "visedit -w")
})


test_that("detect_editor falls back to EDITOR when VISUAL is unset", {
  box::use(artma / libs / editor[detect_editor])

  bin_dir <- tempfile("artma-bin-")
  dir.create(bin_dir)
  create_fake_executable(bin_dir, "ededit")

  withr::local_envvar(PATH = paste(bin_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  withr::local_envvar(VISUAL = "", EDITOR = "ededit")

  detected <- detect_editor()
  expect_equal(detected$source, "env")
  expect_equal(detected$cmd, "ededit")
})


test_that("detect_editor falls back to system default when env vars unset", {
  box::use(artma / libs / editor[detect_editor])

  withr::local_envvar(VISUAL = "", EDITOR = "")

  detected <- detect_editor()

  sysname <- tolower(Sys.info()[["sysname"]])
  if (sysname == "darwin") {
    expect_equal(detected$cmd, "open -t")
    expect_equal(detected$source, "system_default")
  } else if (sysname == "windows") {
    expect_equal(detected$cmd, "notepad")
    expect_equal(detected$source, "system_default")
  } else {
    expect_equal(detected$cmd, "xdg-open")
    expect_equal(detected$source, "system_default")
  }
})


test_that("resolve_cli_editor prefers options file over env vars", {
  box::use(artma / libs / editor[resolve_cli_editor])

  bin_dir <- tempfile("artma-bin-")
  dir.create(bin_dir)
  create_fake_executable(bin_dir, "myeditor")

  withr::local_envvar(PATH = paste(bin_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  withr::local_envvar(VISUAL = "", EDITOR = "")

  opts_file <- tempfile("artma-options-", fileext = ".yaml")
  writeLines(c("cli:", "  editor: myeditor -w"), opts_file)

  resolved <- resolve_cli_editor(options_file_path = opts_file)
  expect_equal(resolved$source, "options_file")
  expect_equal(resolved$cmd, "myeditor -w")
})


test_that("resolve_cli_editor falls back to env vars when options file has no editor", {
  box::use(artma / libs / editor[resolve_cli_editor])

  bin_dir <- tempfile("artma-bin-")
  dir.create(bin_dir)
  create_fake_executable(bin_dir, "visedit")

  withr::local_envvar(PATH = paste(bin_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  withr::local_envvar(VISUAL = "visedit", EDITOR = "")

  opts_file <- tempfile("artma-options-", fileext = ".yaml")
  writeLines(c("cli:", "  editor: .na"), opts_file)

  resolved <- resolve_cli_editor(options_file_path = opts_file)
  expect_equal(resolved$source, "env")
  expect_equal(resolved$cmd, "visedit")
})


test_that("resolve_cli_editor falls back to system default when nothing else available", {
  box::use(artma / libs / editor[resolve_cli_editor])

  empty_bin_dir <- tempfile("artma-empty-bin-")
  dir.create(empty_bin_dir)

  withr::local_envvar(PATH = empty_bin_dir, VISUAL = "", EDITOR = "")

  resolved <- resolve_cli_editor()
  expect_equal(resolved$source, "system_default")

  sysname <- tolower(Sys.info()[["sysname"]])
  if (sysname == "darwin") {
    expect_equal(resolved$cmd, "open -t")
  } else if (sysname == "windows") {
    expect_equal(resolved$cmd, "notepad")
  } else {
    expect_equal(resolved$cmd, "xdg-open")
  }
})
