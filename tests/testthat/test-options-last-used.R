box::use(
  testthat[
    expect_equal,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(
  artma / options / last_used[
    clear_last_used_file,
    last_used_marker_path,
    prune_last_used_file,
    read_last_used_file,
    write_last_used_file
  ]
)

test_that("the marker round-trips the options file name", {
  dir <- withr::local_tempdir()

  write_last_used_file("analysis.yaml", options_dir = dir)

  expect_true(file.exists(last_used_marker_path(options_dir = dir)))
  expect_equal(read_last_used_file(options_dir = dir), "analysis.yaml")
})

test_that("a missing, empty, or blank marker reads as NULL", {
  dir <- withr::local_tempdir()

  expect_null(read_last_used_file(options_dir = dir))

  writeLines(character(0), last_used_marker_path(options_dir = dir))
  expect_null(read_last_used_file(options_dir = dir))

  writeLines("   ", last_used_marker_path(options_dir = dir))
  expect_null(read_last_used_file(options_dir = dir))
})

test_that("writing creates the options directory when it is missing", {
  dir <- file.path(withr::local_tempdir(), "nested")

  write_last_used_file("a.yaml", options_dir = dir)

  expect_equal(read_last_used_file(options_dir = dir), "a.yaml")
})

test_that("clearing removes the marker and tolerates its absence", {
  dir <- withr::local_tempdir()
  write_last_used_file("a.yaml", options_dir = dir)

  clear_last_used_file(options_dir = dir)
  expect_null(read_last_used_file(options_dir = dir))

  # Clearing again must be a silent no-op.
  clear_last_used_file(options_dir = dir)
  expect_null(read_last_used_file(options_dir = dir))
})

test_that("pruning clears the marker only when its file is gone", {
  dir <- withr::local_tempdir()
  write_last_used_file("a.yaml", options_dir = dir)

  prune_last_used_file(c("a.yaml", "b.yaml"), options_dir = dir)
  expect_equal(read_last_used_file(options_dir = dir), "a.yaml")

  prune_last_used_file("b.yaml", options_dir = dir)
  expect_null(read_last_used_file(options_dir = dir))
})

test_that("pruning against no remaining files clears the marker", {
  dir <- withr::local_tempdir()
  write_last_used_file("a.yaml", options_dir = dir)

  prune_last_used_file(character(0), options_dir = dir)

  expect_null(read_last_used_file(options_dir = dir))
})

test_that("session entry resumes on a marker naming an existing file", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "a.yaml"))
  write_last_used_file("a.yaml", options_dir = dir)
  bound <- character(0)

  messages <- testthat::capture_messages(
    restored <- artma:::restore_last_options_file(
      bind_options = function(file_name) bound <<- c(bound, file_name),
      options_dir = dir
    )
  )

  expect_equal(restored, "a.yaml")
  expect_equal(bound, "a.yaml")
  expect_true(any(grepl("Resuming on", messages)))
})

test_that("session entry falls back and clears the marker when the file is gone", {
  dir <- withr::local_tempdir()
  write_last_used_file("gone.yaml", options_dir = dir)

  messages <- testthat::capture_messages(
    restored <- artma:::restore_last_options_file(
      bind_options = function(file_name) stop("bind_options must not be called"),
      options_dir = dir
    )
  )

  expect_null(restored)
  expect_null(read_last_used_file(options_dir = dir))
  expect_true(any(grepl("no longer exists", messages)))
})

test_that("session entry falls back but keeps the marker when loading fails", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "a.yaml"))
  write_last_used_file("a.yaml", options_dir = dir)

  messages <- testthat::capture_messages(
    restored <- artma:::restore_last_options_file(
      bind_options = function(file_name) stop("corrupted"),
      options_dir = dir
    )
  )

  expect_null(restored)
  expect_equal(read_last_used_file(options_dir = dir), "a.yaml")
  expect_true(any(grepl("Could not resume", messages)))
})

test_that("session entry without a marker stays unbound and silent", {
  dir <- withr::local_tempdir()

  messages <- testthat::capture_messages(
    restored <- artma:::restore_last_options_file(
      bind_options = function(file_name) stop("bind_options must not be called"),
      options_dir = dir
    )
  )

  expect_null(restored)
  expect_equal(messages, character(0))
})
