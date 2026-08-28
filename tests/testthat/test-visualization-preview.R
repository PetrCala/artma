box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_null,
    expect_true,
    test_that
  ],
  withr[local_options, local_tempdir, local_tempfile],
  artma / visualization / export[
    export_named_plots,
    preview_needs_file,
    preview_plot,
    save_plot,
    use_file_preview
  ],
  artma / visualization / fork_safety[with_forked_worker_flag]
)

# The regression this file guards: in terminal R on macOS, plot previews went
# through `print()`, which draws to quartz. Quartz windows only pump their
# events while R idles at the prompt, so they beachball for the whole run, and
# successive previews draw into the same window so only the last plot
# survives. Previews there now open the exported PNG file instead.

test_that("use_file_preview is limited to interactive macOS terminal sessions", {
  # The macOS terminal case, with and without X11 support built in.
  expect_true(use_file_preview(interactive_session = TRUE, sysname = "Darwin", gui = "X11"))
  expect_true(use_file_preview(interactive_session = TRUE, sysname = "Darwin", gui = "unused"))

  # GUI front ends pump their own device events; they keep the print preview.
  expect_false(use_file_preview(interactive_session = TRUE, sysname = "Darwin", gui = "RStudio"))
  expect_false(use_file_preview(interactive_session = TRUE, sysname = "Darwin", gui = "AQUA"))

  # Non-interactive sessions and other platforms keep the print preview.
  expect_false(use_file_preview(interactive_session = FALSE, sysname = "Darwin", gui = "X11"))
  expect_false(use_file_preview(interactive_session = TRUE, sysname = "Linux", gui = "X11"))
})

test_that("preview_needs_file is FALSE inside a forked worker", {
  expect_false(with_forked_worker_flag(preview_needs_file()))
  expect_true(is.logical(preview_needs_file()))
})

test_that("preview_plot opens the exported file instead of printing in file mode", {
  # `preview_plot()` calls `print()` from inside a box module, whose scope
  # chain bypasses the global environment, so the probe method must go through
  # the S3 registration table to be dispatched.
  calls <- new.env(parent = emptyenv())
  calls$printed <- 0L
  calls$opened <- character(0)
  registerS3method(
    "print", "artma_file_preview_probe",
    function(x, ...) {
      calls$printed <- calls$printed + 1L
      invisible(x)
    },
    envir = globalenv()
  )
  probe <- structure(list(), class = "artma_file_preview_probe")
  opener <- function(path) calls$opened <- c(calls$opened, path)

  path <- local_tempfile(fileext = ".png")
  file.create(path)

  expect_null(preview_plot(probe, path = path, file_preview = TRUE, opener = opener))
  expect_equal(calls$opened, path)
  expect_equal(calls$printed, 0L)

  # A base-graphics preview has no printable object; the path is enough.
  preview_plot(NULL, path = path, file_preview = TRUE, opener = opener)
  expect_equal(calls$opened, c(path, path))
})

test_that("preview_plot skips a file preview with no usable path", {
  opened <- character(0)
  opener <- function(path) opened <<- c(opened, path)

  preview_plot(NULL, path = NULL, file_preview = TRUE, opener = opener)
  preview_plot(NULL, path = NA_character_, file_preview = TRUE, opener = opener)
  preview_plot(NULL, path = file.path(tempdir(), "artma-no-such-plot.png"), file_preview = TRUE, opener = opener)

  expect_equal(opened, character(0))
})

test_that("preview_plot never opens files inside a forked worker", {
  opened <- character(0)
  opener <- function(path) opened <<- c(opened, path)

  path <- local_tempfile(fileext = ".png")
  file.create(path)

  with_forked_worker_flag(
    preview_plot(NULL, path = path, file_preview = TRUE, opener = opener)
  )

  expect_equal(opened, character(0))
})

test_that("export_named_plots returns one path per plot, NA for skipped entries", {
  dir <- local_tempdir()
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  paths <- export_named_plots(
    plots = list(first = plot, skipped = NULL, second = plot),
    base_name = "preview_test",
    export_path = dir,
    graph_scale = 1,
    width = 200,
    height = 200
  )

  expect_equal(length(paths), 3L)
  expect_true(is.na(paths[2]))
  expect_true(all(file.exists(paths[c(1, 3)])))
  expect_equal(basename(paths[1]), "preview_test_first.png")
})

test_that("record = FALSE keeps preview tempfiles out of the output-file capture", {
  box::use(
    artma / libs / infrastructure / output_files[
      begin_output_file_capture, end_output_file_capture
    ]
  )

  dir <- local_tempdir()
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  id <- begin_output_file_capture()
  export_named_plots(
    plots = list(preview = plot),
    base_name = "preview_test",
    export_path = dir,
    graph_scale = 1,
    width = 200,
    height = 200,
    record = FALSE
  )
  expect_equal(end_output_file_capture(id), character(0))

  id <- begin_output_file_capture()
  paths <- export_named_plots(
    plots = list(recorded = plot),
    base_name = "preview_test",
    export_path = dir,
    graph_scale = 1,
    width = 200,
    height = 200,
    record = TRUE
  )
  recorded <- end_output_file_capture(id)
  expect_equal(recorded, normalizePath(paths[1], mustWork = FALSE))
})

test_that("record = FALSE also suppresses the HTML companion export", {
  local_options(list(artma.visualization.export_html = TRUE))

  dir <- local_tempdir()
  path <- file.path(dir, "preview.png")
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  save_plot(plot, path, width = 200, height = 200, record = FALSE)

  expect_true(file.exists(path))
  expect_false(file.exists(file.path(dir, "preview.html")))
})
