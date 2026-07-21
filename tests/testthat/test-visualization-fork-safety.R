box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_null,
    expect_true,
    skip_if,
    test_that
  ]
)

can_fork <- function() {
  cores <- tryCatch(parallel::detectCores(), error = function(err) NA_integer_)
  !identical(.Platform$OS.type, "windows") && is.numeric(cores) && !is.na(cores) && cores >= 2L
}

# Pixel dimensions straight out of the PNG header: an 8-byte signature, then the
# IHDR chunk whose payload opens with the width and height as big-endian 32-bit
# integers. Reading them here keeps the assertions dependency-free.
png_dim <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  header <- readBin(con, "raw", n = 24L)
  if (length(header) < 24L) {
    return(c(NA_integer_, NA_integer_))
  }
  as.integer(c(
    sum(as.integer(header[17:20]) * 256^(3:0)),
    sum(as.integer(header[21:24]) * 256^(3:0))
  ))
}

# The regression this file guards: on macOS the Objective-C runtime aborts a
# forked child that opens a ragg or quartz PNG device, so every method exporting
# graphics died silently under `artma.general.parallel = TRUE`. The worker was
# killed rather than throwing, so it surfaced as a failure with an empty message.

test_that("save_plot writes a valid PNG from inside a forked worker", {
  box::use(
    artma / modules / method_execution[execute_method_layer],
    artma / visualization / export[save_plot],
    artma / visualization / fork_safety[fork_safe_png_available]
  )

  skip_if(!can_fork(), "forking is unavailable")
  skip_if(!fork_safe_png_available(), "no fork-safe graphics device on this platform")

  export_dir <- withr::local_tempdir()
  plot <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  outcomes <- execute_method_layer(
    c("first", "second"),
    run_one = function(name) {
      save_plot(plot, file.path(export_dir, paste0(name, ".png")))
      name
    },
    workers = 2L
  )

  for (outcome in outcomes) {
    expect_null(outcome$error)
  }
  expect_equal(vapply(outcomes, function(o) o$value, character(1)), c("first", "second"))

  for (name in c("first", "second")) {
    path <- file.path(export_dir, paste0(name, ".png"))
    expect_true(file.exists(path))
    # A device that opened with the wrong units writes a near-empty file, so
    # assert on the raster dimensions rather than mere existence.
    expect_equal(png_dim(path), c(800L, 1100L))
  }
})

test_that("open_png_device writes a valid PNG from inside a forked worker", {
  box::use(
    artma / modules / method_execution[execute_method_layer],
    artma / visualization / export[open_png_device],
    artma / visualization / fork_safety[fork_safe_png_available]
  )

  skip_if(!can_fork(), "forking is unavailable")
  skip_if(!fork_safe_png_available(), "no fork-safe graphics device on this platform")

  export_dir <- withr::local_tempdir()

  outcomes <- execute_method_layer(
    c("first", "second"),
    run_one = function(name) {
      open_png_device(file.path(export_dir, paste0(name, ".png")), width = 800, height = 600)
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::plot(1:10)
      name
    },
    workers = 2L
  )

  for (outcome in outcomes) {
    expect_null(outcome$error)
  }

  for (name in c("first", "second")) {
    path <- file.path(export_dir, paste0(name, ".png"))
    expect_true(file.exists(path))
    expect_equal(png_dim(path), c(800L, 600L))
  }
})

test_that("the forked-worker flag is confined to the child process", {
  box::use(
    artma / modules / method_execution[execute_method_layer],
    artma / visualization / fork_safety[in_forked_worker]
  )

  skip_if(!can_fork(), "forking is unavailable")

  outcomes <- execute_method_layer(
    c("a", "b"),
    run_one = function(name) in_forked_worker(),
    workers = 2L
  )

  expect_true(all(vapply(outcomes, function(o) isTRUE(o$value), logical(1))))
  expect_false(in_forked_worker())
})

test_that("sequential runs keep the default device", {
  box::use(
    artma / modules / method_execution[execute_method_layer],
    artma / visualization / fork_safety[use_fork_safe_png_device]
  )

  outcomes <- execute_method_layer(
    "a",
    run_one = function(name) use_fork_safe_png_device(),
    workers = 1L
  )

  expect_false(isTRUE(outcomes[[1L]]$value))
})

test_that("fork_safe_png_device sizes the raster in inches", {
  box::use(artma / visualization / fork_safety[fork_safe_png_available, fork_safe_png_device])

  skip_if(!fork_safe_png_available(), "cairo is unavailable")

  path <- withr::local_tempfile(fileext = ".png")

  # `ggsave()` resolves the size to inches before handing it to the device.
  fork_safe_png_device(path, width = 4, height = 3, res = 100)
  graphics::plot(1:10)
  grDevices::dev.off()

  expect_equal(png_dim(path), c(400L, 300L))
})

test_that("fork_safe_png_available probes the device rather than the build flag", {
  box::use(artma / visualization / fork_safety[fork_safe_png_available, probe_cairo_png])

  # `capabilities("cairo")` reports how R was built; some headless macOS
  # installations report TRUE while the device writes nothing, which is why the
  # answer comes from an actual write.
  expect_equal(fork_safe_png_available(refresh = TRUE), probe_cairo_png())
  expect_true(is.logical(probe_cairo_png()))
  expect_equal(length(probe_cairo_png()), 1L)
})

test_that("a platform without a working cairo device never forks its graphics", {
  box::use(
    artma / visualization / fork_safety[
      fork_safe_png_available, graphics_fork_is_hostile, graphics_survive_fork,
      use_fork_safe_png_device
    ]
  )

  # Without a working cairo device there is nothing safe to swap in, so a
  # hostile platform must report that graphics cannot survive a fork. That is
  # what pushes `resolve_worker_count()` back to sequential execution.
  if (graphics_fork_is_hostile() && !fork_safe_png_available()) {
    expect_false(graphics_survive_fork())
    expect_false(use_fork_safe_png_device())
  } else {
    expect_true(graphics_survive_fork())
  }
})

test_that("resolve_worker_count stays sequential when a fork cannot draw", {
  box::use(artma / modules / method_execution[resolve_worker_count])

  withr::local_options(list(artma.visualization.export_graphics = TRUE))

  expect_equal(
    resolve_worker_count(
      4L,
      is_interactive = FALSE, os_type = "unix", n_cores = 8L,
      max_workers = Inf, graphics_fork_safe = FALSE
    ),
    1L
  )
  expect_equal(
    resolve_worker_count(
      4L,
      is_interactive = FALSE, os_type = "unix", n_cores = 8L,
      max_workers = Inf, graphics_fork_safe = TRUE
    ),
    4L
  )
})

test_that("resolve_worker_count ignores fork-unsafe graphics when not exporting", {
  box::use(artma / modules / method_execution[resolve_worker_count])

  withr::local_options(list(artma.visualization.export_graphics = FALSE))

  expect_equal(
    resolve_worker_count(
      4L,
      is_interactive = FALSE, os_type = "unix", n_cores = 8L,
      max_workers = Inf, graphics_fork_safe = FALSE
    ),
    4L
  )
})
