box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_gte,
    expect_identical,
    expect_length,
    expect_true,
    test_that
  ],
  artma / visualization / ticks[
    MAX_AXIS_TICKS,
    format_tick_labels,
    generate_regular_ticks,
    resolve_tick_interval,
    thin_ticks,
    tick_label_digits,
    tick_min_separation
  ]
)


# --- Label formatting -------------------------------------------------------

# Regression guard for the ggtext removal: tick labels used to be wrapped in
# HTML color spans that no renderer in the package interprets any more, so
# every axis printed its markup as literal text.
test_that("format_tick_labels emits plain text, never markup", {
  labels <- format_tick_labels(c(1, 50, 72.05, 369.54))

  expect_false(any(grepl("<", labels, fixed = TRUE)))
  expect_false(any(grepl("span", labels, fixed = TRUE)))
})


test_that("format_tick_labels drops decimals when ticks are far apart", {
  expect_identical(
    format_tick_labels(c(1, 50, 72.05, 100, 369.54)),
    c("1", "50", "72", "100", "370")
  )
})


test_that("format_tick_labels keeps enough precision for small effects", {
  labels <- format_tick_labels(c(-0.004, 0, 0.0035, 0.01))

  expect_identical(anyDuplicated(labels), 0L)
  expect_identical(labels[2], "0")
  expect_false(labels[3] == "0")
})


# A critical value labelled "2" tells the reader the line sits at 2.0. The
# label may be rounded, but not by enough to change what it says.
test_that("format_tick_labels keeps landmark values honest", {
  labels <- format_tick_labels(c(-6.24, -4, -1.96, 0.69, 1.96, 4, 7.67))

  expect_true("1.96" %in% labels)
  expect_false("2" %in% labels)
  # Grid ticks stay free of pointless decimals.
  expect_true("4" %in% labels)
})


test_that("format_tick_labels still rounds where the error is immaterial", {
  # 0.46 out of a 368-wide range is invisible on the axis.
  expect_true("368" %in% format_tick_labels(c(1, 50, 100, 150, 181, 250, 300, 368.46)))
})


test_that("tick_label_digits never rounds a non-zero tick to zero", {
  expect_equal(tick_label_digits(c(0, 100, 200)), 0L)
  expect_gte(tick_label_digits(c(0, 0.002, 0.5)), 3L)
})


test_that("format_tick_labels preserves NA", {
  expect_true(is.na(format_tick_labels(c(1, NA, 3))[2]))
})


# --- Interval selection -----------------------------------------------------

test_that("resolve_tick_interval returns round numbers across magnitudes", {
  expect_equal(resolve_tick_interval(0.008), 0.001)
  expect_equal(resolve_tick_interval(21.3), 2)
  expect_equal(resolve_tick_interval(368.5), 50)
  expect_equal(resolve_tick_interval(5000), 500)
})


test_that("resolve_tick_interval keeps the tick count near the target", {
  for (range_size in c(0.03, 0.7, 4, 21.3, 240, 368.5, 5000, 1e6)) {
    n_ticks <- range_size / resolve_tick_interval(range_size)
    expect_true(n_ticks >= 3 && n_ticks <= MAX_AXIS_TICKS)
  }
})


test_that("resolve_tick_interval stays positive for degenerate ranges", {
  expect_true(resolve_tick_interval(0) > 0)
  expect_true(resolve_tick_interval(-1) > 0)
  expect_true(resolve_tick_interval(NA_real_) > 0)
})


# --- Separation and thinning ------------------------------------------------

test_that("tick_min_separation caps the axis at MAX_AXIS_TICKS labels", {
  expect_equal(tick_min_separation(368.5), 368.5 / MAX_AXIS_TICKS)
  expect_equal(tick_min_separation(1), 1 / MAX_AXIS_TICKS)
  expect_equal(tick_min_separation(0), 0)
})


test_that("thin_ticks collapses identical ticks even without a separation", {
  kept <- thin_ticks(c(5, 5, 5), min_distance = 0, priority = c(1, 9, 1))

  expect_identical(kept, 2L)
})


test_that("thin_ticks keeps high-priority ticks and drops colliding filler", {
  ticks <- c(72.05, 1, 369.54, 50, 100, 350)
  priority <- c(4, 2, 2, 1, 1, 1)

  kept <- thin_ticks(ticks, min_distance = 26.3, priority = priority)

  expect_equal(ticks[kept], c(1, 72.05, 100, 369.54))
})


test_that("thin_ticks returns indices ordered by tick value", {
  ticks <- c(10, -5, 3)
  kept <- thin_ticks(ticks, min_distance = 1)

  expect_identical(kept, c(2L, 3L, 1L))
})


test_that("thin_ticks resolves exact duplicates in favor of priority", {
  ticks <- c(2, 2)
  kept <- thin_ticks(ticks, min_distance = 0.5, priority = c(1, 5))

  expect_identical(kept, 2L)
})


test_that("thin_ticks drops non-finite ticks", {
  kept <- thin_ticks(c(1, NA, 5, Inf), min_distance = 1)

  expect_identical(kept, c(1L, 3L))
})


test_that("thin_ticks keeps everything when no separation is required", {
  expect_length(thin_ticks(c(1, 1.001, 2), min_distance = 0), 3L)
})


test_that("thin_ticks handles an empty tick set", {
  expect_length(thin_ticks(numeric(0), min_distance = 1), 0L)
})


# --- Regular tick generation ------------------------------------------------

test_that("generate_regular_ticks avoids bounds and special values", {
  ticks <- generate_regular_ticks(
    lower = -11.3,
    upper = 10,
    interval = 2,
    edge_distance = 1.52,
    special_values = 0.2,
    special_distance = 1.52
  )

  expect_false(any(abs(ticks - 0.2) < 1.52))
  expect_false(any(abs(ticks - (-11.3)) < 1.52))
  expect_true(all(ticks > -11.3 & ticks < 10))
})
