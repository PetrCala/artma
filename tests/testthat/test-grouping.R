box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_length,
    expect_true,
    test_that
  ]
)

box::use(
  artma / libs / core / grouping[
    format_group_value,
    is_group_value_na,
    resolve_group_threshold,
    resolve_variable_groups
  ]
)

# A compact view of a group list: label -> row indices. Pins both the label
# formatting and the exact row membership produced by the resolver.
group_map <- function(groups) {
  stats::setNames(
    lapply(groups, function(g) g$row_idx),
    vapply(groups, function(g) g$label, character(1))
  )
}

test_that("is_group_value_na treats NULL, empty and scalar NA as unset", {
  expect_true(is_group_value_na(NULL))
  expect_true(is_group_value_na(NA))
  expect_true(is_group_value_na(character(0)))
  expect_true(is_group_value_na(NA_character_))
  expect_false(is_group_value_na(0))
  expect_false(is_group_value_na("mean"))
})

test_that("resolve_group_threshold resolves literals, mean and median over the given basis", {
  expect_equal(resolve_group_threshold(1.5, c(0, 1, 2)), 1.5)
  expect_equal(resolve_group_threshold("mean", c(0, 1, 2, 5)), 2)
  expect_equal(resolve_group_threshold("median", c(0, 1, 2, 5)), 1.5)
  # NA values dropped before the reduction.
  expect_equal(resolve_group_threshold("mean", c(0, NA, 2)), 1)
  # Empty basis yields NA for mean/median.
  expect_true(is.na(resolve_group_threshold("mean", numeric(0))))
  expect_true(is.na(resolve_group_threshold("median", numeric(0))))
})

test_that("format_group_value rounds numerics and stringifies the rest", {
  expect_identical(format_group_value(1.23456, 3), "1.235")
  expect_identical(format_group_value(2, 3), "2")
  expect_identical(format_group_value("high", 3), "high")
})

# Characterization: best_practice_estimate call-site profile ------------------
# auto_levels = TRUE, threshold basis defaults to var_values. The expected
# assignments below are the verbatim output of the pre-extraction
# resolve_bpe_factor_groups() on the same fixtures.

bpe_groups <- function(equal_val, gltl_val, var_values) {
  resolve_variable_groups(
    var_label = "Top",
    equal_val = equal_val,
    gltl_val = gltl_val,
    var_values = var_values,
    round_to = 3,
    auto_levels = TRUE
  )
}

test_that("bpe profile: equal split", {
  v <- c(0, 1, 0, 1, 2, NA, 5)
  expect_identical(
    group_map(bpe_groups(1, NA, v)),
    list(`Top = 1` = c(2L, 4L))
  )
})

test_that("bpe profile: gltl mean/median/literal over full non-NA values", {
  v <- c(0, 1, 0, 1, 2, NA, 5)
  expect_identical(
    group_map(bpe_groups(NA, "mean", v)),
    list(`Top >= 1.5` = c(5L, 7L), `Top < 1.5` = c(1L, 2L, 3L, 4L))
  )
  expect_identical(
    group_map(bpe_groups(NA, "median", v)),
    list(`Top >= 1` = c(2L, 4L, 5L, 7L), `Top < 1` = c(1L, 3L))
  )
  expect_identical(
    group_map(bpe_groups(NA, 1.5, v)),
    list(`Top >= 1.5` = c(5L, 7L), `Top < 1.5` = c(1L, 2L, 3L, 4L))
  )
})

test_that("bpe profile: equal and gltl combine, greater-equal takes the tie", {
  v <- c(0, 1, 0, 1, 2, NA, 5)
  expect_identical(
    group_map(bpe_groups(0, 2, v)),
    list(
      `Top = 0` = c(1L, 3L),
      `Top >= 2` = c(5L, 7L),
      `Top < 2` = c(1L, 2L, 3L, 4L)
    )
  )
})

test_that("bpe profile: per-level fallback within [2, 12] distinct levels", {
  v <- c(0, 1, 0, 1, 2, NA, 5)
  expect_identical(
    group_map(bpe_groups(NA, NA, v)),
    list(
      `Top = 0` = c(1L, 3L),
      `Top = 1` = c(2L, 4L),
      `Top = 2` = 5L,
      `Top = 5` = 7L
    )
  )
  # Too many distinct levels: no fallback groups.
  expect_length(bpe_groups(NA, NA, 1:20), 0)
  # Fewer than two distinct levels: no fallback groups.
  expect_length(bpe_groups(NA, NA, c(3, 3, 3)), 0)
})

# Characterization: effect_summary_stats call-site profile --------------------
# auto_levels = FALSE (its own all-data fallback lives outside the resolver),
# threshold basis restricted to rows with finite effect and study size. The
# expected assignments are the verbatim output of the pre-extraction inline
# split logic in effect_summary_stats().

ess_groups <- function(equal_val, gltl_val, var_values, threshold_values) {
  resolve_variable_groups(
    var_label = "Top",
    equal_val = equal_val,
    gltl_val = gltl_val,
    var_values = var_values,
    round_to = 3,
    threshold_values = threshold_values
  )
}

test_that("ess profile: threshold basis differs from the grouping basis", {
  vd <- c(0, 1, 0, 1, 2, NA, 5)
  # Row 7 (value 5) dropped from the mean basis because its effect is infinite;
  # mean of c(0, 1, 0, 1, 2) is 0.8, not the 1.5 the full column would give.
  filtered <- vd[c(1, 2, 3, 4, 5)]
  expect_identical(
    group_map(ess_groups(NA, "mean", vd, filtered)),
    list(`Top >= 0.8` = c(2L, 4L, 5L, 7L), `Top < 0.8` = c(1L, 3L))
  )
})

test_that("ess profile: equal split matches numeric label formatting", {
  vd <- c(0, 1, 0, 1, 2, NA, 5)
  expect_identical(
    group_map(ess_groups(1, NA, vd, vd[!is.na(vd)])),
    list(`Top = 1` = c(2L, 4L))
  )
})

test_that("ess profile: no per-level fallback (auto_levels off)", {
  vd <- c(0, 1, 0, 1, 2, NA, 5)
  expect_length(ess_groups(NA, NA, vd, vd[!is.na(vd)]), 0)
})
