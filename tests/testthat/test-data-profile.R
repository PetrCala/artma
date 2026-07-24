box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_true,
    test_that
  ]
)

box::use(
  artma / data / profile[
    profile_column,
    is_binary_column,
    detect_dummy_groups
  ]
)

# profile_column --------------------------------------------------------------

test_that("profile_column reports sequential numeric patterns", {
  analysis <- profile_column(1:10)

  expect_true(analysis$is_sequential)
  expect_true(analysis$is_unique)
  expect_true(analysis$is_numeric)
  expect_equal(analysis$uniqueness_ratio, 1.0)
  expect_equal(analysis$mean, 5.5)
})

test_that("profile_column drops NA before profiling", {
  analysis <- profile_column(c(100, NA, 200, NA, 300))

  expect_equal(analysis$mean, 200)
  expect_equal(analysis$min, 100)
  expect_equal(analysis$max, 300)
})

test_that("profile_column returns the empty skeleton for all-NA input", {
  analysis <- profile_column(c(NA, NA, NA))

  expect_false(analysis$is_sequential)
  expect_false(analysis$is_unique)
  expect_false(analysis$is_numeric)
  expect_equal(analysis$uniqueness_ratio, 0)
  expect_true(is.na(analysis$mean))
})

test_that("profile_column treats character columns as non-numeric", {
  analysis <- profile_column(c("Study A", "Study B", "Study C"))

  expect_false(analysis$is_numeric)
  expect_true(analysis$is_unique)
})

# is_binary_column ------------------------------------------------------------

test_that("is_binary_column accepts 0/1 numeric columns", {
  expect_true(is_binary_column(c(0, 1, 1, 0, 1)))
})

test_that("is_binary_column accepts logical columns", {
  expect_true(is_binary_column(c(TRUE, FALSE, TRUE)))
})

test_that("is_binary_column ignores NA when counting distinct values", {
  expect_true(is_binary_column(c(0, 1, NA, 1, NA)))
})

test_that("is_binary_column rejects constant columns", {
  expect_false(is_binary_column(rep(1, 10)))
  expect_false(is_binary_column(rep(0, 10)))
})

test_that("is_binary_column rejects near-binary numerics with a third value", {
  expect_false(is_binary_column(c(0, 1, 2)))
})

test_that("is_binary_column rejects all-NA columns", {
  expect_false(is_binary_column(c(NA, NA)))
})

# detect_dummy_groups ---------------------------------------------------------

test_that("detect_dummy_groups groups binary columns sharing a prefix", {
  df <- data.frame(
    country_usa = c(0, 1, 0, 1),
    country_uk = c(1, 0, 1, 0),
    country_other = c(0, 0, 1, 1),
    numeric_var = c(1, 2, 3, 4)
  )

  result <- detect_dummy_groups(names(df), df)

  expect_equal(nrow(result), 3)
  expect_true(all(result$group_id == "dummy_country"))
  expect_equal(sum(result$is_reference), 1)
  expect_true(result$is_reference[result$var_name == "country_other"])
})
