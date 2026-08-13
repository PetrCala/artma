box::use(
  testthat[expect_equal, expect_error, test_that],
  artma / libs / core / string[count_phrases]
)

test_that("count_phrases pluralizes each count and keeps the given order", {
  expect_equal(
    count_phrases(c(table = 2, plot = 1)),
    c("2 tables", "1 plot")
  )
})

test_that("count_phrases drops the zeros", {
  expect_equal(count_phrases(c(table = 2, plot = 0)), "2 tables")
  expect_equal(count_phrases(c(table = 0, plot = 0)), character())
})

test_that("count_phrases handles multi-word nouns and missing counts", {
  expect_equal(count_phrases(c("other file" = 3)), "3 other files")
  expect_equal(count_phrases(c(table = NA_integer_, plot = 1)), "1 plot")
})

test_that("count_phrases returns nothing for an empty input", {
  expect_equal(count_phrases(integer()), character())
  expect_equal(count_phrases(list()), character())
})

test_that("count_phrases rejects unnamed counts", {
  expect_error(count_phrases(c(1, 2)), "named")
})
