test_that("str_remove removes first match", {
  box::use(artma / libs / infrastructure / polyfills[str_remove])

  # Test basic removal
  expect_equal(str_remove("hello world", "world"), "hello ")

  # Test regex pattern
  expect_equal(str_remove("test123", "[0-9]+"), "test")

  # Test removing from start
  expect_equal(str_remove("prefix_test", "^prefix_"), "test")

  # Test with no match
  expect_equal(str_remove("hello", "xyz"), "hello")
})

test_that("map_chr extracts character values", {
  box::use(artma / libs / infrastructure / polyfills[map_chr])

  # Test with function
  result <- map_chr(1:3, as.character)
  expect_equal(result, c("1", "2", "3"))

  # Test with name extraction
  test_list <- list(
    list(name = "a", value = 1),
    list(name = "b", value = 2),
    list(name = "c", value = 3)
  )
  result <- map_chr(test_list, "name")
  expect_equal(result, c("a", "b", "c"))

  # Test with formula-style function
  result <- map_chr(c("a", "b", "c"), toupper)
  expect_equal(result, c("A", "B", "C"))
})

test_that("map_lgl returns logical values", {
  box::use(artma / libs / infrastructure / polyfills[map_lgl])

  # Test with function
  result <- map_lgl(1:3, function(x) x > 1)
  expect_equal(result, c(FALSE, TRUE, TRUE))

  # Test with is.numeric
  result <- map_lgl(list(1, "a", 2), is.numeric)
  expect_equal(result, c(TRUE, FALSE, TRUE))
})

test_that("keep filters elements", {
  box::use(artma / libs / infrastructure / polyfills[keep])

  # Test basic filtering
  result <- keep(1:10, function(x) x %% 2 == 0)
  expect_equal(result, c(2, 4, 6, 8, 10))

  # Test with list
  test_list <- list(a = 1, b = 2, c = 3, d = 4)
  result <- keep(test_list, function(x) x > 2)
  expect_equal(result, list(c = 3, d = 4))

  # Test with character vector
  result <- keep(c("a", "ab", "abc"), function(x) nchar(x) > 1)
  expect_equal(result, c("ab", "abc"))
})
