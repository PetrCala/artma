test_that("str_trim removes whitespace correctly", {
  box::use(artma / libs / infrastructure / polyfills[str_trim])

  # Test both sides
  expect_equal(str_trim("  hello  ", side = "both"), "hello")
  expect_equal(str_trim("  hello  "), "hello") # default is both

  # Test left side only
  expect_equal(str_trim("  hello  ", side = "left"), "hello  ")

  # Test right side only
  expect_equal(str_trim("  hello  ", side = "right"), "  hello")

  # Test with no whitespace
  expect_equal(str_trim("hello", side = "both"), "hello")

  # Test with tabs and newlines
  expect_equal(str_trim("\t\nhello\t\n", side = "both"), "hello")
})

test_that("str_replace_all replaces all matches", {
  box::use(artma / libs / infrastructure / polyfills[str_replace_all])

  # Test basic replacement
  expect_equal(str_replace_all("hello world", "o", "0"), "hell0 w0rld")

  # Test regex patterns
  expect_equal(str_replace_all("test123abc", "[0-9]", "X"), "testXXXabc")

  # Test removing characters
  expect_equal(str_replace_all("a-b-c", "-", ""), "abc")

  # Test with no matches
  expect_equal(str_replace_all("hello", "x", "y"), "hello")
})

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

test_that("str_to_title converts to title case", {
  box::use(artma / libs / infrastructure / polyfills[str_to_title])

  # Test basic conversion
  expect_equal(str_to_title("hello world"), "Hello World")

  # Test with mixed case
  expect_equal(str_to_title("hELLo WoRLd"), "Hello World")

  # Test with single word
  expect_equal(str_to_title("hello"), "Hello")
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

test_that("glue interpolates strings", {
  box::use(artma / libs / infrastructure / polyfills[glue])

  # Test basic interpolation
  name <- "World"
  result <- glue("Hello {name}")
  expect_equal(result, "Hello World")

  # Test with multiple variables
  x <- 5
  y <- 10
  result <- glue("x={x}, y={y}")
  expect_equal(result, "x=5, y=10")

  # Test without interpolation
  result <- glue("plain text")
  expect_equal(result, "plain text")

  # Test with concatenation
  result <- glue("a", "b", "c", .sep = "-")
  expect_equal(result, "a-b-c")
})

test_that("glue_collapse joins strings", {
  box::use(artma / libs / infrastructure / polyfills[glue_collapse])

  # Test basic collapse
  result <- glue_collapse(c("a", "b", "c"), sep = ", ")
  expect_equal(result, "a, b, c")

  # Test with last separator
  result <- glue_collapse(c("a", "b", "c"), sep = ", ", last = " and ")
  expect_equal(result, "a, b and c")

  # Test with empty vector
  result <- glue_collapse(character(0), sep = ", ")
  expect_equal(result, "")

  # Test with single element
  result <- glue_collapse("a", sep = ", ")
  expect_equal(result, "a")
})

test_that("digest creates consistent hashes", {
  box::use(artma / libs / infrastructure / polyfills[digest])

  # Test basic hashing
  obj1 <- list(a = 1, b = 2)
  hash1 <- digest(obj1)
  expect_type(hash1, "character")
  expect_equal(nchar(hash1), 16) # Should be 16 hex characters

  # Test consistency - same object should produce same hash
  hash2 <- digest(obj1)
  expect_equal(hash1, hash2)

  # Test different objects produce different hashes
  obj2 <- list(a = 1, b = 3)
  hash3 <- digest(obj2)
  expect_false(hash1 == hash3)

  # Test with different data types
  expect_type(digest(1:10), "character")
  expect_type(digest("test"), "character")
  expect_type(digest(data.frame(x = 1:3)), "character")
})

test_that("edit_file validates file existence", {
  box::use(artma / libs / infrastructure / polyfills[edit_file])

  # Test error on non-existent file
  expect_error(
    edit_file(tempfile()),
    "File does not exist"
  )

  # Note: We cannot test the actual file.edit() call as it opens an interactive editor
  # which would hang the tests. The actual functionality is tested manually.
})

test_that("polyfills match original behavior on real usage patterns", {
  box::use(
    artma / libs / infrastructure / polyfills[str_replace_all, str_trim, map_chr, keep]
  )

  # Test pattern from inst/artma/libs/string.R
  input_string <- "Test String! @#$"
  str_out <- str_replace_all(input_string, "[^a-zA-Z0-9]", "_")
  str_out <- tolower(str_out)
  str_out <- str_trim(str_out, side = "both")
  str_out <- str_replace_all(str_out, "^_+|_+$", "")
  # After removing leading/trailing underscores, we get "test_string"
  expect_equal(str_out, "test_string")

  # Test pattern from inst/artma/data/utils.R
  defs <- list(
    list(name = "data.colnames.effect", value = 1),
    list(name = "data.colnames.se", value = 2),
    list(name = "other.option", value = 3)
  )
  defs <- keep(defs, function(x) grepl("^data.colnames", x$name))
  expect_length(defs, 2)

  names <- map_chr(defs, "name")
  expect_equal(names, c("data.colnames.effect", "data.colnames.se"))
})
