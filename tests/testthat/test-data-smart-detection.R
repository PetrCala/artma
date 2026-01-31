box::use(
  artma / data / smart_detection[
    detect_delimiter,
    detect_encoding,
    smart_read_csv,
    validate_df_structure
  ]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_error <- getFromNamespace("expect_error", "testthat")
expect_no_error <- getFromNamespace("expect_no_error", "testthat")


# Helper function to create temp CSV files for testing
create_temp_csv <- function(content, delimiter = ",") {
  tmp_file <- tempfile(fileext = ".csv")
  lines <- if (is.character(content)) {
    content
  } else {
    paste(apply(content, 1, paste, collapse = delimiter), collapse = "\n")
  }
  writeLines(lines, tmp_file)
  tmp_file
}


test_that("detect_delimiter correctly identifies comma delimiter", {
  csv_content <- c(
    "name,age,city",
    "Alice,30,NYC",
    "Bob,25,LA"
  )
  tmp_file <- create_temp_csv(csv_content)
  on.exit(unlink(tmp_file))

  delim <- detect_delimiter(tmp_file)
  expect_equal(delim, ",")
})


test_that("detect_delimiter correctly identifies semicolon delimiter", {
  csv_content <- c(
    "name;age;city",
    "Alice;30;NYC",
    "Bob;25;LA"
  )
  tmp_file <- create_temp_csv(csv_content)
  on.exit(unlink(tmp_file))

  delim <- detect_delimiter(tmp_file)
  expect_equal(delim, ";")
})


test_that("detect_delimiter correctly identifies tab delimiter", {
  csv_content <- "name\tage\tcity\nAlice\t30\tNYC\nBob\t25\tLA"
  tmp_file <- tempfile(fileext = ".csv")
  writeLines(csv_content, tmp_file)
  on.exit(unlink(tmp_file))

  delim <- detect_delimiter(tmp_file)
  expect_equal(delim, "\t")
})


test_that("detect_delimiter correctly identifies pipe delimiter", {
  csv_content <- c(
    "name|age|city",
    "Alice|30|NYC",
    "Bob|25|LA"
  )
  tmp_file <- create_temp_csv(csv_content)
  on.exit(unlink(tmp_file))

  delim <- detect_delimiter(tmp_file)
  expect_equal(delim, "|")
})


test_that("detect_delimiter returns comma for empty file", {
  tmp_file <- tempfile(fileext = ".csv")
  writeLines(character(0), tmp_file)
  on.exit(unlink(tmp_file))

  delim <- detect_delimiter(tmp_file)
  expect_equal(delim, ",")
})


test_that("detect_delimiter handles inconsistent delimiters by choosing most consistent", {
  # File with mixed delimiters but semicolon is most consistent
  csv_content <- c(
    "name;age;city",
    "Alice;30;NYC",
    "Bob;25;LA,Extra"
  )
  tmp_file <- create_temp_csv(csv_content)
  on.exit(unlink(tmp_file))

  delim <- detect_delimiter(tmp_file)
  expect_equal(delim, ";")
})


test_that("detect_encoding returns valid encoding", {
  tmp_file <- tempfile(fileext = ".csv")
  writeLines("test,data", tmp_file)
  on.exit(unlink(tmp_file))

  encoding <- detect_encoding(tmp_file)
  expect_true(encoding %in% c("UTF-8", "latin1", "ISO-8859-1", "CP1252"))
})


test_that("smart_read_csv reads comma-delimited file correctly", {
  csv_content <- c(
    "name,age,city",
    "Alice,30,NYC",
    "Bob,25,LA"
  )
  tmp_file <- create_temp_csv(csv_content)
  on.exit(unlink(tmp_file))

  df <- smart_read_csv(tmp_file)

  expect_equal(nrow(df), 2)
  expect_equal(ncol(df), 3)
  expect_equal(names(df), c("name", "age", "city"))
  expect_equal(df$name, c("Alice", "Bob"))
})


test_that("smart_read_csv reads semicolon-delimited file correctly", {
  csv_content <- c(
    "name;age;city",
    "Alice;30;NYC",
    "Bob;25;LA"
  )
  tmp_file <- create_temp_csv(csv_content)
  on.exit(unlink(tmp_file))

  df <- smart_read_csv(tmp_file)

  expect_equal(nrow(df), 2)
  expect_equal(ncol(df), 3)
  expect_equal(names(df), c("name", "age", "city"))
})


test_that("smart_read_csv handles NA values correctly", {
  csv_content <- c(
    "name,age,city",
    "Alice,30,NYC",
    "Bob,NA,LA",
    "Charlie,," # Empty city
  )
  tmp_file <- create_temp_csv(csv_content)
  on.exit(unlink(tmp_file))

  df <- smart_read_csv(tmp_file)

  expect_true(is.na(df$age[2]))
  expect_true(is.na(df$city[3]))
})


test_that("smart_read_csv handles quoted fields", {
  csv_content <- c(
    "name,description", # nolint
    '"Alice","Works in tech, enjoys coding"',
    '"Bob","Likes music, plays guitar"'
  )
  tmp_file <- create_temp_csv(csv_content)
  on.exit(unlink(tmp_file))

  df <- smart_read_csv(tmp_file)

  expect_equal(nrow(df), 2)
  expect_true(grepl("tech", df$description[1]))
})


test_that("smart_read_csv can use explicit delimiter", {
  csv_content <- c(
    "name|age|city",
    "Alice|30|NYC"
  )
  tmp_file <- create_temp_csv(csv_content)
  on.exit(unlink(tmp_file))

  df <- smart_read_csv(tmp_file, delim = "|")

  expect_equal(ncol(df), 3)
  expect_equal(df$name, "Alice")
})


test_that("validate_df_structure removes empty columns", {
  df <- data.frame(
    name = c("Alice", "Bob"),
    age = c(30, 25),
    empty = c(NA, NA)
  )

  cleaned <- validate_df_structure(df, "test_path")

  expect_equal(ncol(cleaned), 2)
  expect_true(!"empty" %in% names(cleaned))
})


test_that("validate_df_structure handles duplicate column names", {
  df <- data.frame(
    name = c("Alice", "Bob"),
    age = c(30, 25)
  )
  names(df) <- c("name", "name")

  cleaned <- validate_df_structure(df, "test_path")

  expect_equal(ncol(cleaned), 2)
  expect_true("name" %in% names(cleaned))
  expect_true("name_1" %in% names(cleaned))
})


test_that("validate_df_structure removes trailing empty rows", {
  df <- data.frame(
    name = c("Alice", "Bob", NA, NA),
    age = c(30, 25, NA, NA)
  )

  cleaned <- validate_df_structure(df, "test_path")

  expect_equal(nrow(cleaned), 2)
})


test_that("validate_df_structure errors on empty data frame after cleaning", {
  df <- data.frame(
    empty1 = c(NA, NA),
    empty2 = c(NA, NA)
  )

  expect_error(
    validate_df_structure(df, "test_path"),
    "empty"
  )
})


test_that("validate_df_structure errors on zero-row data frame", {
  df <- data.frame(name = character(0), age = numeric(0))

  expect_error(
    validate_df_structure(df, "test_path"),
    "empty.*0 rows"
  )
})


test_that("validate_df_structure errors on zero-column data frame", {
  df <- data.frame(row.names = 1:5)

  expect_error(
    validate_df_structure(df, "test_path"),
    "no columns"
  )
})


test_that("smart_read_csv with auto-detection works end-to-end", {
  # Create a complex CSV with semicolons and special characters
  csv_content <- c(
    '"study_id";"effect";"se";"n_obs"',
    '"Study A";10.5;2.3;100',
    '"Study B";8.2;1.8;150',
    '"Study C";;0.9;200' # Missing effect
  )
  tmp_file <- create_temp_csv(csv_content)
  on.exit(unlink(tmp_file))

  df <- smart_read_csv(tmp_file)

  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 4)
  expect_true(is.na(df$effect[3]))
  expect_equal(df$study_id[1], "Study A")
})
