box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_no_error,
    expect_true,
    test_that
  ]
)

box::use(
  artma / data / normalize[
    ensure_utf8_columns,
    normalize_read_df,
    repair_utf8,
    replace_stata_missing
  ],
  artma / data / read[read_file]
)

# A raw 0xFC byte: "ü" in Windows-1252 and Latin-1, invalid on its own in UTF-8.
# Exactly what an undeclared Excel or Stata export puts on disk.
LATIN1_MULLER <- "M\xfcller et al. (2011)"

# Writes lines byte-for-byte, bypassing the connection layer's re-encoding, so
# the file on disk really is single-byte encoded.
write_bytes <- function(path, lines) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)
  path
}


test_that("repair_utf8 converts single-byte encoded text to valid UTF-8", {
  expect_false(validUTF8(LATIN1_MULLER))

  repaired <- repair_utf8(LATIN1_MULLER)

  expect_true(validUTF8(repaired))
  expect_equal(repaired, "Müller et al. (2011)")
})

test_that("repair_utf8 leaves valid UTF-8 and NA untouched", {
  x <- c("Müller", "plain ascii", NA_character_, "中文")

  expect_identical(repair_utf8(x), x)
})

test_that("repair_utf8 repairs only the invalid elements of a mixed vector", {
  x <- c("already fine", LATIN1_MULLER, NA_character_)

  repaired <- repair_utf8(x)

  expect_identical(repaired[[1]], "already fine")
  expect_equal(repaired[[2]], "Müller et al. (2011)")
  expect_true(is.na(repaired[[3]]))
  expect_true(all(validUTF8(repaired[!is.na(repaired)])))
})

test_that("repair_utf8 passes through non-character input", {
  expect_identical(repair_utf8(1:3), 1:3)
  expect_identical(repair_utf8(character(0)), character(0))
})

test_that("ensure_utf8_columns repairs column names, character columns, and factor levels", {
  df <- data.frame(
    a = c(LATIN1_MULLER, "Smith (2015)"),
    b = factor(c(LATIN1_MULLER, "Smith (2015)")),
    n = c(1, 2),
    stringsAsFactors = FALSE
  )
  names(df) <- c(LATIN1_MULLER, "b", "n")

  withr::local_options(list(artma.verbose = 0))
  out <- ensure_utf8_columns(df)

  expect_true(all(validUTF8(names(out))))
  expect_true(all(validUTF8(out[[1]])))
  expect_true(all(validUTF8(levels(out$b))))
  expect_equal(names(out)[[1]], "Müller et al. (2011)")
  expect_equal(out[[1]][[1]], "Müller et al. (2011)")
  expect_equal(out$n, c(1, 2)) # numeric columns untouched
})

test_that("normalize_read_df repairs encoding before regex-based normalization", {
  # normalize_whitespace_to_na() matches "^\\s*$" against every character
  # column. Without the repair step that call warns (and, in the detection
  # code, errors outright) on invalid UTF-8.
  df <- data.frame(
    study = c(LATIN1_MULLER, "  ", "Smith (2015)"),
    effect = c("0.5", "0.7", "0.9"),
    stringsAsFactors = FALSE
  )

  withr::local_options(list(artma.verbose = 0))
  out <- expect_no_error(normalize_read_df(df))

  expect_equal(out$study, c("Müller et al. (2011)", NA, "Smith (2015)"))
  expect_equal(out$effect, c(0.5, 0.7, 0.9))
})

test_that("replace_stata_missing converts bare and extended missing markers", {
  df <- data.frame(
    male = c("0.75", ".", "0.5", ".a", " . ", ".z"),
    keep = c("a", "b", "c", "d", "e", "f"),
    stringsAsFactors = FALSE
  )

  out <- replace_stata_missing(df)

  expect_equal(out$male, c("0.75", NA, "0.5", NA, NA, NA))
  expect_equal(out$keep, df$keep)
})

test_that("replace_stata_missing leaves genuine values that merely contain a dot", {
  df <- data.frame(
    x = c("0.5", "..", ".ab", "a.", "N.A", "1.", "-.3"),
    stringsAsFactors = FALSE
  )

  expect_equal(replace_stata_missing(df)$x, df$x)
})

test_that("replace_stata_missing handles factors and passes numeric columns through", {
  df <- data.frame(
    f = factor(c("1.5", ".", "2.5")),
    n = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  out <- replace_stata_missing(df)

  expect_equal(out$f, c("1.5", NA, "2.5"))
  expect_equal(out$n, c(1, 2, 3))
})

test_that("normalize_read_df types a Stata-exported column as numeric with NAs", {
  # The regression: a column that is numeric apart from its "." missing values
  # used to stay character, so it was treated as a categorical moderator
  # rather than the continuous variable it is.
  df <- data.frame(
    male = c("0.75", ".", "0.5", ".", "1"),
    effect = c("0.1", "0.2", "0.3", "0.4", "0.5"),
    stringsAsFactors = FALSE
  )

  withr::local_options(list(artma.verbose = 0))
  out <- normalize_read_df(df)

  expect_true(is.numeric(out$male))
  expect_equal(out$male, c(0.75, NA, 0.5, NA, 1))
  expect_equal(sum(is.na(out$male)), 2)
})

test_that("read_file reads a Latin-1 encoded CSV without an encoding error", {
  withr::local_options(list(artma.verbose = 0))
  tmp <- withr::local_tempfile(fileext = ".csv")
  write_bytes(tmp, c(
    "study_id,effect,standard_error",
    paste0(LATIN1_MULLER, ",0.5,0.1"),
    "Smith (2015),0.7,0.2"
  ))

  df <- expect_no_error(read_file(tmp))

  expect_equal(nrow(df), 2)
  expect_true(all(validUTF8(df$study_id)))
  expect_equal(df$study_id[[1]], "Müller et al. (2011)")
})

test_that("column recognition survives a Latin-1 encoded study label column", {
  # The regression this whole path exists for: is_likely_study_key() runs
  # character-class regexes over the candidate study column, which is where a
  # citation-style label like "Müller et al. (2011)" lives. On invalid UTF-8
  # base R raises a hard error there rather than a warning.
  box::use(artma / data / column_recognition[recognize_columns])

  withr::local_options(list(artma.verbose = 0))
  tmp <- withr::local_tempfile(fileext = ".csv")
  write_bytes(tmp, c(
    "study_id,effect,standard_error,sample_size",
    paste0(LATIN1_MULLER, ",0.5,0.1,100"),
    "Smith (2015),0.7,0.2,150",
    "Jones (2019),0.3,0.05,220"
  ))

  df <- read_file(tmp)
  mapping <- expect_no_error(recognize_columns(df))

  expect_true("se" %in% names(mapping))
  expect_equal(mapping[["se"]], "standard_error")
})
