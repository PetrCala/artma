box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_message,
    expect_true,
    skip_if_not_installed,
    test_that
  ]
)

box::use(
  artma / data / read[read_by_type, read_file, read_data],
  artma / data / normalize[normalize_read_df]
)

# A canonical all-text data frame standing in for the raw output of any reader.
# Every input format should normalize to the same typed result:
#   - "N/A", "null", "NA" become NA (CONST$DATA$NA_STRINGS)
#   - whitespace-only becomes NA
#   - numeric text is coerced to numeric, identifier text stays character
raw_text_df <- function() {
  data.frame(
    study_id = c("S1", "S2", "S3"),
    effect = c("1.5", "N/A", "2.5"),
    se = c("0.1", "0.2", "null"),
    note = c("ok", "NA", "  "),
    stringsAsFactors = FALSE
  )
}

expect_canonical <- function(df) {
  expect_equal(df$study_id, c("S1", "S2", "S3"))
  expect_equal(df$effect, c(1.5, NA, 2.5))
  expect_equal(df$se, c(0.1, 0.2, NA))
  expect_equal(df$note, c("ok", NA, NA))
}


test_that("normalize_read_df replaces NA-strings, whitespace, and coerces types", {
  out <- normalize_read_df(raw_text_df())
  expect_canonical(out)
})


# read_file must normalize every supported input format to the same canonical
# typed result. One registered test per format, each carrying its own optional
# package skip so a missing reader skips only its own case.
read_file_formats <- list(
  list(name = "CSV", ext = ".csv", skip = character(0), write = function(df, path) utils::write.csv(df, path, row.names = FALSE)),
  list(name = "RDS", ext = ".rds", skip = character(0), write = function(df, path) saveRDS(df, path)),
  list(name = "Excel", ext = ".xlsx", skip = c("writexl", "readxl"), write = function(df, path) writexl::write_xlsx(df, path)),
  list(name = "JSON", ext = ".json", skip = "jsonlite", write = function(df, path) jsonlite::write_json(df, path)),
  list(name = "Stata", ext = ".dta", skip = "haven", write = function(df, path) haven::write_dta(df, path))
)

for (case in read_file_formats) {
  test_that(sprintf("read_file normalizes a %s file identically to the canonical result", case$name), {
    for (pkg in case$skip) skip_if_not_installed(pkg)
    withr::local_options(list(artma.verbose = 0))
    tmp <- withr::local_tempfile(fileext = case$ext)
    case$write(raw_text_df(), tmp)

    expect_canonical(read_file(tmp))
  })
}


test_that("read_by_type errors clearly when JSON does not flatten to a data frame", {
  skip_if_not_installed("jsonlite")
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(a = list(1, 2), b = "x"), tmp, auto_unbox = TRUE)

  expect_error(read_by_type(tmp, "json"), "did not flatten to a data frame")
})


test_that("read_data and the shared read_file read a file identically", {
  # Guards against the two call sites (read_data and the options-file column
  # preprocessing, which both go through read_file) diverging again.
  withr::local_options(list(artma.verbose = 0))
  tmp <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(raw_text_df(), tmp, row.names = FALSE)

  expect_equal(read_data(tmp), read_file(tmp))
})


# -- Comma decimals (issue #554) -----------------------------------------------
# The CSV reader picks `dec` from the file; every other format reaches
# coerce_df_columns with "0,817" still as text, and type.convert leaves it as
# character. The coercer re-parses a column whose values are all comma-decimal
# numbers (plain integers allowed) and reports which columns it touched.

test_that("normalize_read_df re-parses a column of comma-decimal text as numeric", {
  withr::local_options(list(artma.verbose = 0))
  df <- data.frame(
    study_id = c("S1", "S2", "S3", "S4"),
    effect = c("0,817", "-1,25", NA, "2"),
    se = c("0,083", "0,057", "0,4", "0,1"),
    stringsAsFactors = FALSE
  )

  out <- normalize_read_df(df)

  expect_true(is.numeric(out$effect))
  expect_equal(out$effect, c(0.817, -1.25, NA, 2))
  expect_equal(out$se, c(0.083, 0.057, 0.4, 0.1))
  expect_equal(out$study_id, c("S1", "S2", "S3", "S4"))
})

test_that("normalize_read_df leaves mixed or list-like comma text as character", {
  withr::local_options(list(artma.verbose = 0))
  df <- data.frame(
    mixed = c("0,5", "high", "1"),
    codes = c("1,2,3", "4,5", "6,7,8"),
    ints = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )

  out <- normalize_read_df(df)

  expect_true(is.character(out$mixed))
  expect_true(is.character(out$codes))
  expect_equal(out$ints, c(1L, 2L, 3L))
})

test_that("normalize_read_df reports re-parsed comma-decimal columns at info level", {
  withr::local_options(list(artma.verbose = 3))
  df <- data.frame(effect = c("0,1", "0,2"), se = c("0,3", "0,4"), stringsAsFactors = FALSE)

  expect_message(normalize_read_df(df), "comma decimals")
})

test_that("read_file reads a semicolon CSV with comma decimals into numeric effect and se", {
  withr::local_options(list(artma.verbose = 0))
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "study_name;sample_size;beta_estimate;beta_se",
    "Abebe et al. (2021);1557;0,817;0,083",
    "Abebe et al. (2021);1559;0,794;0,057"
  ), tmp)

  out <- read_file(tmp)

  expect_true(is.numeric(out$beta_estimate))
  expect_true(is.numeric(out$beta_se))
  expect_equal(out$beta_estimate / out$beta_se, c(0.817 / 0.083, 0.794 / 0.057))
  expect_equal(out$sample_size, c(1557L, 1559L))
})


# -- "na" as a real category value (issue #402) --------------------------------
# Lowercase "na" is a legitimate category value in some real datasets (e.g. "no
# functional form assumed"). Only the exact-case conventional spellings ("NA",
# "N/A", "NULL", "null") are treated as missing-value sentinels; "na" and
# "n/a" must survive untouched.

test_that("normalize_read_df keeps lowercase 'na' as a literal category value", {
  df <- data.frame(
    study_id = c("S1", "S2", "S3"),
    discounting = c("na", "exponential", "na"),
    stringsAsFactors = FALSE
  )

  out <- normalize_read_df(df)

  expect_equal(out$discounting, c("na", "exponential", "na"))
  expect_false(anyNA(out$discounting))
})

test_that("normalize_read_df still treats exact-case 'NA' as missing", {
  df <- data.frame(x = c("NA", "value"), stringsAsFactors = FALSE)

  out <- normalize_read_df(df)

  expect_equal(out$x, c(NA_character_, "value"))
})

test_that("read_file keeps lowercase 'na' as a category value read from a CSV", {
  withr::local_options(list(artma.verbose = 0))
  tmp <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      study_id = c("S1", "S2", "S3"),
      effect = c(0.1, 0.2, 0.3),
      se = c(0.01, 0.02, 0.03),
      n_obs = c(10L, 20L, 30L),
      discounting = c("na", "exponential", "na"),
      stringsAsFactors = FALSE
    ),
    tmp,
    row.names = FALSE
  )

  out <- read_file(tmp)

  expect_equal(out$discounting, c("na", "exponential", "na"))
  expect_false(anyNA(out$discounting))
})


# -- UTF-8 BOM (issue #556) ----------------------------------------------------
# Excel's "CSV UTF-8" export prefixes the file with EF BB BF. Read naively, the
# bytes become part of the first header name and make.names() renders it as
# "X...obs_id", so the first column loses its identity.

test_that("read_file reads a BOM-prefixed CSV's first column under its real name", {
  withr::local_options(list(artma.verbose = 0))
  plain <- withr::local_tempfile(fileext = ".csv")
  with_bom <- withr::local_tempfile(fileext = ".csv")
  lines <- c(
    "obs_id;study_name;beta_estimate;beta_se",
    "1;Smith (2020);0.5;0.1",
    "2;Jones (2021);0.7;0.2"
  )
  writeLines(lines, plain)
  con <- file(with_bom, open = "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)
  close(con)

  df <- read_file(with_bom)

  expect_equal(names(df)[[1]], "obs_id")
  expect_equal(df, read_file(plain))
})
