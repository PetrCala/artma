box::use(
  testthat[
    expect_equal,
    expect_error,
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
