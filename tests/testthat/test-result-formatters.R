box::use(
  testthat[
    expect_equal,
    expect_identical,
    test_that
  ]
)

box::use(artma / libs / formatting / results[significance_mark])

test_that("significance_mark handles vector inputs", {
  p_values <- c(0.001, 0.02, 0.08, 0.2, NA, Inf)
  expect_equal(
    significance_mark(p_values),
    c("***", "**", "*", "", "", "")
  )
})

test_that("significance_mark preserves input names", {
  named_values <- c(alpha = 0.01, beta = 0.1, gamma = 1)
  expect_identical(
    significance_mark(named_values),
    c(alpha = "***", beta = "*", gamma = "")
  )
})

test_that("significance_mark returns empty vector for empty input", {
  expect_identical(significance_mark(numeric(0)), character(0))
})

# print_sectioned_table -----------------------------------------------------

box::use(
  testthat[expect_match, expect_true],
  artma / libs / formatting / results[print_sectioned_table, print_paragraph]
)

sectioned_fixture <- function() {
  out <- data.frame(
    Section = c("Estimate", "Estimate", "Diagnostics"),
    Statistic = c("MAIVE estimate", "Std. error", "First-stage F"),
    Value = c("0.123", "(0.045)", "4.512"),
    Note = c("significant at 5%", "", "weak instrument"),
    stringsAsFactors = FALSE
  )
  attr(out, "tone") <- c("good", "", "bad")
  out
}

# The printer returns the exact lines it emitted, so assertions can read them
# without fighting cli's output connection.
capture_sectioned <- function(x, ...) {
  withr::with_options(
    list(cli.num_colors = 1),
    utils::capture.output(lines <- print_sectioned_table(x, ...))
  )
  lines
}

test_that("print_sectioned_table underlines every section heading", {
  lines <- capture_sectioned(sectioned_fixture())

  expect_true("Estimate" %in% lines)
  expect_true(strrep("-", nchar("Estimate")) %in% lines)
  expect_true("Diagnostics" %in% lines)
})

test_that("print_sectioned_table left-aligns labels and right-aligns values", {
  lines <- capture_sectioned(sectioned_fixture())
  rows <- grep("^  \\S", lines, value = TRUE)

  # Labels all start in the same column; print.data.frame would right-align them.
  expect_identical(substring(rows, 1, 3), c("  M", "  S", "  F"))
  # Values share a right edge, so the decimal points line up.
  value_ends <- regexpr("0\\.045\\)|0\\.123|4\\.512", rows) +
    attr(regexpr("0\\.045\\)|0\\.123|4\\.512", rows), "match.length")
  expect_equal(length(unique(value_ends)), 1L)
})

test_that("print_sectioned_table emits no blank separator rows inside a section", {
  lines <- capture_sectioned(sectioned_fixture())
  # One blank line only, between the two sections.
  expect_equal(sum(!nzchar(lines)), 1L)
})

test_that("print_sectioned_table falls back to the plain printer without a Section column", {
  plain <- data.frame(Statistic = "a", Value = "1", stringsAsFactors = FALSE)

  expect_true(any(grepl("Statistic", capture_sectioned(plain))))
})

test_that("print_sectioned_table ignores an empty table", {
  expect_equal(capture_sectioned(sectioned_fixture()[0, ]), character(0))
})

test_that("print_paragraph wraps sentences and drops empty ones", {
  utils::capture.output(
    lines <- print_paragraph(c("One sentence.", "", "Another sentence."), width = 20)
  )

  expect_true(length(lines) > 1)
  expect_match(paste(lines, collapse = " "), "One sentence\\. Another sentence\\.")
})

test_that("format_estimate keeps non-finite estimates as NA", {
  box::use(artma / libs / formatting / results[format_estimate])

  # A plain paste0() would turn the NA into the literal string "NA", which then
  # slips past every downstream is.na() guard and prints as "NA" in tables.
  expect_identical(
    format_estimate(c(0.5, NA_real_, Inf), 2L, c("**", "", "")),
    c("0.50**", NA_character_, NA_character_)
  )
})

test_that("format_estimate recycles a single significance mark", {
  box::use(artma / libs / formatting / results[format_estimate])

  expect_identical(format_estimate(c(1, 2), 1L), c("1.0", "2.0"))
})

test_that("format_estimate returns an empty vector for empty input", {
  box::use(artma / libs / formatting / results[format_estimate])

  expect_identical(format_estimate(numeric(0), 2L), character(0))
})

test_that("format_estimate_with_pvalue renders a non-finite estimate as literal \"NA\"", {
  box::use(artma / libs / formatting / results[format_estimate_with_pvalue])

  # Unlike format_estimate(), marks are pasted on before the finiteness check,
  # so a non-finite estimate becomes the literal string "NA" rather than
  # NA_character_. This mirrors the moved nonlinear.R behaviour and is pinned
  # deliberately, not fixed, by the B7 consolidation.
  expect_identical(
    format_estimate_with_pvalue(NA_real_, NA_real_, 2L, TRUE),
    "NA"
  )
})

test_that("format_estimate_with_pvalue appends marks only when add_marks is TRUE", {
  box::use(artma / libs / formatting / results[format_estimate_with_pvalue])

  expect_identical(format_estimate_with_pvalue(0.5, 0.001, 2L, TRUE), "0.50***")
  expect_identical(format_estimate_with_pvalue(0.5, 0.001, 2L, FALSE), "0.50")
})

test_that("format_standard_error blanks non-finite standard errors", {
  box::use(artma / libs / formatting / results[format_standard_error])

  expect_identical(format_standard_error(c(0.05, NA_real_, Inf), 2L), c("(0.05)", "", ""))
})
