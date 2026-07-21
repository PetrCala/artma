box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_true,
    test_that
  ],
  withr[local_options],
  artma / output / latex[df_to_latex, escape_latex, write_latex_table]
)

# escape_latex --------------------------------------------------------------

test_that("escape_latex escapes the LaTeX special characters", {
  expect_equal(escape_latex("p_value"), "p\\_value")
  expect_equal(escape_latex("50%"), "50\\%")
  expect_equal(escape_latex("a & b"), "a \\& b")
  expect_equal(escape_latex("$5 #1"), "\\$5 \\#1")
  expect_equal(escape_latex("{x}"), "\\{x\\}")
})

test_that("escape_latex leaves significance asterisks untouched", {
  expect_equal(escape_latex("0.123***"), "0.123***")
})

test_that("escape_latex replaces backslashes without double-escaping the macro", {
  expect_equal(escape_latex("a\\b"), "a\\textbackslash{}b")
  expect_equal(escape_latex("a~b"), "a\\textasciitilde{}b")
  expect_equal(escape_latex("a^b"), "a\\textasciicircum{}b")
})

test_that("escape_latex handles empty input", {
  expect_equal(escape_latex(character(0)), character(0))
})

# df_to_latex ---------------------------------------------------------------

test_that("df_to_latex emits a booktabs table environment", {
  lines <- df_to_latex(data.frame(term = c("a", "b"), estimate = c(1, 2)))

  expect_true("\\begin{table}[htbp]" %in% lines)
  expect_true("\\toprule" %in% lines)
  expect_true("\\midrule" %in% lines)
  expect_true("\\bottomrule" %in% lines)
  expect_true("\\end{tabular}" %in% lines)
  expect_true("\\end{table}" %in% lines)
})

test_that("df_to_latex right-aligns numeric columns and left-aligns the rest", {
  df <- data.frame(term = c("a", "b"), estimate = c(1, 2), flag = c(TRUE, FALSE))
  lines <- df_to_latex(df)

  expect_true("\\begin{tabular}{lrl}" %in% lines)
})

test_that("df_to_latex writes one body row per data row", {
  local_options(artma.output.number_of_decimals = 2)
  df <- data.frame(term = c("a", "b"), estimate = c(1.234, 2.345))
  lines <- df_to_latex(df)

  expect_true("term & estimate \\\\" %in% lines)
  expect_true("a & 1.23 \\\\" %in% lines)
  expect_true("b & 2.35 \\\\" %in% lines)
})

test_that("df_to_latex renders NA as an empty cell", {
  df <- data.frame(term = c("a", NA), estimate = c(NA_real_, 2))
  lines <- df_to_latex(df, digits = 1)

  expect_true("a &  \\\\" %in% lines)
  expect_true(" & 2.0 \\\\" %in% lines)
})

test_that("df_to_latex includes the caption and label only when supplied", {
  df <- data.frame(a = 1)

  with_meta <- df_to_latex(df, caption = "My table", label = "tab:my_table")
  expect_true("\\caption{My table}" %in% with_meta)
  expect_true("\\label{tab:my_table}" %in% with_meta)

  without_meta <- df_to_latex(df)
  expect_false(any(grepl("\\caption", without_meta, fixed = TRUE)))
  expect_false(any(grepl("\\label", without_meta, fixed = TRUE)))
})

test_that("df_to_latex escapes headers and cells", {
  df <- data.frame(p_value = "a & b", stringsAsFactors = FALSE)
  lines <- df_to_latex(df)

  expect_true("p\\_value \\\\" %in% lines)
  expect_true("a \\& b \\\\" %in% lines)
})

test_that("df_to_latex handles a table with no rows", {
  lines <- df_to_latex(data.frame(a = character(0), b = numeric(0)))

  expect_true("a & b \\\\" %in% lines)
  expect_equal(sum(grepl("\\midrule", lines, fixed = TRUE)), 1L)
})

# write_latex_table ---------------------------------------------------------

test_that("write_latex_table writes the rendered source to disk", {
  path <- tempfile(fileext = ".tex")
  write_latex_table(data.frame(a = 1), path = path, caption = "Cap")

  expect_true(file.exists(path))
  contents <- readLines(path)
  expect_true("\\caption{Cap}" %in% contents)
  unlink(path)
})
