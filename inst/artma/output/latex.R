#' @title LaTeX Table Rendering
#' @description
#' Renders result data frames as booktabs LaTeX tables ready to be included
#' in a paper. Significance asterisks and rounding are already baked into the
#' table cells by the methods themselves, so the rendering here is purely
#' presentational: escaping, column alignment, and the table environment.
NULL

BACKSLASH_SENTINEL <- "artmabackslash"

#' Escape LaTeX special characters
#'
#' @description
#' Escapes the characters LaTeX treats specially. Asterisks are left alone:
#' they carry significance marks and render as-is in text mode. Backslashes are
#' parked on a sentinel first so that the braces introduced by the replacement
#' macros are not escaped a second time.
#'
#' @param x *\[character\]* The values to escape.
#' @return *\[character\]* The escaped values.
#' @keywords internal
escape_latex <- function(x) {
  x <- as.character(x)
  if (!length(x)) {
    return(character(0))
  }

  x <- gsub("\\", BACKSLASH_SENTINEL, x, fixed = TRUE)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  gsub(BACKSLASH_SENTINEL, "\\textbackslash{}", x, fixed = TRUE)
}

#' Format a single column for LaTeX output
#'
#' @param column The column to format.
#' @param digits *\[integer\]* Number of decimals used for numeric columns.
#' @return *\[character\]* The formatted, escaped cells. `NA` becomes an empty cell.
#' @keywords internal
format_latex_column <- function(column, digits) {
  box::use(artma / libs / formatting / results[format_number])

  formatted <- if (is.numeric(column) && !is.integer(column)) {
    format_number(column, digits)
  } else {
    as.character(column)
  }

  formatted <- escape_latex(formatted)
  formatted[is.na(column) | is.na(formatted)] <- ""
  formatted
}

#' Resolve the column alignment string
#'
#' @param df *\[data.frame\]* The table to align.
#' @return *\[character\]* A single string such as `"lrr"`.
#' @keywords internal
resolve_alignment <- function(df) {
  alignments <- vapply(
    df,
    function(column) if (is.numeric(column)) "r" else "l",
    character(1)
  )
  paste(alignments, collapse = "")
}

#' Render a data frame as a booktabs LaTeX table
#'
#' @param df *\[data.frame\]* The table to render.
#' @param caption *\[character, optional\]* Table caption. Omitted when `NULL`.
#' @param label *\[character, optional\]* Table label. Omitted when `NULL`.
#' @param digits *\[integer, optional\]* Number of decimals for numeric columns.
#'   Defaults to the `artma.output.number_of_decimals` option.
#' @return *\[character\]* The LaTeX source, one element per line.
#' @keywords internal
df_to_latex <- function(df,
                        caption = NULL,
                        label = NULL,
                        digits = getOption("artma.output.number_of_decimals", 3)) {
  box::use(artma / libs / core / validation[assert])

  assert(is.data.frame(df), "The table to render as LaTeX must be a data frame.")

  header <- paste(escape_latex(names(df)), collapse = " & ")

  body <- character(0)
  if (nrow(df) > 0 && ncol(df) > 0) {
    cells <- lapply(df, format_latex_column, digits = digits)
    body <- vapply(
      seq_len(nrow(df)),
      function(i) paste0(paste(vapply(cells, `[`, character(1), i), collapse = " & "), " \\\\"),
      character(1)
    )
  }

  c(
    "% Requires \\usepackage{booktabs} in the document preamble.",
    "\\begin{table}[htbp]",
    "\\centering",
    if (!is.null(caption)) paste0("\\caption{", escape_latex(caption), "}"),
    if (!is.null(label)) paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{", resolve_alignment(df), "}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
}

#' Write a data frame to a `.tex` file
#'
#' @param df *\[data.frame\]* The table to render.
#' @param path *\[character\]* Destination file path.
#' @param caption *\[character, optional\]* Table caption.
#' @param label *\[character, optional\]* Table label.
#' @keywords internal
write_latex_table <- function(df, path, caption = NULL, label = NULL) {
  writeLines(df_to_latex(df, caption = caption, label = label), con = path)
  invisible(path)
}

box::export(
  escape_latex,
  df_to_latex,
  write_latex_table
)
