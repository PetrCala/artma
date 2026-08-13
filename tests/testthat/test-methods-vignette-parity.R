box::use(
  testthat[expect_equal, expect_setequal, skip_if_not, test_that]
)

# The methods-overview vignette documents each runtime method in a markdown
# table. This file keeps those tables in step with what the methods actually
# register: the method set, the required columns, the dependencies, the
# suggested packages, and the opt-in flag. Prose is not checked, only the
# facts that also drive the run.
#
# A table that omits a column asserts the empty case: a table without a
# "Depends on" column documents methods with no dependencies, and one without
# a "Packages" column documents methods needing no optional packages.
vignette_path <- function() {
  testthat::test_path("..", "..", "vignettes", "methods-overview.Rmd")
}

split_table_row <- function(line) {
  cells <- strsplit(gsub("^\\||\\|$", "", line), "|", fixed = TRUE)[[1L]]
  trimws(cells)
}

is_separator_row <- function(cells) {
  all(grepl("^:?-+:?$", cells))
}

# Every markdown table in the file, as a list of rows, each row a character
# vector named by the table's headers.
parse_markdown_tables <- function(path) {
  rows <- list()
  headers <- NULL

  for (line in readLines(path, warn = FALSE)) {
    line <- trimws(line)
    if (!startsWith(line, "|")) {
      headers <- NULL
      next
    }

    cells <- split_table_row(line)
    if (is.null(headers)) {
      headers <- cells
      next
    }
    if (is_separator_row(cells)) {
      next
    }

    names(cells) <- headers
    rows[[length(rows) + 1L]] <- cells
  }

  rows
}

# A header the table does not carry reads as the empty case.
table_cell <- function(row, header) {
  if (header %in% names(row)) row[[header]] else NA_character_
}

# `` `effect`, `se` `` -> c("effect", "se"); "none" and "-" mean nothing.
parse_cell_list <- function(cell) {
  if (is.null(cell) || is.na(cell)) {
    return(character())
  }
  values <- trimws(strsplit(gsub("`", "", cell), ",", fixed = TRUE)[[1L]])
  values <- values[nzchar(values)]
  values[!values %in% c("none", "-")]
}

parse_cell_flag <- function(cell) {
  if (is.null(cell) || is.na(cell)) {
    return(FALSE)
  }
  tolower(trimws(cell)) %in% c("yes", "true")
}

documented_methods <- function(path) {
  rows <- Filter(function(row) "Method" %in% names(row), parse_markdown_tables(path))
  names(rows) <- vapply(rows, function(row) gsub("`", "", row[["Method"]]), character(1))
  rows
}

registered_methods <- function() {
  box::use(
    artma / modules / runtime_methods[get_method_metadata, get_runtime_method_modules]
  )

  modules <- get_runtime_method_modules()
  stats::setNames(
    lapply(names(modules), function(name) get_method_metadata(modules[[name]][["run"]], name = name)),
    names(modules)
  )
}

test_that("the methods-overview vignette documents exactly the registered methods", {
  path <- vignette_path()
  skip_if_not(file.exists(path), "vignette sources are not available")

  documented <- documented_methods(path)
  expect_setequal(names(documented), names(registered_methods()))
  expect_equal(anyDuplicated(names(documented)), 0L)
})

test_that("the methods-overview vignette matches the registered method metadata", {
  path <- vignette_path()
  skip_if_not(file.exists(path), "vignette sources are not available")

  documented <- documented_methods(path)
  registered <- registered_methods()

  for (name in names(registered)) {
    meta <- registered[[name]]
    row <- documented[[name]]
    if (is.null(row)) {
      next # The set is asserted by the test above.
    }

    expect_equal(
      parse_cell_list(table_cell(row, "Required columns")),
      as.character(meta$required_columns),
      info = sprintf("required columns documented for '%s'", name)
    )
    expect_equal(
      parse_cell_list(table_cell(row, "Depends on")),
      as.character(meta$depends_on),
      info = sprintf("dependencies documented for '%s'", name)
    )
    expect_equal(
      parse_cell_list(table_cell(row, "Packages")),
      as.character(meta$suggests),
      info = sprintf("suggested packages documented for '%s'", name)
    )
    expect_equal(
      parse_cell_flag(table_cell(row, "Opt-in")),
      isTRUE(meta$opt_in),
      info = sprintf("opt-in documented for '%s'", name)
    )
  }
})
