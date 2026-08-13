box::use(artma / libs / core / validation[validate, assert])

#' Pluralize a word based on count
#'
#' @param word *\[character\]* The word to potentially pluralize
#' @param count *\[integer\]* The count to determine if pluralization is needed
#' `character` The word, pluralized if count is not 1
pluralize <- function(word, count = NULL) {
  validate(is.character(word))

  apply_plural <- function(word) {
    if (grepl("[sxz]$", word) || grepl("[sc]h$", word)) {
      return(paste0(word, "es"))
    }
    if (grepl("[^aeiou]y$", word)) {
      return(sub("y$", "ies", word))
    }
    paste0(word, "s")
  }

  if (is.null(count)) {
    return(apply_plural(word))
  }

  validate(is.numeric(count))

  if (count == 1) {
    return(word)
  }

  apply_plural(word)
}
#' @title Describe counts, leaving the zeros out
#' @description Turn named counts into readable phrases: `c(table = 2, plot = 0)`
#'   becomes `"2 tables"`. A count of zero is dropped rather than reported, so a
#'   message never tells the reader about something that did not happen. The
#'   result is a character vector, which `cli` collapses into "a, b and c" when
#'   interpolated; an all-zero input yields an empty vector, which the caller
#'   uses to pick a "nothing happened" wording instead.
#' @param counts *\[numeric\]* Counts, named by the singular noun each belongs to.
#' @return *\[character\]* One phrase per non-zero count, in the given order.
count_phrases <- function(counts) {
  counts <- unlist(counts)
  if (length(counts) == 0) {
    return(character())
  }

  validate(is.numeric(counts))
  assert(!is.null(names(counts)), "`counts` must be named by the noun each count describes.")

  counts <- counts[!is.na(counts) & counts > 0]
  vapply(
    seq_along(counts),
    function(i) paste(counts[[i]], pluralize(names(counts)[[i]], counts[[i]])),
    character(1)
  )
}

#' @title Trim quotes
#' @description Removes single or double quotes from the beggining and end of a string. Preserves these quotes elsewhere in the string.
#' @param s *\[character\]* The string to trim quotes for.
trim_quotes <- function(s) gsub("^(\"|')+|(\"|')+$", "", s)


#' Make a verbose name
#'
#' @param input_str *\[character\]* The string to make verbose
#' `character` The verbose string
make_verbose_name <- function(input_str) {
  verbose <- gsub("_", " ", input_str)
  verbose <- trimws(verbose)
  verbose <- paste(toupper(substring(verbose, 1, 1)),
    substring(verbose, 2),
    sep = ""
  )
  verbose
}

box::export(
  count_phrases,
  make_verbose_name,
  pluralize,
  trim_quotes
)
