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
  make_verbose_name,
  pluralize,
  trim_quotes
)
