box::use(artma / libs / validation[validate, assert])

#' Pluralize a word based on count
#'
#' @param word *\[character\]* The word to potentially pluralize
#' @param count *\[integer\]* The count to determine if pluralization is needed
#' `character` The word, pluralized if count is not 1
pluralize <- function(word, count = NULL) {
  validate(is.character(word))

  if (is.null(count)) {
    if (grepl("[sxz]$", word) || grepl("[sc]h$", word))
      return(paste0(word, "es"))
    if (grepl("[^aeiou]y$", word))
      return(sub("y$", "ies", word))
    return(paste0(word, "s"))
  }

  validate(is.numeric(count))

  if (count == 1)
    return(word)

  if (grepl("[sxz]$", word) || grepl("[sc]h$", word))
    return(paste0(word, "es"))
  if (grepl("[^aeiou]y$", word))
    return(sub("y$", "ies", word))
  return(paste0(word, "s"))
}
#' Find a string in a vector of strings using a substring
#'
#' @param vector_of_strings *\[character\]* The vector of strings to search
#' @param substring *\[character\]* The substring to search for
#' `character` The string that contains the substring
find_string_using_substring <- function(vector_of_strings, substring) {
  assert(is.character(substring), "The substring must be a character")
  assert(is.vector(vector_of_strings), "The vector of strings must be a character vector")
  match_bool <- grepl(substring, vector_of_strings)
  if (sum(match_bool) == 0) {
    cli::cli_abort("Could not find the substring", substring, "in the vector of strings.")
  }
  if (sum(match_bool) > 1) {
    cli::cli_abort("Found multiple matches for the substring", substring, "in the vector of strings.")
  }
  vector_of_strings[match_bool]
}

#' @title Trim quotes
#' @description Removes single or double quotes from the beggining and end of a string. Preserves these quotes elsewhere in the string.
#' @param s *\[character\]* The string to trim quotes for.
trim_quotes <- function(s) gsub("^(\"|')+|(\"|')+$", "", s)


#' Clean a string by removing special characters and converting to lowercase
#'
#' @param input_string *\[character\]* The string to clean
#' `character` The cleaned string
clean_string <- function(input_string) {
  box::use(artma / libs / polyfills[str_replace_all, str_trim])

  # Remove special characters
  str_out <- str_replace_all(input_string, "[^a-zA-Z0-9]", "_")

  # Convert to lowercase
  str_out <- tolower(str_out)

  # Remove leading or trailing underscores
  str_out <- str_trim(str_out, side = "both")
  str_out <- str_replace_all(str_out, "^_+|_+$", "")

  # Remove quotes
  str_out <- trim_quotes(str_out)

  str_out
}

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
  clean_string,
  find_string_using_substring,
  make_verbose_name,
  pluralize,
  trim_quotes
)
