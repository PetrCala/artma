box::use(artma / libs / validation[validate, assert])

#' Pluralize a word
#'
#' @param word [character] The word to pluralize
#' @return [character] The pluralized word
#' @export
pluralize <- function(word) {
  validate(is.character(word))
  if (grepl("[sxz]$", word) || grepl("[sc]h$", word)) {
    return(paste0(word, "es"))
  } else if (grepl("[^aeiou]y$", word)) {
    return(sub("y$", "ies", word))
  } else {
    return(paste0(word, "s"))
  }
}

#' Find a string in a vector of strings using a substring
#'
#' @param vector_of_strings [character] The vector of strings to search
#' @param substring [character] The substring to search for
#' @return [character] The string that contains the substring
#' @export
find_string_using_substring <- function(vector_of_strings, substring) {
  assert(is.character(substring), "The substring must be a character")
  assert(is.vector(vector_of_strings), "The vector of strings must be a character vector")
  match_bool <- grepl(substring, vector_of_strings)
  if (sum(match_bool) == 0) {
    rlang::abort("Could not find the substring", substring, "in the vector of strings.")
  }
  if (sum(match_bool) > 1) {
    rlang::abort("Found multiple matches for the substring", substring, "in the vector of strings.")
  }
  return(vector_of_strings[match_bool])
}

#' @title Trim quotes
#' @description Removes single or double quotes from the beggining and end of a string. Preserves these quotes elsewhere in the string.
#' @param s [character] The string to trim quotes for.
#' @export
trim_quotes <- function(s) gsub("^(\"|')+|(\"|')+$", "", s)


#' Clean a string by removing special characters and converting to lowercase
#'
#' @param input_string [character] The string to clean
#' @return [character] The cleaned string
#' @export
clean_string <- function(input_string) {
  # Remove special characters
  str_out <- stringr::str_replace_all(input_string, "[^a-zA-Z0-9]", "_")

  # Convert to lowercase
  str_out <- tolower(str_out)

  # Remove leading or trailing underscores
  str_out <- stringr::str_trim(str_out, side = "both")
  str_out <- stringr::str_replace_all(str_out, "^_+|_+$", "")

  # Remove quotes
  str_out <- trim_quotes(str_out)

  return(str_out)
}
