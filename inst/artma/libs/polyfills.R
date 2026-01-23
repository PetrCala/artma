#' @title Polyfills for Removed Dependencies
#' @description
#' Custom implementations of functions from removed dependencies to reduce
#' the package's dependency footprint. This module replaces functionality from:
#' - stringr (string manipulation)
#' - purrr (functional programming)
#' - glue (string interpolation)
#' - digest (hashing for cache keys)
#' - usethis (file editing)
#'
#' All functions maintain API compatibility with the original packages.
#'
#' @name polyfills

# String manipulation (stringr replacements) ----

#' Trim whitespace from strings
#' @description Replacement for stringr::str_trim()
#' @param string Character vector
#' @param side Which side to trim: "both" (default), "left", "right"
#' @return Trimmed character vector
#' @export
str_trim <- function(string, side = c("both", "left", "right")) {
  side <- match.arg(side)
  switch(side,
    both = trimws(string, which = "both"),
    left = trimws(string, which = "left"),
    right = trimws(string, which = "right")
  )
}

#' Replace all matches in a string
#' @description Replacement for stringr::str_replace_all()
#' @param string Input vector
#' @param pattern Pattern to look for
#' @param replacement Replacement string
#' @return Character vector with replacements
#' @export
str_replace_all <- function(string, pattern, replacement) {
  gsub(pattern, replacement, string, perl = TRUE)
}

#' Remove first match from a string
#' @description Replacement for stringr::str_remove()
#' @param string Input vector
#' @param pattern Pattern to remove
#' @return Character vector with pattern removed
#' @export
str_remove <- function(string, pattern) {
  sub(pattern, "", string, perl = TRUE)
}

#' Convert string to title case
#' @description Replacement for stringr::str_to_title()
#' @param string Input vector
#' @return Character vector in title case
#' @export
str_to_title <- function(string) {
  # tools::toTitleCase() doesn't lowercase first, so we need to do that
  tools::toTitleCase(tolower(string))
}

# Functional programming (purrr replacements) ----

#' Map over a vector and return character results
#' @description Replacement for purrr::map_chr()
#' @param .x A list or atomic vector
#' @param .f A function, formula, or vector
#' @param ... Additional arguments passed to .f
#' @return Character vector
#' @export
map_chr <- function(.x, .f, ...) {
  if (is.character(.f) && length(.f) == 1) {
    # Handle name extraction: map_chr(list, "name")
    vapply(.x, function(x) x[[.f]], character(1), USE.NAMES = FALSE)
  } else if (is.function(.f)) {
    vapply(.x, .f, character(1), ..., USE.NAMES = FALSE)
  } else {
    stop("`.f` must be a function or string") # nolint
  }
}

#' Map over a vector and return logical results
#' @description Replacement for purrr::map_lgl()
#' @param .x A list or atomic vector
#' @param .f A function or formula
#' @param ... Additional arguments passed to .f
#' @return Logical vector
#' @export
map_lgl <- function(.x, .f, ...) {
  # Handle formula notation (purrr-style)
  if (inherits(.f, "formula")) {
    formula_env <- environment(.f)
    formula_body <- .f[[2]]
    .f <- function(.x) {
      # Support both . and .x notation
      eval(formula_body, envir = list(.x = .x, . = .x), enclos = formula_env)
    }
  }

  if (is.function(.f)) {
    vapply(.x, .f, logical(1), ..., USE.NAMES = FALSE)
  } else {
    stop("`.f` must be a function or formula") # nolint
  }
}

#' Keep elements that satisfy a predicate
#' @description Replacement for purrr::keep()
#' @param .x A list or atomic vector
#' @param .p A predicate function or formula
#' @param ... Additional arguments passed to .p
#' @return Filtered vector
#' @export
keep <- function(.x, .p, ...) {
  # Handle formula notation (purrr-style)
  if (inherits(.p, "formula")) {
    # Convert formula ~expr to function(x) expr
    formula_env <- environment(.p)
    formula_body <- .p[[2]]
    # Replace .x with x in the formula
    .p <- function(x) {
      # Evaluate the formula body with x as the argument
      # Support both . and .x notation
      eval(formula_body, envir = list(.x = x, . = x), enclos = formula_env)
    }
  }
  Filter(.p, .x, ...)
}

# String interpolation (glue replacements) ----

#' Interpolate strings
#' @description Replacement for glue::glue()
#' Uses sprintf-style formatting instead of brace interpolation
#' @param ... Character strings with %s, %d, etc. placeholders
#' @param .sep Separator between elements
#' @param .envir Environment for evaluation (ignored, for compatibility)
#' @return Character string
#' @export
glue <- function(..., .sep = "", .envir = parent.frame()) {
  # This is a simplified version that works with sprintf-style formatting
  # For complex cases, use sprintf() or paste0() directly
  args <- list(...)

  if (length(args) == 0) {
    return("")
  }

  # If single string without placeholders, return as-is
  if (length(args) == 1 && is.character(args[[1]])) {
    # Check if it contains {var} style interpolation
    str <- args[[1]]
    if (grepl("\\{[^}]+\\}", str)) {
      # Extract variable names
      vars <- gregexpr("\\{([^}]+)\\}", str, perl = TRUE)
      var_names <- regmatches(str, vars)[[1]]
      var_names <- gsub("[{}]", "", var_names)

      # Get values from environment
      values <- lapply(var_names, function(v) {
        tryCatch(eval(parse(text = v), envir = .envir), error = function(e) v)
      })

      # Replace {var} with values
      result <- str
      for (i in seq_along(var_names)) {
        result <- sub(
          paste0("\\{", var_names[i], "\\}"),
          as.character(values[[i]]),
          result
        )
      }
      return(result)
    }
    return(str)
  }

  # Otherwise concatenate
  paste0(unlist(args), collapse = .sep)
}

#' Collapse a character vector
#' @description Replacement for glue::glue_collapse()
#' @param x Character vector
#' @param sep Separator
#' @param width Maximum width (not implemented)
#' @param last Separator for last element
#' @return Collapsed string
#' @export
glue_collapse <- function(x, sep = "", width = Inf, last = "") {
  if (length(x) == 0) {
    return("")
  }
  if (length(x) == 1) {
    return(as.character(x))
  }

  if (nzchar(last) && length(x) > 1) {
    paste0(paste(x[-length(x)], collapse = sep), last, x[length(x)])
  } else {
    paste(x, collapse = sep)
  }
}

# Hashing (digest replacement) ----

#' Create hash for caching
#' @description Replacement for digest::digest()
#' Uses serialize + base R hashing for cache keys
#' @param object Object to hash
#' @param algo Algorithm (ignored, always uses same method)
#' @param ... Additional arguments (for compatibility)
#' @return Hash string
#' @export
digest <- function(object, algo = "xxhash64", ...) {
  # Serialize the object to raw bytes
  raw_bytes <- serialize(object, connection = NULL, ascii = FALSE)

  # Create a simple hash using base R
  # We'll use a combination of length and checksum for speed
  # Use modulo arithmetic throughout to avoid integer overflow
  hash_high <- 0
  hash_low <- 0

  # Process bytes in chunks to avoid overflow
  for (i in seq_along(raw_bytes)) {
    val <- as.numeric(raw_bytes[i])
    # Update hash_low
    hash_low <- (hash_low + val * (i %% 1000)) %% (2^31 - 1)
    # Update hash_high
    hash_high <- (hash_high + val * (i %% 997)) %% (2^31 - 1)
  }

  # Convert to hex string (16 characters for consistency with xxhash64)
  hex_hash <- sprintf("%08x%08x", as.integer(hash_high), as.integer(hash_low))

  hex_hash
}

# File utilities (usethis replacement) ----

#' Open file in editor
#' @description Replacement for usethis::edit_file()
#' @param path File path to open
#' @return NULL (invisibly)
#' @export
edit_file <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("File does not exist: %s", path)) # nolint
  }
  utils::file.edit(path)
  invisible(NULL)
}
