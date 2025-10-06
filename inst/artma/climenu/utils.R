#' CLI Menu Utilities
#'
#' Internal utility functions for the climenu package.
#' @keywords internal

#' Validate choices parameter
#' @export
validate_choices <- function(choices) {
  if (!is.character(choices)) {
    cli::cli_abort("choices must be a character vector")
  }
  if (length(choices) == 0) {
    cli::cli_abort("choices must have at least one element")
  }
  if (any(is.na(choices))) {
    cli::cli_abort("choices must not contain NA values")
  }
}

#' Normalize selected parameter to indices
#' @export
normalize_selected <- function(selected, choices, multiple = FALSE) {
  if (is.null(selected)) {
    return(NULL)
  }

  if (is.numeric(selected)) {
    indices <- as.integer(selected)
    if (any(indices < 1 | indices > length(choices))) {
      cli::cli_warn("Some selected indices are out of range. Ignoring.")
      indices <- indices[indices >= 1 & indices <= length(choices)]
    }
  } else if (is.character(selected)) {
    indices <- which(choices %in% selected)
    if (length(indices) == 0) {
      cli::cli_warn("None of the selected values found in choices. Ignoring.")
      return(NULL)
    }
  } else {
    cli::cli_abort("selected must be numeric (indices) or character (values)")
  }

  if (!multiple && length(indices) > 1) {
    cli::cli_warn("Multiple items selected for single-select menu. Using first.")
    indices <- indices[1]
  }

  return(indices)
}

#' Render menu display
#' @export
render_menu <- function(choices, cursor_pos, selected_indices, type = c("select", "checkbox")) {
  type <- match.arg(type)

  lines <- character(length(choices))

  for (i in seq_along(choices)) {
    is_cursor <- i == cursor_pos
    is_selected <- i %in% selected_indices

    if (type == "checkbox") {
      checkbox_mark <- if (is_selected) "\u2611" else "\u2610" # ☑ or ☐
      cursor_mark <- if (is_cursor) "\u276f" else " " # ❯
      lines[i] <- sprintf("%s %s %s", cursor_mark, checkbox_mark, choices[i])
    } else {
      cursor_mark <- if (is_cursor) "\u276f" else " " # ❯
      lines[i] <- sprintf("%s %s", cursor_mark, choices[i])
    }

    # Apply styling
    if (is_cursor) {
      lines[i] <- cli::col_cyan(lines[i])
    }
  }

  # Print lines
  for (line in lines) {
    cat(line, "\n", sep = "")
  }

  return(lines)
}

#' Get single keypress from user
#' @export
get_keypress <- function() {
  # Try to use getPass package if available for better key detection
  if (requireNamespace("getPass", quietly = TRUE)) {
    key <- tryCatch(
      {
        char <- rawToChar(as.raw(getPass::getPass(msg = "", noblank = TRUE, forcemask = FALSE)))

        # Handle arrow keys and special keys
        if (char == "\033") { # ESC sequence
          # Read next two characters for arrow keys
          next_chars <- rawToChar(as.raw(c(
            getPass::getPass(msg = "", noblank = TRUE, forcemask = FALSE),
            getPass::getPass(msg = "", noblank = TRUE, forcemask = FALSE)
          )))

          if (next_chars == "[A") {
            return("up")
          }
          if (next_chars == "[B") {
            return("down")
          }
          if (next_chars == "[C") {
            return("right")
          }
          if (next_chars == "[D") {
            return("left")
          }
          return("esc")
        }

        char
      },
      error = function(e) readline(prompt = "")
    )
  } else {
    # Fallback to readline
    key <- readline(prompt = "")
  }

  # Map keys
  if (key == "") {
    return("enter")
  }
  if (key == " ") {
    return("space")
  }
  if (key == "k") {
    return("up")
  }
  if (key == "j") {
    return("down")
  }
  if (tolower(key) == "q") {
    return("esc")
  }

  return(key)
}

#' Move cursor up n lines
#' @export
move_cursor_up <- function(n) {
  if (n > 0) {
    cat(sprintf("\033[%dA", n))
  }
}

#' Clear n lines
#' @export
clear_lines <- function(n) {
  if (n > 0) {
    for (i in seq_len(n)) {
      move_cursor_up(1)
      cat("\033[2K") # Clear entire line
    }
  }
}
