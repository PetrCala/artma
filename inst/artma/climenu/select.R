#' Single Selection Menu
#'
#' Interactive menu for selecting a single item from a list.
#' Uses arrow keys (or j/k) to navigate and Enter to select.
#'
#' @param choices *\[character\]* Vector of choices to display
#' @param prompt *\[character, optional\]* Prompt message to display
#' @param selected *\[integer|character, optional\]* Pre-selected item (index or value)
#' @param return_index *\[logical, optional\]* Return index instead of value
#'
#' @return Selected item as character or index
#' @export
select <- function(choices,
                   prompt = "Select an item:",
                   selected = NULL,
                   return_index = FALSE) {
  box::use(
    . / utils[
      validate_choices,
      normalize_selected,
      render_menu,
      get_keypress,
      move_cursor_up,
      clear_lines
    ]
  )

  # Validate inputs
  validate_choices(choices)

  # Determine initial cursor position
  cursor_pos <- normalize_selected(selected, choices, multiple = FALSE)
  if (is.null(cursor_pos)) cursor_pos <- 1L

  n_choices <- length(choices)

  # Check if running in interactive mode
  if (!interactive()) {
    cli::cli_warn("Not running in interactive mode. Returning first choice.")
    return(if (return_index) 1L else choices[1])
  }

  # Display prompt
  cat("\n")
  cli::cli_text(prompt)
  cat("\n")

  # Main interaction loop
  repeat {
    # Render menu
    menu_lines <- render_menu(
      choices = choices,
      cursor_pos = cursor_pos,
      selected_indices = NULL,
      type = "select"
    )

    n_lines <- length(menu_lines)

    # Get user input
    key <- get_keypress()

    # Clear previous menu
    clear_lines(n_lines)

    # Handle key press
    if (key %in% c("up", "k")) {
      cursor_pos <- if (cursor_pos > 1) cursor_pos - 1L else n_choices
    } else if (key %in% c("down", "j")) {
      cursor_pos <- if (cursor_pos < n_choices) cursor_pos + 1L else 1L
    } else if (key == "enter") {
      break
    } else if (key == "esc") {
      cat("\n")
      cli::cli_alert_info("Selection cancelled")
      return(NULL)
    }
  }

  cat("\n")
  cli::cli_alert_success("Selected: {.val {choices[cursor_pos]}}")

  if (return_index) {
    return(cursor_pos)
  } else {
    return(choices[cursor_pos])
  }
}
