#' @title Shared menu composition for the interactive flows
#' @description
#' Turns lists of menu items into the `choices` + `descriptions` pair the
#' `ask_select()`/`ask_checkbox()` backends consume. The two-column rendering
#' itself (name padded to a fixed column, dim description truncated to the
#' console) lives in `climenu`; this module only supplies the plain text. Kept
#' out of `interactive/hub.R` so the hub and its submenus
#' (`interactive/options_file_menu.R`) can compose the same way without
#' importing one another.
NULL

box::use(
  artma / libs / core / validation[validate]
)

#' @title Compose value-keyed menu choices
#' @description
#' Build the input `ask_select()` consumes: a named character vector (names
#' are the plain item labels, values are stable action keys, so every menu
#' loop dispatches on values and never on labels) plus the parallel
#' description column. Labels and descriptions stay unstyled; `climenu` owns
#' the alignment, truncation, and dim styling.
#' @param items *\[list\]* One list per item with `value`, `name`, and
#'   `description` entries.
#' @return *\[list\]* With `choices` (action keys named by their labels) and
#'   `descriptions` (one entry per item, `""` for none).
compose_menu_choices <- function(items) {
  validate(is.list(items) && length(items) > 0L)

  values <- vapply(items, function(item) item$value, character(1))
  names <- vapply(items, function(item) item$name, character(1))
  descriptions <- vapply(items, function(item) item$description %||% "", character(1))

  list(
    choices = stats::setNames(values, names),
    descriptions = descriptions
  )
}

#' @title A menu item
#' @description Convenience constructor for the item lists
#'   `compose_menu_choices()` consumes.
#' @param value *\[character\]* The stable action key the loop dispatches on.
#' @param name *\[character\]* The item name, shown in the first column.
#' @param description *\[character, optional\]* The dim second column.
#' @return *\[list\]* One menu item.
menu_item <- function(value, name, description = "") {
  list(value = value, name = name, description = description)
}

box::export(compose_menu_choices, menu_item)
