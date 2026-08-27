#' @title Shared menu rendering for the interactive flows
#' @description
#' The two-column label layout every hub menu uses: an item name padded to a
#' fixed column, followed by a dim description truncated to whatever the
#' console leaves. Kept out of `interactive/hub.R` so the hub and its submenus
#' (`interactive/options_file_menu.R`) can render the same way without
#' importing one another.
NULL

box::use(
  artma / libs / core / validation[validate],
  artma / modules / methods_table[pad_cell, truncate_cell]
)

# Width climenu spends in front of every entry: the cursor mark and the
# separating spaces.
MENU_PREFIX_WIDTH <- 6L

# Spaces between the item name and description columns of a label.
COLUMN_GAP <- 2L

# The description column never shrinks below this, even in a narrow console;
# climenu truncates any overflowing line to the console width itself.
MIN_DESCRIPTION_WIDTH <- 12L

#' @title Compose value-keyed menu choices
#' @description
#' Build the named character vector `ask_select()` consumes: names are the
#' rendered labels (item name padded to a fixed column, dim description
#' truncated to the remaining width), values are stable action keys, so every
#' menu loop dispatches on values and never on labels.
#' @param items *\[list\]* One list per item with `value`, `name`, and
#'   `description` entries.
#' @param width *\[numeric, optional\]* Console width to fit the labels into.
#'   Defaults to the detected console width.
#' @return *\[character\]* Action keys, named by their rendered labels.
compose_menu_choices <- function(items, width = NULL) {
  validate(is.list(items) && length(items) > 0L)

  width <- width %||% cli::console_width()

  values <- vapply(items, function(item) item$value, character(1))
  names <- vapply(items, function(item) item$name, character(1))
  descriptions <- vapply(items, function(item) item$description %||% "", character(1))

  name_width <- max(nchar(names))
  description_width <- max(
    MIN_DESCRIPTION_WIDTH,
    width - MENU_PREFIX_WIDTH - name_width - COLUMN_GAP
  )

  gap <- strrep(" ", COLUMN_GAP)
  labels <- vapply(seq_along(names), function(i) {
    label <- pad_cell(names[[i]], name_width)
    if (nzchar(descriptions[[i]])) {
      label <- paste0(
        label, gap,
        cli::col_grey(truncate_cell(descriptions[[i]], description_width))
      )
    }
    label
  }, character(1))

  stats::setNames(values, labels)
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
