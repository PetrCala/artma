#' Interactive CLI Menu
#'
#' Creates an interactive menu in the R console allowing users to select items.
#' Inspired by inquirer.js, Python's pick, and Go's survey libraries.
#'
#' @param choices *\[character\]* Vector of choices to display
#' @param prompt *\[character, optional\]* Prompt message to display (default: "Select an item:")
#' @param type *\[character, optional\]* Menu type: "select" (single) or "checkbox" (multiple) (default: "select")
#' @param selected *\[integer|character, optional\]* Pre-selected items (indices or values)
#' @param return_index *\[logical, optional\]* Return indices instead of values (default: FALSE)
#'
#' @return Selected item(s) as character vector or indices
#' @export
menu <- function(choices,
                 prompt = "Select an item:",
                 type = c("select", "checkbox"),
                 selected = NULL,
                 return_index = FALSE) {
  type <- match.arg(type)

  if (type == "checkbox") {
    box::use(. / checkbox[checkbox])
    return(checkbox(
      choices = choices,
      prompt = prompt,
      selected = selected,
      return_index = return_index
    ))
  } else {
    box::use(. / select[select])
    return(select(
      choices = choices,
      prompt = prompt,
      selected = selected,
      return_index = return_index
    ))
  }
}
