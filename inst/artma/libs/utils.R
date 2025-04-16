#' Check whether an object is a function call (created using 'call').
#' Return a boolean to indicate this.
#'
#' @param obj [any] The object to evaluate
#' `logical` TRUE if the object is a function call, FALSE otherwise
#' @export
is_function_call <- function(obj) {
  if (is.call(obj)) {
    func_name <- as.character(obj[[1]])
    is_valid_function_call <- exists(func_name) && is.function(get(func_name))
    if (!(is_valid_function_call)) {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
  return(TRUE)
}


#' Convert a number to a percentage
#'
#' @param x [numeric] The number to convert
#' `character` The number as a percentage
#' @export
to_perc <- function(x) paste0(round(x * 100, 2), "%")


#' @title Nullable lapply
#' @description A version of lapply that disassigns items from a list in case their value is NULL. In case the result is empty, return NULL.
#' @param x A list
#' @param FUN A function
#' @return *\[list\]* A list of the same length as x, with the same names. The values are the result of applying FUN to each element of x. If FUN returns NULL for an element, that element is removed from the list.
#' @export
nullable_lapply <- function(x, FUN) {
  box::use(
    artma / libs / validation[assert, validate]
  )
  validate(is.function(FUN))

  out <- list()
  names <- names(x)
  argcount <- length(formals(FUN)) # Number of arguments

  assert(argcount %in% c(1, 2), "Your function must contain either one or two arguments.")

  for (i in seq_along(x)) {
    name <- names[[i]]
    fun_ <- if (argcount == 2) "FUN(i, x[[name]])" else "FUN(x[[name]])"
    out[[name]] <- eval(parse(text = fun_)) # Assigns nothing if the return is NULL
  }

  if (length(out) == 0) NULL else out
}
