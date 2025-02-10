#' Check whether an object is a function call (created using 'call').
#' Return a boolean to indicate this.
#'
#' @param obj [any] The object to evaluate
#' @return [logical] TRUE if the object is a function call, FALSE otherwise
#' @export
is_function_call <- function(obj) {
  if (is.call(obj)) {
    func_name <- as.character(obj[[1L]])
    is_valid_function_call <- exists(func_name) && is.function(get(func_name))
    if (!(is_valid_function_call)) {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
  return(TRUE)
}


#' Check whether an object is empty.
#' @export
is_empty <- function(obj) {
  type_obj <- typeof(obj)

  result <- switch(type_obj,
    logical = !any(obj),
    integer = length(obj) == 0L || all(is.na(obj)),
    double = length(obj) == 0L || all(is.na(obj)), # Treat numeric as double
    character = length(obj) == 0L || any(obj == ""),
    list = length(obj) == 0L,
    NULL = TRUE,
    data.frame = is.data.frame(obj) && nrow(obj) == 0L,
    factor = length(obj) == 0L || all(is.na(obj)),
    # Default case if none of the above types match
    {
      if (is.na(obj)) {
        TRUE # Single NA values are considered empty
      } else {
        FALSE # If it's a different type and not NA, it's not empty
      }
    }
  )

  # Return the result
  return(result)
}

#' Convert a number to a percentage
#'
#' @param x [numeric] The number to convert
#' @return [character] The number as a percentage
#' @export
to_perc <- function(x) {
  return(paste0(round(x * 100L, 2L), "%"))
}


#' @title Nullable lapply
#' @description A version of lapply that disassigns items from a list in case their value is NULL. In case the result is empty, return NULL.
#' @param x A list
#' @param FUN A function
#' @return A list
#' @export
nullable_lapply <- function(x, FUN) {
  if (!is.function(FUN)) rlang::abort("FUN must be a function")
  out <- list()
  names <- names(x)
  argcount <- length(formals(FUN)) # Number of arguments
  if (!argcount %in% c(1L, 2L)) rlang::abort("Your function must contain either one or two arguments.")
  for (i in seq_along(x)) {
    name <- names[[i]]
    fun_ <- if (argcount == 2L) "FUN(i, x[[name]])" else "FUN(x[[name]])"
    out[[name]] <- eval(parse(text = fun_)) # Assigns nothing if the return is NULL
  }
  if (length(out) == 0L) NULL else out
}
