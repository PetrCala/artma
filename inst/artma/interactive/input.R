#' @title Render dim hint lines
#' @description Print hint lines (examples, format notes) dim and indented,
#'   followed by a blank separator line. A `NULL` or empty vector renders
#'   nothing.
#' @param hints *\[character, optional\]* Hint lines. May contain `cli` inline markup.
#' @keywords internal
render_hints <- function(hints) {
  for (hint in hints) {
    cli::cat_line("  ", cli::col_grey(cli::format_inline(hint)))
  }
  if (length(hints) > 0) {
    cli::cat_line()
  }
}

#' @title Ask for a line of text
#' @description Render a text prompt with a consistent layout: a short question
#'   line, optional dim hint lines below it, and an input row that starts at the
#'   left margin. An empty answer falls back to `default` when one is given;
#'   otherwise the question is re-asked up to `max_retries` times unless
#'   `allow_empty` is set.
#' @param question *\[character\]* A short question line. May contain `cli` inline markup; interpolate any variables before passing (e.g. with `cli::format_inline()`).
#' @param hints *\[character, optional\]* Hint lines (examples, format notes) rendered dim below the question. May contain `cli` inline markup. Defaults to `NULL`.
#' @param default *\[character, optional\]* Value returned when the user presses Enter without typing; shown in the input row. Never passed through `sanitize` or `validate`; it is a trusted caller-supplied value. Defaults to `NULL`.
#' @param allow_empty *\[logical, optional\]* Whether an empty answer is returned as-is instead of re-asking. Defaults to `FALSE`.
#' @param max_retries *\[integer, optional\]* Total number of attempts before giving up and returning an empty string. Defaults to 3.
#' @param error_message *\[character, optional\]* Message shown when an empty answer is rejected. Defaults to "Value cannot be empty.".
#' @param validate *\[function, optional\]* Called with a non-empty answer; returns `NULL` to accept it, or a character error message which is shown before re-asking. Rejections count toward `max_retries`. Defaults to `NULL` (no validation).
#' @param sanitize *\[function, optional\]* Applied to the trimmed raw answer before the default substitution and emptiness checks, so a sanitizer that strips an answer down to nothing (e.g. a quote stripper) still falls through to `default`. Defaults to `NULL` (no sanitization).
#' @param read_input *\[function, optional\]* Reader used to collect the answer; receives the input row prefix and returns a character. Defaults to `readline`. Exposed for testing.
#' `character` The accepted answer, or an empty string when allowed or after all attempts were used up.
ask_text <- function(
  question,
  hints = NULL,
  default = NULL,
  allow_empty = FALSE,
  max_retries = 3,
  error_message = "Value cannot be empty.",
  validate = NULL,
  sanitize = NULL,
  read_input = readline
) {
  # The `validate` parameter shadows the validation helper, so import it under
  # an alias.
  box::use(artma / libs / core / validation[validate_args = validate])

  # An injected reader cannot hang a script, so only the real readline requires
  # an interactive session.
  if (!interactive() && identical(read_input, readline)) {
    cli::cli_abort("Text input can only be collected in interactive R sessions.")
  }

  validate_args(
    is.character(question) && length(question) == 1,
    is.null(hints) || is.character(hints),
    is.null(default) || (is.character(default) && length(default) == 1),
    is.logical(allow_empty),
    is.null(validate) || is.function(validate),
    is.null(sanitize) || is.function(sanitize),
    is.function(read_input)
  )

  cli::cli_text(question)
  render_hints(hints)

  # Kept free of ANSI styling: some terminals miscount the width of styled
  # readline prompts, which breaks line editing.
  input_prefix <- if (is.null(default)) "> " else sprintf("[%s] > ", default)

  attempts <- 0
  repeat {
    answer <- trimws(read_input(input_prefix))
    attempts <- attempts + 1
    if (!is.null(sanitize)) {
      answer <- sanitize(answer)
      if (!is.character(answer) || length(answer) != 1 || is.na(answer)) {
        answer <- ""
      }
    }
    if (answer == "" && !is.null(default)) {
      return(default)
    }
    if (answer == "") {
      if (allow_empty || attempts >= max_retries) {
        return(answer)
      }
      cli::cli_alert_danger(error_message)
      next
    }
    if (!is.null(validate)) {
      validation_error <- validate(answer)
      if (!is.null(validation_error)) {
        cli::cli_alert_danger(validation_error)
        if (attempts >= max_retries) {
          return("")
        }
        next
      }
    }
    return(answer)
  }
}

box::export(ask_text)
