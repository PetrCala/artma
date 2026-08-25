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

#' @title Ask to pick one item from a menu
#' @description Render a single-choice menu with the shared prompt layout:
#'   optional dim hint lines, then the question and the `climenu` menu. When
#'   `choices` is a named vector, the names are the labels shown in the menu and
#'   the values are what gets returned. An empty selection (cancelled menu or
#'   non-interactive fallback of an injected `select_fn`) falls back to
#'   `default` when one is given.
#' @param question *\[character\]* A short question line shown directly above the menu.
#' @param choices *\[character\]* Menu items. Names, when present, are the displayed labels; the corresponding values are returned.
#' @param hints *\[character, optional\]* Hint lines rendered dim above the question. May contain `cli` inline markup. Defaults to `NULL`.
#' @param default *\[character, optional\]* Value (not label) returned on an empty selection and preselected in the menu. Must be one of `choices`. Defaults to `NULL`, in which case an empty selection returns `character(0)` and the caller decides what that means.
#' @param confirm *\[logical, optional\]* Whether to confirm the mapped value after selection. The confirmation only prints when the value differs from the displayed label; `climenu` already echoes the label itself. Defaults to `TRUE`.
#' @param select_fn *\[function, optional\]* Menu backend; receives `choices`, `prompt`, and `selected` and returns the selected label or an empty value. Defaults to `climenu::select`. Exposed for testing.
#' `character` The selected value, the default on an empty selection, or `character(0)` when there is no default to fall back to.
ask_select <- function(
  question,
  choices,
  hints = NULL,
  default = NULL,
  confirm = TRUE,
  select_fn = climenu::select
) {
  box::use(artma / libs / core / validation[validate])

  # An injected backend cannot hang a script, so only the real climenu menu
  # requires an interactive session. This also keeps climenu's non-interactive
  # fallback (warn and return the first choice) from being reached.
  if (!interactive() && identical(select_fn, climenu::select)) {
    cli::cli_abort("Menu selections can only be collected in interactive R sessions.")
  }

  labels <- if (is.null(names(choices))) unname(choices) else names(choices)
  values <- unname(choices)

  validate(
    is.character(question) && length(question) == 1,
    is.character(choices) && length(choices) > 0,
    all(nzchar(labels)),
    is.null(hints) || is.character(hints),
    is.null(default) || (length(default) == 1 && default %in% values),
    is.logical(confirm),
    is.function(select_fn)
  )

  render_hints(hints)

  selected_label <- select_fn(
    choices = labels,
    prompt = question,
    selected = if (is.null(default)) NULL else match(default, values)
  )

  if (rlang::is_empty(selected_label)) {
    if (is.null(default)) {
      return(character(0))
    }
    cli::cli_alert_info("No selection made. Using default: {.strong {default}}")
    return(default)
  }

  selected_value <- values[[match(selected_label, labels)]]
  if (confirm && !identical(selected_value, selected_label)) {
    cli::cli_alert_success("Selected: {.strong {selected_value}}")
  }
  selected_value
}

#' @title Ask a yes/no question
#' @description Render a Yes/No menu with the shared prompt layout and return
#'   the answer as a logical. An empty selection falls back to `default`.
#'   Aborting on a "No" is deliberately left to the caller, which owns the
#'   context-specific abort message.
#' @param question *\[character\]* A short question line shown directly above the menu.
#' @param hints *\[character, optional\]* Hint lines rendered dim above the question. May contain `cli` inline markup. Defaults to `NULL`.
#' @param default *\[logical, optional\]* Answer returned on an empty selection and preselected in the menu. Defaults to `FALSE`.
#' @param select_fn *\[function, optional\]* Menu backend passed through to `ask_select`. Defaults to `climenu::select`. Exposed for testing.
#' `logical` `TRUE` for yes, `FALSE` for no.
ask_yes_no <- function(
  question,
  hints = NULL,
  default = FALSE,
  select_fn = climenu::select
) {
  box::use(artma / libs / core / validation[validate])

  validate(
    is.logical(default) && length(default) == 1 && !is.na(default)
  )

  answer <- ask_select(
    question = question,
    choices = c("Yes" = "yes", "No" = "no"),
    hints = hints,
    default = if (default) "yes" else "no",
    confirm = FALSE,
    select_fn = select_fn
  )

  identical(answer, "yes")
}

box::export(ask_text, ask_select, ask_yes_no)
