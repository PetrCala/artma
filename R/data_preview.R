#' @title Preview data
#' @description
#' Open a data frame in R's viewer. Data can be supplied as a file path, a data
#' frame, or loaded from an options file (with the same prompt flow as
#' \code{artma()} when no options are given).
#'
#' @param data *\[character, data.frame, optional\]* Either \code{NULL}, a
#'   length-one character path to a data file, or a data frame. If \code{NULL},
#'   data is loaded from the options file (you will be prompted to select or
#'   create one in interactive mode).
#' @param options *\[character, optional\]* Name of the options file (with or
#'   without \code{.yaml} extension). If \code{NULL} and options are required,
#'   you will be prompted in interactive mode.
#' @param options_dir *\[character, optional\]* Directory containing the options
#'   file. If \code{NULL}, uses the default options directory.
#' @param preprocess *\[logical, optional\]* If \code{TRUE} (default), data is
#'   run through the full pipeline (read, preprocess, compute) so the viewer
#'   shows what runtime methods receive. If \code{FALSE}, only the raw file
#'   read (for a path) or the given data frame is shown, without
#'   options-dependent standardization or preprocessing.
#'
#' @return Invisible \code{NULL}. Opens the data in the standard R viewer
#'   (\code{utils::View()}).
#'
#' @details
#' Three data sources are supported:
#' \itemize{
#'   \item \strong{Path}: pass a length-one character path to a data file. With
#'     \code{preprocess = FALSE}, the file is read without loading options
#'     (raw read). With \code{preprocess = TRUE}, options are loaded and the
#'     full pipeline is applied.
#'   \item \strong{Data frame}: pass a data frame. With \code{preprocess = FALSE},
#'     it is viewed as-is. With \code{preprocess = TRUE}, options are loaded
#'     and preprocess + compute are applied before viewing.
#'   \item \strong{NULL}: data comes from the chosen options file (same
#'     "select or create options file" flow as \code{artma()} with empty
#'     arguments). With \code{preprocess = TRUE} (default), the full pipeline
#'     (read, preprocess, compute) is run. With \code{preprocess = FALSE}, only
#'     the data as read from file (with column standardization from options) is
#'     shown, without preprocessing or computed columns.
#' }
#'
#' In non-interactive mode, when \code{data} is \code{NULL} and \code{options}
#' is \code{NULL}, no viewer is shown (consistent with \code{artma()}).
#'
#' @examples
#' \dontrun{
#' # Preview data from options file (prompts for file if NULL)
#' data.preview(options = "my_analysis.yaml")
#'
#' # Preview raw file without loading options
#' data.preview("/path/to/data.csv", preprocess = FALSE)
#'
#' # Preview preprocessed data from a path (uses options for standardization)
#' data.preview("/path/to/data.csv", options = "my_analysis.yaml")
#'
#' # Preview a data frame as-is
#' data.preview(mtcars, preprocess = FALSE)
#' }
#'
#' @seealso
#' - \code{\link{artma}} - Run meta-analysis methods
#' - \code{prepare_data()} - Prepare data manually
#' - \code{\link{options.load}} - Load options
#'
#' @export
data.preview <- function(
    data = NULL,
    options = NULL,
    options_dir = NULL,
    preprocess = TRUE) {
  box::use(
    artma / data / read[read_data, read_data_raw],
    artma / data / index[prepare_data],
    artma / data / preprocess[preprocess_data],
    artma / data / compute[compute_optional_columns],
    artma / libs / core / validation[assert]
  )

  # Validate data: NULL, length-1 character (path), or data.frame
  if (!is.null(data)) {
    if (is.character(data)) {
      if (length(data) != 1L) {
        cli::cli_abort(
          "{.arg data} must be a single file path (length 1), a data frame, or NULL. Got character of length {length(data)}."
        )
      }
    } else if (!is.data.frame(data)) {
      cli::cli_abort(
        "{.arg data} must be NULL, a file path (character), or a data frame. Got {.type {data}}."
      )
    }
  }

  assert(
    is.logical(preprocess) && length(preprocess) == 1L && !is.na(preprocess),
    "preprocess must be a single non-NA logical"
  )

  view_if_available <- function(x, title) {
    tryCatch(
      utils::View(x, title = title),
      error = function(e) invisible(NULL)
    )
    invisible(NULL)
  }

  # Path + preprocess FALSE: raw read, no options
  if (is.character(data) && length(data) == 1L && isFALSE(preprocess)) {
    df <- read_data_raw(data)
    view_if_available(df, title = paste0("Preview: ", basename(data)))
    return(invisible(NULL))
  }

  # Data frame + preprocess FALSE: view as-is
  if (is.data.frame(data) && isFALSE(preprocess)) {
    view_if_available(data, title = "Preview: data")
    return(invisible(NULL))
  }

  # All other cases: need runtime_setup (options), then resolve df and optionally preprocess
  main <- function() {
    box::use(artma / libs / core / utils[get_verbosity])

    if (is.null(data)) {
      if (isFALSE(preprocess)) {
        df <- read_data()
        view_if_available(df, title = "Preview: data (read only)")
      } else {
        df <- prepare_data()
        view_if_available(df, title = "Preview: prepared data")
      }
      return(invisible(NULL))
    }

    if (is.character(data)) {
      df <- read_data(data)
      if (isTRUE(preprocess)) {
        df <- preprocess_data(df)
        df <- compute_optional_columns(df)
      }
      view_if_available(df, title = "Preview: prepared data")
      return(invisible(NULL))
    }

    # data is a data frame
    if (get_verbosity() >= 3) {
      cli::cli_inform("Using provided data frame (skipping file read step).")
    }
    if (isTRUE(preprocess)) {
      df <- preprocess_data(data)
      df <- compute_optional_columns(df)
    } else {
      df <- data
    }
    view_if_available(df, title = "Preview: prepared data")
    invisible(NULL)
  }

  runtime_setup( # nolint: box_usage_linter.
    options_file_name = options,
    options_dir = options_dir,
    FUN = main
  )

  invisible(NULL)
}
