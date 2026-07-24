#' @title Read the stored column records from an options file
#' @description Returns the `data.columns` store held in a user options file,
#'   falling back to the session option when the file cannot be read (it may
#'   not exist yet during creation). Used to seed a merge so records already on
#'   disk are never clobbered by a partial in-session store.
#' @param options_file_name *\[character\]* Options file name, including suffix.
#' @param options_dir *\[character, optional\]* Directory holding user options
#'   files. Defaults to the standard directory.
#' @return *\[list\]* The stored per-column records, possibly empty.
read_stored_columns <- function(options_file_name, options_dir = NULL) {
  box::use(
    artma / options / files[
      options_file_path,
      read_options_file,
      resolve_options_dir
    ]
  )

  fallback <- function() {
    store <- getOption("artma.data.columns", list())
    if (is.list(store)) store else list()
  }

  tryCatch(
    {
      path <- options_file_path(
        resolve_options_dir(options_dir, must_exist = FALSE),
        options_file_name
      )
      if (!file.exists(path)) {
        return(fallback())
      }
      store <- read_options_file(path)[["data"]][["columns"]]
      if (is.list(store)) store else list()
    },
    error = function(e) fallback()
  )
}

#' @title Save column mapping to options
#' @description Save the confirmed mapping into the unified per-column store
#'   (`data.columns`): each standard column gets a role record carrying the
#'   mapped `source_name`.
#' @param mapping *\[list\]* The column mapping (std_col -> data_col)
#' @param options_file_name *\[character, optional\]* Options file name
#' @param options_dir *\[character, optional\]* Directory holding user options
#'   files. Defaults to the standard directory.
#' @return *\[invisible\]* NULL
save_column_mapping_to_options <- function(mapping, options_file_name = NULL, options_dir = NULL) {
  box::use(artma / libs / core / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Saving column mapping to options")
  }

  # Merge the mapping into the existing unified column store. Identity
  # mappings clear any stale source_name instead of being stored: the sparse
  # store only holds genuine renames.
  #
  # When writing to a named options file, seed from that file rather than the
  # session option: this runs during file creation too, where the option is
  # still unset and using it would overwrite the file's records with the role
  # mapping alone.
  store <- if (!is.null(options_file_name)) {
    read_stored_columns(options_file_name, options_dir = options_dir)
  } else {
    getOption("artma.data.columns", list())
  }
  if (!is.list(store)) store <- list()

  for (std_col in names(mapping)) {
    entry <- store[[std_col]]
    if (!is.list(entry)) entry <- list()
    if (identical(mapping[[std_col]], std_col)) {
      entry$source_name <- NULL
      store[[std_col]] <- if (length(entry) == 0) NULL else entry
    } else {
      entry$source_name <- mapping[[std_col]]
      store[[std_col]] <- entry
    }
  }

  # Update options
  if (!is.null(options_file_name)) {
    artma::options.modify(
      user_input = list("data.columns" = store),
      options_file_name = options_file_name,
      options_dir = options_dir
    )
    options("artma.data.columns" = store)
  } else {
    # Set in current session
    options("artma.data.columns" = store)
  }

  if (get_verbosity() >= 3) {
    cli::cli_alert_success("Column mapping saved")
  }

  invisible(NULL)
}

box::export(
  read_stored_columns,
  save_column_mapping_to_options
)
