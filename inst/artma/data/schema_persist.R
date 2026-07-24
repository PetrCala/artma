#' @title Schema Reconciliation Persistence
#' @description Write layer for schema reconciliation: reads the unified
#'   per-column store, applies resolved decisions to it, and persists both the
#'   store and the expected-schema-columns baseline through the single write
#'   path. No prompts and no detection logic live here.

#' @title Read the unified per-column store from options
#' @keywords internal
get_columns_store <- function() {
  store <- getOption("artma.data.columns", list())
  if (!is.list(store)) {
    return(list())
  }
  store
}

#' @title Normalize expected schema columns
#' @keywords internal
normalize_expected_schema_cols <- function(cols) {
  if (is.null(cols)) {
    return(character(0))
  }

  if (is.list(cols)) {
    cols <- unlist(cols, use.names = FALSE)
  }

  cols <- as.character(cols)
  cols <- cols[!is.na(cols)]
  cols <- trimws(cols)
  cols <- cols[nzchar(cols)]

  if (length(cols) == 0L) {
    return(character(0))
  }

  unique(make.names(cols))
}

#' @title Persist expected schema columns
#' @keywords internal
persist_expected_schema_cols <- function(cols) {
  box::use(artma / libs / core / utils[get_verbosity])

  normalized <- normalize_expected_schema_cols(cols)
  options("artma.data.expected_schema_columns" = normalized)

  options_file_name <- getOption("artma.temp.file_name", NULL)
  options_dir <- getOption("artma.temp.dir_name", NULL)
  has_options_file <- !is.null(options_file_name) && !is.null(options_dir)

  if (!has_options_file) {
    return(invisible(normalized))
  }

  tryCatch(
    {
      suppressMessages(
        artma::options.modify(
          options_file_name = options_file_name,
          options_dir = options_dir,
          user_input = list("data.expected_schema_columns" = normalized),
          should_validate = FALSE
        )
      )
    },
    error = function(e) {
      if (get_verbosity() >= 2) {
        cli::cli_alert_warning(
          "Could not persist expected schema columns to options file: {e$message}"
        )
      }
    }
  )

  invisible(normalized)
}

#' @title Apply reconciliation decisions
#' @description Applies all decisions to the unified per-column store and
#'   persists it through the single write path.
#' @keywords internal
apply_reconciliation <- function(decisions) {
  box::use(artma / data_config / write[write_unified_columns])

  store <- get_columns_store()

  # 1. Role renames: update the record's source_name
  for (std in names(decisions$renames)) {
    entry <- store[[std]]
    if (!is.list(entry)) entry <- list()
    entry$source_name <- decisions$renames[[std]]
    store[[std]] <- entry
  }

  # 2. Moderator drops: remove the record
  for (mod in decisions$drops) {
    store[[make.names(mod)]] <- NULL
  }

  # 3. Moderator remaps: move the record to the new key
  for (old_mod in names(decisions$remaps)) {
    new_mod <- decisions$remaps[[old_mod]]
    old_key <- make.names(old_mod)
    new_key <- make.names(new_mod)

    entry <- store[[old_key]]
    if (!is.list(entry)) entry <- list()
    entry$var_name <- new_mod
    store[[new_key]] <- entry
    if (!identical(old_key, new_key)) {
      store[[old_key]] <- NULL
    }
  }

  # 4. Mapping conflict resolutions: either the mapping wins and the raw
  #    column is flagged for dropping at standardization time, or the mapping
  #    is removed so the raw column backs the role directly.
  for (std in names(decisions$conflicts)) {
    entry <- store[[std]]
    if (!is.list(entry)) entry <- list()
    if (identical(decisions$conflicts[[std]], "keep_mapping")) {
      entry$drop_conflicting_raw <- TRUE
      store[[std]] <- entry
    } else {
      entry$source_name <- NULL
      entry$drop_conflicting_raw <- NULL
      store[[std]] <- if (length(entry) == 0) NULL else entry
    }
  }

  write_unified_columns(store)

  invisible(NULL)
}

box::export(
  get_columns_store,
  normalize_expected_schema_cols,
  persist_expected_schema_cols,
  apply_reconciliation
)
