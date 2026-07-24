#' Assign NA to a column in a data frame
assign_na_col <- function(df, colname) {
  df[[colname]] <- rep(NA, nrow(df))
  df
}

#' @title Get standard column names
#' @description Get the vector of standard column names the pipeline recognizes.
#'   These are the role keys of the unified per-column store (`data.columns`).
#' @return *\[character\]* A vector of column names.
get_standardized_colnames <- function() {
  box::use(artma / const[CONST])
  CONST$DATA$STANDARD_COLNAMES
}

#' @title Get required columns
#' @description Get a vector of columns required by the analysis to exist in the data frame.
#' @return *\[character\]* A vector of column names.
get_required_colnames <- function() {
  box::use(artma / const[CONST])
  CONST$DATA$REQUIRED_COLNAMES
}

#' @title Get the column name mapping from the unified column store
#' @description Reads `artma.data.columns` and extracts the mapping from
#'   standard column names to the source column names in the user's data file.
#'   Only role records (keyed by a standard column name) with a non-NA
#'   `source_name` participate. Identity mappings are typically absent from the
#'   sparse store; a standard column already named correctly in the data frame
#'   needs no record.
#' @return *\[list\]* Named list mapping standard names to source column names.
get_colnames_map <- function() {
  store <- getOption("artma.data.columns", list())
  if (!is.list(store) || length(store) == 0) {
    return(list())
  }

  map <- list()
  for (std in intersect(names(store), get_standardized_colnames())) {
    entry <- store[[std]]
    if (!is.list(entry)) next
    src <- entry[["source_name"]]
    if (is.null(src) || length(src) != 1 || is.na(src) || !nzchar(src)) next
    map[[std]] <- src
  }
  map
}

#' @title Get reserved column names
#' @description Get the full set of standard column names produced by the
#'   data pipeline: required input columns plus columns computed downstream
#'   (e.g. `obs_id`, `reg_dof`, `precision`). These are never valid moderator
#'   variables and must be excluded from variable detection and suggestion.
#' @return *\[character\]* A vector of reserved column names.
get_reserved_colnames <- function() {
  box::use(artma / const[CONST])
  union(get_required_colnames(), CONST$DATA$COMPUTED_COLNAMES)
}

#' Get the number of studies in an analysis data frame.
#'
#' @param df *\[data.frame\]* The analysis data frame.
#' @return *\[integer\]* The number of studies.
get_number_of_studies <- function(df) {
  if (!"study_id" %in% colnames(df)) {
    cli::cli_abort("The data frame does not have a 'study_id' column.", class = "missing_study_column")
  }
  length(table(df$study_id))
}

#' @title Standardize column names
#' @description Standardize the column names of a data frame to a single source of truth set of column names.
#'   This is the pure standardizer: it never prompts and never attempts
#'   auto-detection. Callers that need to fill in a missing mapping
#'   interactively should run that workflow first (see
#'   `artma / data / interactive_mapping`) and persist the result before
#'   calling this function.
#' @param df *\[data.frame\]* The data frame to standardize
#' @return *\[data.frame\]* The standardized data frame
standardize_column_names <- function(df) {
  box::use(
    artma / libs / core / utils[get_verbosity],
    artma / libs / core / validation[validate, assert]
  )

  validate(is.data.frame(df))

  names(df) <- make.names(names(df))

  # Read the column mapping from the unified per-column store
  map <- lapply(get_colnames_map(), make.names) # Handle non-standard column names
  required_colnames <- get_required_colnames()

  # Check that every required column is either mapped or already present
  missing_required <- base::setdiff(required_colnames, c(names(map), names(df)))
  if (length(missing_required)) {
    cli::cli_abort(c(
      "x" = "Missing mapping for required columns: {.val {missing_required}}",
      "i" = "Please specify column mappings in your options file or enable automatic detection"
    ))
  }

  # Check that every required column exists in the data frame
  mapped_cols <- unlist(unname(map[names(map) %in% required_colnames]))
  absent_required <- mapped_cols[!mapped_cols %in% names(df)]
  if (length(absent_required)) {
    cli::cli_abort(c(
      "x" = "These required columns are absent in the data frame: {.val {absent_required}}",
      "i" = "Available columns: {.val {paste(names(df), collapse = ', ')}}"
    ))
  }

  # Rename columns to standardized names - only when present in the data frame
  reverse_map <- stats::setNames(names(map), unlist(map))
  rename_from <- names(df)[names(df) %in% names(reverse_map)]
  rename_to <- unname(reverse_map[rename_from])

  # A rename target already occupied by a different column would otherwise
  # silently produce two columns sharing a name; `df$<name>` would then return
  # whichever comes first, ignoring the user's explicit mapping. Detect this
  # before renaming and abort, except for the cases where no collision
  # actually results: an identity mapping (source == target, a no-op), the
  # occupying column being renamed away itself in this same pass, or the user
  # having resolved the conflict in favor of the mapping (drop_conflicting_raw).
  column_store <- getOption("artma.data.columns", list())
  if (!is.list(column_store)) column_store <- list()

  for (i in seq_along(rename_from)) {
    source_col <- rename_from[[i]]
    target_col <- rename_to[[i]]

    if (identical(source_col, target_col)) next
    if (!target_col %in% names(df)) next
    if (target_col %in% rename_from) next

    if (identical(df[[source_col]], df[[target_col]])) {
      # Byte-identical duplicate: drop the stale column quietly and proceed.
      df[[target_col]] <- NULL
      next
    }

    store_entry <- column_store[[target_col]]
    if (is.list(store_entry) && isTRUE(store_entry[["drop_conflicting_raw"]])) {
      if (get_verbosity() >= 3) {
        cli::cli_alert_info(
          "Dropping the raw {.val {target_col}} column: {.val {source_col}} is configured to supply it."
        )
      }
      df[[target_col]] <- NULL
      next
    }

    cli::cli_abort(c(
      "x" = "Cannot map {.val {source_col}} to standard column {.val {target_col}}: the data frame already has a different column named {.val {target_col}}.",
      "i" = "Keep the mapping and drop the raw column: {.code artma::config.set(\"{target_col}\", drop_conflicting_raw = TRUE)}",
      "i" = "Use the raw {.val {target_col}} column instead: {.code artma::config.reset(\"{target_col}\")}",
      "i" = "Or map {.val {target_col}} to another source column: {.code artma::config.set(\"{target_col}\", source_name = \"<column>\")}"
    ))
  }

  names(df)[names(df) %in% names(reverse_map)] <- reverse_map[names(df)[names(df) %in% names(reverse_map)]]

  missing_required <- setdiff(required_colnames, colnames(df))
  if (length(missing_required)) {
    cli::cli_abort("Failed to standardize the following columns: {.val {missing_required}}")
  }

  assert(!anyDuplicated(names(df)), cli::format_inline(
    "Standardization produced duplicated column names: {.val {names(df)[duplicated(names(df))]}}"
  ))

  df
}


#' @description Raise an error for an invalid data type.
#' @param data_type [str] The invalid data type.
#' @return `NULL`
raise_invalid_data_type_error <- function(data_type) {
  box::use(artma / const[CONST])

  cli::cli_abort(c(
    cli::format_inline("{CONST$PACKAGE_NAME} does not currently support the following data type {.val {data_type}}."),
    cli::format_inline("Supported data types are {.val {CONST$DATA$TYPES}}.")
  ))
}


#' @title Determine data type
#' @description Determine a data type based on its path.
#' @param path [str] The path to the data.
#' `str` The data type
determine_df_type <- function(path, should_validate = TRUE) {
  box::use(
    artma / const[CONST],
    artma / libs / core / validation[validate]
  )

  validate(is.character(path))

  if (!file.exists(path)) {
    cli::cli_abort("The specified data file path {.path {path}} is invalid. No such file found.")
  }

  file_extension <- tools::file_ext(path)

  if (should_validate && !(file_extension %in% CONST$DATA$TYPES)) {
    raise_invalid_data_type_error(file_extension)
  }

  file_extension
}

determine_vector_type <- function(data, recognized_data_types = NULL) {
  box::use(
    artma / libs / core / validation[validate]
  )

  validate(is.vector(data))

  data_type <- if (length(data[!is.na(data)]) == 0) {
    "empty"
  } else if (is.logical(data)) {
    "dummy"
  } else if (is.character(data)) {
    "category"
  } else if (is.numeric(data)) {
    clean_data <- data[!is.na(data)]
    if (all(clean_data == floor(clean_data))) {
      if (all(clean_data >= 0 & clean_data <= 100)) {
        "perc"
      } else {
        "int"
      }
    } else {
      "float"
    }
  } else {
    "unknown"
  }

  if (!is.null(recognized_data_types)) {
    if (!(data_type %in% recognized_data_types)) {
      cli::cli_abort("The data type {.val {data_type}} is not supported. Please use one of the following types: {.val {recognized_data_types}}.")
    }
  }

  data_type
}

box::export(
  assign_na_col,
  determine_df_type,
  determine_vector_type,
  get_colnames_map,
  get_number_of_studies,
  get_required_colnames,
  get_reserved_colnames,
  get_standardized_colnames,
  raise_invalid_data_type_error,
  standardize_column_names
)
