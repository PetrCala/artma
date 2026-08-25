#' @title Preprocess column mapping before options parsing
#' @description If data.source_path is provided in user_input, read the file and auto-detect column mappings.
#' This runs BEFORE the main options prompting, ensuring data detection happens first.
#' Confirmed mappings are stored as role records in the unified per-column store
#' (`data.columns`), each carrying a `source_name` field.
#' @param user_input [list] User-supplied values
#' @param options_def [list] Flattened options definitions from template
#' @return [list] Updated user_input with auto-detected column mappings
#' @keywords internal
preprocess_column_mapping <- function(user_input, options_def) {
  box::use(artma / libs / core / utils[get_verbosity])

  # Check if data source path is provided
  data_source_path <- user_input[["data.source_path"]]

  # If no data source, skip
  if (is.null(data_source_path) || !nzchar(data_source_path) || is.na(data_source_path)) {
    return(user_input)
  }

  # If the unified column store is already supplied, respect it as-is
  existing_columns <- user_input[["data.columns"]]
  if (is.list(existing_columns) && length(existing_columns) > 0) {
    return(user_input)
  }

  # Expand path
  data_source_path <- path.expand(data_source_path)

  # Check if file exists
  if (!file.exists(data_source_path)) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("Data source path {.path {data_source_path}} not found. Skipping column auto-detection.")
    }
    return(user_input)
  }

  # Check config setup mode
  config_setup <- user_input[["data.config_setup"]]
  if (!is.null(config_setup) && config_setup == "manual") {
    # User wants manual configuration, skip auto-detection
    return(user_input)
  }

  if (get_verbosity() >= 3) {
    cli::cli_h2("Auto-detecting column mappings")
    cli::cli_alert_info("Reading data from {.path {data_source_path}}")
  }

  box::use(
    artma / data / read[read_file],
    artma / data / column_recognition[recognize_columns],
    artma / data / interactive_mapping[confirm_derivation, interactive_column_mapping],
    artma / libs / core / log[log_warn]
  )

  # Reading is the one unrecoverable step here: with no data frame there is
  # nothing to map against and nothing to prompt about.
  # Read through the same dispatch and normalization as read_data, without
  # standardizing column names yet.
  df <- tryCatch(
    read_file(data_source_path),
    error = function(e) {
      log_warn(c(
        "Could not read {.path {data_source_path}}: {e$message}",
        "i" = "Skipping column detection. Set {.code data.columns} in the options file to map columns manually."
      ))
      NULL
    }
  )
  if (is.null(df)) {
    return(user_input)
  }

  # Detection is recoverable. A crash here (an unreadable value, an unexpected
  # column type) must not also cost the user the mapping prompt: falling
  # through with an empty mapping asks about every required role instead of
  # writing an options file that silently maps nothing.
  detected <- tryCatch(
    {
      # Recognize columns via the shared matching engine
      auto_mapping <- recognize_columns(df)

      # Effect and se may be better derived from a (t, df) pair than read from
      # any column. Settled before the mapping is presented, so the two roles
      # are not then prompted for as missing.
      confirm_derivation(auto_mapping)
    },
    error = function(e) {
      log_warn(c(
        "Failed to auto-detect columns: {e$message}",
        "i" = "Falling back to mapping every required column by hand."
      ))
      NULL
    }
  )

  auto_mapping <- if (is.null(detected)) list() else detected$mapping
  derivation <- if (is.null(detected)) NULL else detected$derivation
  derived_roles <- if (is.null(derivation)) character(0) else c("effect", "se")

  # Present detected columns to the user for confirmation. This shows detected
  # columns and allows the user to accept, modify, or skip optional ones.
  # Entered even with nothing detected: `data.columns` has a non-NULL template
  # default, so an unset mapping resolves silently rather than being prompted
  # for later, and the required columns would surface only as a hard abort
  # partway through the first analysis run. `interactive_column_mapping()`
  # applies the autonomy and `interactive()` gates itself, and leaves roles
  # unmapped rather than guessing when it cannot ask.
  mapping <- interactive_column_mapping(
    df = df,
    auto_mapping = auto_mapping,
    required_only = TRUE,
    show_detected_first = TRUE,
    derived_roles = derived_roles
  )

  # The derived route needs both inputs mapped, whatever the user did with
  # the optional columns in the menus above.
  if (!is.null(derivation)) {
    mapping[["t_stat"]] <- derivation$t_stat
    mapping[["reg_dof"]] <- derivation$dof
    user_input[["data.derive_pcc"]] <- TRUE
  }

  # Convert the confirmed mapping into unified role records.
  # Skip any NULL, NA, or empty string values to prevent validation errors.
  # Identity mappings (a column already carrying the standard name) are not
  # stored: the sparse store only holds genuine renames.
  records <- list()
  for (std_col in names(mapping)) {
    val <- mapping[[std_col]]
    if (is.null(val) || (length(val) == 1 && is.na(val)) || !nzchar(trimws(val))) {
      log_warn("Skipping invalid mapping for {.field {std_col}}: value is NULL, NA, or empty")
      next
    }
    if (identical(trimws(val), std_col)) next
    records[[std_col]] <- list(source_name = trimws(val))
  }

  if (length(records) > 0) {
    user_input[["data.columns"]] <- records
  }

  user_input
}


box::export(preprocess_column_mapping)
