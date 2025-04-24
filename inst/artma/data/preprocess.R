standardize_column_names <- function(df, schema) {
  map <- getOption("data.colnames")

  # # canonical schema -------------------------------------------------------
  # schema <- list(
  #   required = c("obs_id", "study_id", "study_name",
  #                "effect", "se", "n_obs"),
  #   optional = c("t_stat", "study_size", "reg_df", "precision")
  # )

  # `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x


  # 2a ─ check required elements are supplied
  miss_req <- setdiff(schema$required, names(map))
  if (length(miss_req)) {
    abort(paste(
      "Missing mapping for required columns:",
      paste(miss_req, collapse = ", ")
    ))
  }

  # 2b ─ check that every mapped raw column exists in `df`
  absent_raw <- map[!map %in% names(df)]
  if (length(absent_raw)) {
    abort(paste(
      "These columns are absent in the data frame:",
      paste(absent_raw, collapse = ", ")
    ))
  }

  # 2c ─ rename, keeping only columns we care about
  df_std <- df |> rename(!!!setNames(names(map), map))

  # 2d ─ attach original names for later
  attr(df_std, "original_names") <- map

  df_std
}

#' @title Preprocess the raw data
#' @param input_data *\[data.frame\]* Main data frame
#' @return *\[data.frame\]* The preprocessed data
preprocess_data <- function(input_data) { # nolint: cyclocomp_linter
  box::use(
    artma / data_config / read[get_data_config],
    artma / data_config / utils[get_config_values],
    artma / libs / validation[validate]
  )

  validate(is.data.frame(input_data))

  # Standardize column names
  input_data <- standardize_column_names(input_data)

  config <- get_data_config()

  # Remove redundant columns
  expected_col_n <- length(config)
  while (ncol(input_data) > expected_col_n) {
    input_data <- input_data[, -ncol(input_data)]
  }

  # Variable name validity check
  varnames <- colnames(input_data)
  expected_varnames <- get_config_values(config, "var_name")

  # Check if all columns of the first vector are in the second one and vice versa
  if (!all(varnames %in% expected_varnames) || !all(expected_varnames %in% varnames)) {
    missing_from_var_list <- varnames[!varnames %in% expected_varnames]
    missing_from_data <- expected_varnames[!expected_varnames %in% varnames]
    cli::cli_abort(c(
      "x" = "Mismatching variable names.",
      "i" = "These variables are not a part of the variable list: {.val {missing_from_var_list}}",
      "i" = "These variables are not a part of the main data frame columns: {.val {missing_from_data}}"
    ))
  }

  # Check for correct ordering
  if (!identical(varnames, expected_varnames)) {
    problematic_indexes <- which(varnames != expected_varnames)
    cli::cli_abort(c(
      "x" = "The order of some columns in the data frame and the expected variable list is different.",
      "i" = "Problematic indexes and their column names: {.val {problematic_indexes}}",
      "i" = "Data frame has '{.val {varnames[problematic_indexes]}}' but expected variable list has '{.val {expected_varnames[problematic_indexes]}}'."
    ))
  }


  # Remove redundant rows
  # while (is.na(input_data[nrow(input_data), "study_name"])) {
  #   input_data <- input_data[-nrow(input_data), ]
  # }

  # # Preprocess and enforce correct data types
  # for (col_name in varnames) {
  #   col_data_type <- input_var_list$data_type[input_var_list$var_name == col_name]
  #   if (col_data_type == "int" || col_data_type == "dummy") {
  #     input_data[[col_name]] <- as.integer(input_data[[col_name]])
  #   } else if (col_data_type == "float" || col_data_type == "perc") {
  #     input_data[[col_name]] <- as.numeric(input_data[[col_name]])
  #   } else if (col_data_type == "category") {
  #     input_data[[col_name]] <- as.character(input_data[[col_name]])
  #   }
  # }
  # cli::cli_alert_success("Preprocessing finished.")
  # return(input_data)
}

box::export(
  preprocess_data
)
