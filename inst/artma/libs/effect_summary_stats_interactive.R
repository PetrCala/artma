#' @title Interactive variable selection for effect summary statistics
#' @description
#' Provides interactive prompts for selecting and configuring variables
#' for effect summary statistics analysis. Supports both automatic suggestion
#' and manual variable selection with specification of split methods.

#' Prompt user to select variables for effect summary statistics
#'
#' @description
#' Main entry point for interactive variable selection. Prompts the user to choose
#' between automatic suggestion and manual selection, then collects variable
#' configurations and updates the data config.
#'
#' @param df *\[data.frame\]* The data frame containing the data
#' @param config *\[list\]* The data configuration list
#'
#' @return *\[list\]* Updated data configuration with effect_sum_stats settings
#'
#' @export
prompt_effect_summary_var_selection <- function(df, config) {
  box::use(
    artma / libs / utils[get_verbosity],
    artma / libs / validation[validate]
  )

  validate(
    is.data.frame(df),
    is.list(config)
  )

  if (get_verbosity() >= 3) {
    cli::cli_h1("Effect Summary Statistics - Variable Selection")
  }

  # Prompt for selection mode
  selection_mode <- prompt_selection_mode()

  # Get variable configurations based on mode
  var_configs <- if (selection_mode == "auto") {
    auto_select_effect_summary_vars(df, config)
  } else {
    manual_select_effect_summary_vars(df, config)
  }

  if (length(var_configs) == 0) {
    if (get_verbosity() >= 2) {
      cli::cli_alert_warning("No variables configured for effect summary statistics.")
    }
    return(config)
  }

  # Update config with the selected variable configurations
  updated_config <- update_config_with_selections(config, var_configs)

  if (get_verbosity() >= 3) {
    cli::cli_alert_success(
      "Configured {length(var_configs)} variable{?s} for effect summary statistics"
    )
  }

  updated_config
}


#' Prompt for selection mode
#'
#' @description
#' Ask the user whether they want automatic suggestion or manual selection.
#'
#' @return *\[character\]* Either "auto" or "manual"
#'
#' @keywords internal
prompt_selection_mode <- function() {
  box::use(artma / const[CONST])

  choices <- c(
    "Automatic suggestion (recommended)" = "auto",
    "Manual selection" = "manual"
  )

  cli::cli_h2("Variable Selection Mode")
  cli::cli_text(
    "Choose how to select variables for effect summary statistics analysis."
  )
  cli::cli_ul(c(
    "{.strong Automatic:} Uses intelligent logic to suggest informative variables",
    "{.strong Manual:} Select variables and specify split methods yourself"
  ))
  cli::cat_line()

  selected <- climenu::select(
    choices = names(choices),
    prompt = "Select variable selection mode",
    selected = 1 # Default to auto
  )

  if (rlang::is_empty(selected)) {
    cli::cli_alert_info("No selection made. Using default: automatic suggestion")
    return("auto")
  }

  selected_value <- choices[selected][[1]]
  cli::cli_alert_success("Selected mode: {selected_value}")
  cli::cat_line()

  selected_value
}


#' Automatically select and configure variables
#'
#' @description
#' Uses the automatic variable suggestion logic to propose variables,
#' then prompts the user to review and confirm the suggestions.
#'
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* The data configuration
#'
#' @return *\[list\]* Named list of variable configurations
#'
#' @keywords internal
auto_select_effect_summary_vars <- function(df, config) {
  box::use(
    artma / libs / variable_suggestion[suggest_variables_for_effect_summary],
    artma / libs / utils[get_verbosity]
  )

  cli::cli_h2("Automatic Variable Suggestion")
  cli::cli_text("Analyzing variables to identify informative splits...")
  cli::cat_line()

  # Get automatic suggestions
  suggestions <- suggest_variables_for_effect_summary(
    df,
    config = config,
    min_obs_per_split = 5,
    min_variance_ratio = 0.01,
    exclude_reference = TRUE
  )

  suggested_vars <- suggestions[suggestions$suggested, ]

  if (nrow(suggested_vars) == 0) {
    cli::cli_alert_warning("No variables were automatically suggested.")
    cli::cli_text("This may occur if:")
    cli::cli_ul(c(
      "Variables have insufficient observations for meaningful splits",
      "Variables have very low variance",
      "No numeric variables are available"
    ))
    cli::cat_line()

    choices <- c("Yes, manual selection" = "yes", "No, skip" = "no")
    use_manual_selection <- climenu::select(
      choices = names(choices),
      prompt = "Would you like to manually select variables instead?",
      selected = 1
    )
    use_manual <- !rlang::is_empty(use_manual_selection) &&
      choices[use_manual_selection] == "yes"

    if (use_manual) {
      return(manual_select_effect_summary_vars(df, config))
    } else {
      return(list())
    }
  }

  # Display suggestions
  cli::cli_alert_success("Found {nrow(suggested_vars)} suggested variable{?s}:")
  cli::cat_line()

  for (i in seq_len(nrow(suggested_vars))) {
    var <- suggested_vars[i, ]
    var_name <- var$var_name
    split_method <- var$split_method
    split_value <- var$split_value

    # Get verbose name from config
    var_config <- config[[make.names(var_name)]]
    var_verbose <- if (!is.null(var_config$var_name_verbose)) {
      var_config$var_name_verbose
    } else {
      var_name
    }

    # Format description
    if (split_method == "equal") {
      desc <- cli::format_inline("{.field {var_verbose}} = {.val {split_value}}")
    } else if (split_method == "gltl") {
      if (split_value %in% c("mean", "median")) {
        desc <- cli::format_inline("{.field {var_verbose}} split at {.emph {split_value}}")
      } else {
        desc <- cli::format_inline("{.field {var_verbose}} split at {.val {split_value}}")
      }
    } else {
      desc <- cli::format_inline("{.field {var_verbose}}")
    }

    cli::cli_text("{i}. {desc} {cli::col_silver('[{var$reason}]')}")
  }
  cli::cat_line()

  # Confirm suggestions
  confirm_choices <- c("Yes, use these suggestions" = "yes", "No, manual selection" = "no")
  confirmation <- climenu::select(
    choices = names(confirm_choices),
    prompt = "Do you want to use these suggested variables?",
    selected = 1
  )
  confirmed <- !rlang::is_empty(confirmation) &&
    confirm_choices[confirmation] == "yes"

  if (!confirmed) {
    cli::cli_alert_info("Suggestions declined. Switching to manual selection...")
    cli::cat_line()
    return(manual_select_effect_summary_vars(df, config))
  }

  # Convert suggestions to variable configurations
  var_configs <- list()
  for (i in seq_len(nrow(suggested_vars))) {
    var <- suggested_vars[i, ]
    var_configs[[var$var_name]] <- list(
      var_name = var$var_name,
      split_method = var$split_method,
      split_value = var$split_value
    )
  }

  cli::cli_alert_success("Using {length(var_configs)} suggested variable{?s}")

  var_configs
}


#' Manually select and configure variables
#'
#' @description
#' Provides an interactive interface for manually selecting variables
#' and specifying their split methods (equal or gltl with threshold).
#'
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* The data configuration
#'
#' @return *\[list\]* Named list of variable configurations
#'
#' @keywords internal
manual_select_effect_summary_vars <- function(df, config) {
  box::use(artma / libs / utils[get_verbosity])

  cli::cli_h2("Manual Variable Selection")
  cli::cat_line()

  if (is.null(config) || length(config) == 0) {
    cli::cli_alert_warning("No variables available for analysis.")
    return(list())
  }

  # Get potential variables (exclude reserved columns)
  reserved_vars <- c("effect", "se", "study_id", "study_size", "sample_size", "dof")
  potential_vars <- names(config)[!names(config) %in% reserved_vars]

  # Filter to numeric/integer variables that exist in the data
  valid_vars <- character(0)
  for (var in potential_vars) {
    if (var %in% names(df)) {
      var_data <- df[[var]]
      if (is.numeric(var_data) || is.integer(var_data)) {
        valid_vars <- c(valid_vars, var)
      }
    }
  }

  if (length(valid_vars) == 0) {
    cli::cli_alert_warning("No numeric variables available for effect summary statistics.")
    return(list())
  }

  # Create display names
  var_display_names <- vapply(valid_vars, function(var) {
    var_config <- config[[var]]
    if (!is.null(var_config$var_name_verbose)) {
      paste0(var, " (", var_config$var_name_verbose, ")")
    } else {
      var
    }
  }, character(1))

  # Display instructions
  cli::cli_h3("Instructions")
  cli::cli_text(
    "{cli::symbol$info} Select variables to include in effect summary statistics"
  )
  cli::cli_text(
    "{cli::symbol$info} After selection, you'll specify split methods for each variable"
  )
  cli::cat_line()

  # Multi-select menu
  selected_indices <- climenu::checkbox(
    choices = var_display_names,
    prompt = "Select variables (use SPACE to select, ENTER to confirm)",
    return_index = TRUE
  )

  if (rlang::is_empty(selected_indices) || length(selected_indices) == 0) {
    cli::cli_alert_warning("No variables selected.")
    return(list())
  }

  selected_vars <- valid_vars[selected_indices]

  cli::cli_alert_success("Selected {length(selected_vars)} variable{?s}")
  cli::cat_line()

  # Now collect specifications for each variable
  var_configs <- list()

  for (var_name in selected_vars) {
    var_config <- specify_variable_split(var_name, df, config)
    if (!is.null(var_config)) {
      var_configs[[var_name]] <- var_config
    }
  }

  var_configs
}


#' Specify split method for a single variable
#'
#' @description
#' Prompts the user to specify how a variable should be split for
#' effect summary statistics. Supports equality splits (for binary/categorical)
#' and threshold splits (for numeric variables).
#'
#' @param var_name *\[character\]* Variable name
#' @param df *\[data.frame\]* The data frame
#' @param config *\[list\]* The data configuration
#'
#' @return *\[list\]* Variable configuration with split_method and split_value,
#' or NULL if user cancels
#'
#' @keywords internal
specify_variable_split <- function(var_name, df, config) {
  box::use(
    artma / libs / validation[validate],
    artma / data / utils[determine_vector_type],
    artma / const[CONST]
  )

  validate(
    is.character(var_name),
    length(var_name) == 1,
    is.data.frame(df),
    is.list(config)
  )

  var_config <- config[[make.names(var_name)]]
  var_verbose <- if (!is.null(var_config$var_name_verbose)) {
    var_config$var_name_verbose
  } else {
    var_name
  }

  cli::cli_h3("Configure: {var_verbose}")

  # Get variable data
  var_data <- df[[var_name]]
  var_data_clean <- var_data[!is.na(var_data) & is.finite(var_data)]

  if (length(var_data_clean) == 0) {
    cli::cli_alert_warning("No valid data for {var_verbose}. Skipping.")
    return(NULL)
  }

  # Determine data type
  data_type <- tryCatch(
    determine_vector_type(var_data, CONST$DATA_CONFIG$DATA_TYPES),
    error = function(e) "unknown"
  )

  # Display variable summary
  cli::cli_text("Data type: {.val {data_type}}")
  cli::cli_text("Valid observations: {.val {length(var_data_clean)}}")
  cli::cli_text("Unique values: {.val {length(unique(var_data_clean))}}")

  # Show range/distribution info
  if (data_type %in% c("int", "float", "perc")) {
    cli::cli_text(
      "Range: {.val {round(min(var_data_clean), 3)}} to {.val {round(max(var_data_clean), 3)}}"
    )
    cli::cli_text("Mean: {.val {round(mean(var_data_clean), 3)}}")
    cli::cli_text("Median: {.val {round(stats::median(var_data_clean), 3)}}")
  } else if (data_type == "dummy") {
    n_zero <- sum(var_data_clean == 0)
    n_one <- sum(var_data_clean == 1)
    cli::cli_text("Distribution: {.val {n_zero}} zeros, {.val {n_one}} ones")
  }

  cli::cat_line()

  # Suggest split methods based on data type
  if (data_type == "dummy") {
    # Binary: suggest equality split
    choices <- c(
      "Split by value (e.g., where variable = 1)" = "equal",
      "Skip this variable" = "skip"
    )
    suggested <- 1
  } else if (data_type == "perc") {
    # Percentage: suggest threshold
    choices <- c(
      "Split by threshold (e.g., above/below a value)" = "gltl",
      "Split by value (e.g., where variable = specific value)" = "equal",
      "Skip this variable" = "skip"
    )
    suggested <- 1
  } else {
    # Numeric: suggest threshold
    choices <- c(
      "Split by threshold (e.g., above/below mean or median)" = "gltl",
      "Split by value (e.g., where variable = specific value)" = "equal",
      "Skip this variable" = "skip"
    )
    suggested <- 1
  }

  selected <- climenu::select(
    choices = names(choices),
    prompt = "How should this variable be split?",
    selected = suggested
  )

  if (rlang::is_empty(selected)) {
    return(NULL)
  }

  split_method <- choices[selected][[1]]

  if (split_method == "skip") {
    cli::cli_alert_info("Skipping {var_verbose}")
    return(NULL)
  }

  # Collect split value based on method
  if (split_method == "equal") {
    split_value <- prompt_equal_value(var_name, var_data_clean, data_type)
  } else {
    split_value <- prompt_gltl_value(var_name, var_data_clean, data_type)
  }

  if (is.null(split_value) || is.na(split_value)) {
    cli::cli_alert_warning("Invalid split value. Skipping {var_verbose}")
    return(NULL)
  }

  cli::cli_alert_success("Configured {var_verbose} with {split_method} = {split_value}")
  cli::cat_line()

  list(
    var_name = var_name,
    split_method = split_method,
    split_value = as.character(split_value)
  )
}


#' Prompt for equality split value
#'
#' @description
#' Ask user to specify which value to use for an equality split.
#'
#' @param var_name *\[character\]* Variable name
#' @param var_data *\[numeric\]* Clean variable data
#' @param data_type *\[character\]* Data type
#'
#' @return *\[character\]* The split value
#'
#' @keywords internal
prompt_equal_value <- function(var_name, var_data, data_type) {
  unique_vals <- sort(unique(var_data))

  if (data_type == "dummy") {
    # For binary, default to 1
    cli::cli_text("Typical choice: compare where {var_name} = 1 vs. {var_name} = 0")
    default_val <- "1"
  } else if (length(unique_vals) <= 10) {
    # For categorical/few values, show options
    cli::cli_text("Available values: {.val {unique_vals}}")
    default_val <- as.character(unique_vals[1])
  } else {
    cli::cli_text("Variable has {length(unique_vals)} unique values")
    default_val <- as.character(stats::median(var_data))
  }

  value <- readline(
    prompt = paste0("Enter value for equality comparison [default: ", default_val, "]: ")
  )

  if (nchar(trimws(value)) == 0) {
    return(default_val)
  }

  # Try to convert to numeric if possible
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!is.na(numeric_value)) {
    return(as.character(numeric_value))
  }

  value
}


#' Prompt for threshold split value
#'
#' @description
#' Ask user to specify threshold for above/below split.
#' Can be numeric value, "mean", or "median".
#'
#' @param var_name *\[character\]* Variable name
#' @param var_data *\[numeric\]* Clean variable data
#' @param data_type *\[character\]* Data type
#'
#' @return *\[character\]* The split value (number, "mean", or "median")
#'
#' @keywords internal
prompt_gltl_value <- function(var_name, var_data, data_type) {
  var_mean <- mean(var_data, na.rm = TRUE)
  var_median <- stats::median(var_data, na.rm = TRUE)

  if (data_type == "perc") {
    # For percentages, suggest 0.5
    cli::cli_text("Typical choice: 0.5 (split at 50%)")
    default_val <- "0.5"
  } else {
    # For numeric, suggest mean or median
    cli::cli_text("You can enter:")
    cli::cli_ul(c(
      "{.val mean} - split at mean ({round(var_mean, 3)})",
      "{.val median} - split at median ({round(var_median, 3)})",
      "A specific numeric value"
    ))
    default_val <- "mean"
  }

  value <- readline(
    prompt = paste0("Enter threshold value [default: ", default_val, "]: ")
  )

  if (nchar(trimws(value)) == 0) {
    return(default_val)
  }

  value_trimmed <- trimws(tolower(value))

  # Check if it's mean or median
  if (value_trimmed %in% c("mean", "median")) {
    return(value_trimmed)
  }

  # Try to parse as numeric
  numeric_value <- suppressWarnings(as.numeric(value_trimmed))
  if (!is.na(numeric_value)) {
    return(as.character(numeric_value))
  }

  # Invalid input, return default
  cli::cli_alert_warning("Invalid input. Using default: {default_val}")
  default_val
}


#' Update data config with variable selections
#'
#' @description
#' Takes the variable configurations from selection and updates
#' the data config with appropriate effect_sum_stats flags and split values.
#'
#' @param config *\[list\]* Original data configuration
#' @param var_configs *\[list\]* Named list of variable configurations
#'
#' @return *\[list\]* Updated data configuration
#'
#' @keywords internal
update_config_with_selections <- function(config, var_configs) {
  box::use(artma / libs / validation[validate])

  validate(is.list(config), is.list(var_configs))

  if (length(var_configs) == 0) {
    return(config)
  }

  for (var_name in names(var_configs)) {
    var_conf <- var_configs[[var_name]]
    config_key <- make.names(var_name)

    if (!config_key %in% names(config)) {
      next
    }

    # Set effect_sum_stats flag
    config[[config_key]]$effect_sum_stats <- TRUE

    # Set split method and value
    if (var_conf$split_method == "equal") {
      config[[config_key]]$equal <- var_conf$split_value
      # Ensure gltl is NA
      config[[config_key]]$gltl <- NA_character_
    } else if (var_conf$split_method == "gltl") {
      config[[config_key]]$gltl <- var_conf$split_value
      # Ensure equal is NA
      config[[config_key]]$equal <- NA_character_
    }
  }

  config
}


box::export(
  prompt_effect_summary_var_selection,
  prompt_selection_mode,
  auto_select_effect_summary_vars,
  manual_select_effect_summary_vars,
  specify_variable_split,
  update_config_with_selections
)
