# climenu Examples
# Run these interactively in the R console

# Basic usage - single selection
example_basic <- function() {
  box::use(artma / climenu / index[menu])

  fruits <- c("Apple", "Banana", "Cherry", "Date", "Elderberry")
  selected <- menu(fruits, prompt = "Pick a fruit:")

  cli::cli_alert_info("You selected: {selected}")
}

# Multiple selection with checkboxes
example_checkbox <- function() {
  box::use(artma / climenu / index[menu])

  hobbies <- c(
    "Reading",
    "Gaming",
    "Sports",
    "Cooking",
    "Music",
    "Travel",
    "Photography"
  )

  selected <- menu(
    hobbies,
    type = "checkbox",
    prompt = "Select your hobbies (Space to toggle, Enter to confirm):"
  )

  if (length(selected) > 0) {
    cli::cli_alert_success("Your hobbies: {.val {selected}}")
  } else {
    cli::cli_alert_info("No hobbies selected")
  }
}

# Pre-selection example
example_preselected <- function() {
  box::use(artma / climenu / index[checkbox])

  languages <- c("R", "Python", "JavaScript", "Go", "Rust", "Julia")

  selected <- checkbox(
    languages,
    prompt = "Which languages do you know?",
    selected = c("R", "Python") # Pre-select R and Python
  )

  cli::cli_alert_info("You know: {.val {selected}}")
}

# Return indices instead of values
example_indices <- function() {
  box::use(artma / climenu / index[select])

  options <- c("First Option", "Second Option", "Third Option")

  index <- select(
    options,
    prompt = "Select an option:",
    return_index = TRUE
  )

  cli::cli_alert_info("You selected index: {index}")
  cli::cli_alert_info("Which corresponds to: {options[index]}")
}

# Use case: Method selection for artma
example_method_selection <- function() {
  box::use(artma / climenu / index[checkbox])

  # Simulated method list
  methods <- c(
    "effect_summary_stats",
    "fixed_effects",
    "random_effects",
    "subgroup_analysis",
    "meta_regression",
    "publication_bias",
    "sensitivity_analysis"
  )

  selected_methods <- checkbox(
    methods,
    prompt = "Select meta-analysis methods to run:",
    selected = c("effect_summary_stats", "fixed_effects")
  )

  if (length(selected_methods) > 0) {
    cli::cli_h2("Running selected methods")
    for (method in selected_methods) {
      cli::cli_alert("Running {.fn {method}}")
    }
  }
}

# Use case: File selection
example_file_selection <- function() {
  box::use(artma / climenu / index[select])

  # Get files in current directory
  files <- list.files(pattern = "\\.(csv|xlsx|json)$")

  if (length(files) == 0) {
    cli::cli_alert_warning("No data files found in current directory")
    return(NULL)
  }

  selected_file <- select(
    files,
    prompt = "Select a data file to load:"
  )

  cli::cli_alert_success("Loading: {.file {selected_file}}")
  # Here you would load the file
  # data <- read.csv(selected_file)
}

# Run all examples
run_all_examples <- function() {
  cli::cli_h1("climenu Examples")

  cli::cli_h2("Example 1: Basic Selection")
  example_basic()

  cli::cli_h2("Example 2: Checkbox (Multiple Selection)")
  example_checkbox()

  cli::cli_h2("Example 3: Pre-selected Items")
  example_preselected()

  cli::cli_h2("Example 4: Return Indices")
  example_indices()

  cli::cli_h2("Example 5: Method Selection")
  example_method_selection()
}
