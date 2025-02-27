PACKAGE_NAME <- "artma"
PACKAGE_NAME_VERBOSE <- "Automatic Replication Tools for Meta-Analysis"

#' @export
CONST <- list(
  # Base
  PACKAGE_NAME = PACKAGE_NAME,
  PACKAGE_NAME_VERBOSE = PACKAGE_NAME_VERBOSE,

  # Various
  DATE_FORMAT = "%Y-%m-%d %H:%M:%S",
  DATE_ONLY_FORMAT = "%Y-%m-%d",

  # Logger
  LOG_PATTERN = "^(INFO|WARN|ERROR|DEBUG|FATAL)\\s+\\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\]\\s*",
  LOG_LEVEL_MAP = list(
    DEBUG = logger::DEBUG,
    INFO  = logger::INFO,
    WARN  = logger::WARN,
    ERROR = logger::ERROR,
    FATAL = logger::FATAL
  ),

  # A collection of all ARTMA supported methods
  SUPPORTED_METHODS = c(
    "variable_summary_stats", # Various summary statistics
    "effect_summary_stats", # Effect summary statistics
    "prima_facie_graphs", # Prima Facie graphs
    "box_plot", # Box plots
    "funnel_plot", # Funnel plot
    "t_stat_histogram", # Histogram of t-statistics
    "linear_tests", # Linear tests for publication bias
    "nonlinear_tests", # Non-linear tests for publication bias
    "exo_tests", # Exogeneity tests for publication bias
    "p_hacking_tests", # P-hacking tests
    "bma", # Bayesian Model Averaging
    "fma", # Frequentist Model Averaging
    "ma_variables_description_table", # Model Averaging variable descriptions
    "bpe", # Best-practice estimate
    "bpe_graphs", # Best-practice estimate graphs
    "robma" # Robust Bayesian Model Averaging
  ),

  # Config
  CONFIG_SPECIAL_KEYS = c("description", "details", "type", "optional", "default", "values"),

  # Regex
  REGEX = list(
    OPTIONS_FILE_SUFFIX = "\\.yaml|\\.yml"
  )
)
