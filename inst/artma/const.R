PACKAGE_NAME <- "artma"
PACKAGE_NAME_VERBOSE <- "Automatic Replication Tools for Meta-Analysis"

#' @export
CONST <- list(
  PACKAGE_NAME = PACKAGE_NAME,
  PACKAGE_NAME_VERBOSE = PACKAGE_NAME_VERBOSE,
  DATA = list(
    # A list of recognized data (meaning data frame) types
    TYPES = c("csv", "tsv", "xlsx", "xls", "xlsm", "json", "dta", "rds")
  ),
  DATE_FORMAT = "%Y-%m-%d %H:%M:%S",
  DATE_ONLY_FORMAT = "%Y-%m-%d",
  DATA_CONFIG = list(
    KEYS = list(
      VAR_NAME = "var_name",
      VAR_NAME_VERBOSE = "var_name_verbose",
      VAR_NAME_DESCRIPTION = "var_name_description",
      DATA_TYPE = "data_type",
      GROUP_CATEGORY = "group_category",
      NA_HANDLING = "na_handling",
      VARIABLE_SUMMARY = "variable_summary",
      EFFECT_SUM_STATS = "effect_sum_stats",
      EQUAL = "equal",
      GLTL = "gltl",
      BMA = "bma",
      BMA_REFERENCE_VAR = "bma_reference_var",
      BMA_TO_LOG = "bma_to_log",
      BPE = "bpe",
      BPE_SUM_STATS = "bpe_sum_stats",
      BPE_EQUAL = "bpe_equal",
      BPE_GLTL = "bpe_gltl"
    ),
    DATA_TYPES = c("dummy", "category", "int", "float", "perc", "empty", "unknown"),
    SETUP_TYPES = c("auto", "manual")
  ),
  OPTIONS = list(
    VALIDATION_ACTIONS = c(
      "abort_verbose",
      "abort_quiet",
      "return_errors_verbose",
      "return_errors_quiet"
    ),
    PROMPT_TYPES = list(
      READLINE = "readline",
      FILE = "file",
      DIRECTORY = "directory",
      FUNCTION = "function"
    ),
    # These are the names of the template files that are used to generate the options template.
    TEMPLATE_NAMES = c("template.yaml", "template.yml", "options_template.yaml", "options_template.yml"),
    # These are the names of the keywords that are recognized in the options template.
    RECOGNIZED_KEYWORDS = c("default", "help", "prompt", "allow_na", "confirm_default"),
    DEFAULT_PROMPT_TYPE = "readline",
    SPECIAL_KEYS = c("description", "details", "type", "optional", "default", "values")
  ),
  VARIABLE_SUMMARY_STATS = list(
    NAMES = c(
      "Var Name", "Var Class", "Mean", "Median",
      "Min", "Max", "SD", "Obs", "Missing obs"
    ),
    DESIRED_VARS = c("effect", "se", "sample_size", "dof")
  ),
  EFFECT_SUMMARY_STATS = list(
    NAMES = c(
      "Var Name", "Var Class", "Mean", "CI lower", "CI upper", "Weighted Mean",
      "WM CI lower", "WM CI upper", "Median", "Min", "Max", "SD", "Obs"
    ),
    DESIRED_VARS = c("effect", "se", "sample_size", "dof")
  ),
  MOCKS = list(
    TMP_DATA_FILE_NAME = "tmp_data.csv",
    TMP_OPTIONS_FILE_NAME = "tmp_options.yaml",
    MOCK_DF_NROWS = 1000,
    MOCK_DF_NSTUDIES = 50,
    MOCK_DF_SEED = 123
  ),
  PATTERNS = list(
    YAML_FILES = list(
      PLAIN = c(".yaml"),
      REGEX = "\\.ya(ml|yml)$"
    )
  ),
  DEFAULT_VERBOSITY = 3,
  STYLES = list(
    OPTIONS = list(
      NAME = cli::col_magenta,
      VALUE = cli::col_green,
      TYPE = cli::col_cyan,
      DEFAULT = cli::col_yellow
    )
  ),
  RUNTIME_METHODS = list(
    EXECUTION_ORDER = c(
      "variable_summary_stats",
      "effect_summary_stats",
      "linear_tests",
      "nonlinear_tests",
      "exogeneity_tests",
      "p_hacking_tests",
      "bma"
    )
  )
)
