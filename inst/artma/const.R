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
    DATA_TYPES = c("dummy", "category", "int", "float", "perc", "empty", "unknown")
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
      DIRECTORY = "directory"
    ),
    SPECIAL_KEYS = c("description", "details", "type", "optional", "default", "values")
  ),
  MOCKS = list(
    TMP_DATA_FILE_NAME = "tmp_data.csv",
    TMP_OPTIONS_FILE_NAME = "tmp_options.yaml",
    MOCK_DF_NROWS = 1000,
    MOCK_DF_NSTUDIES = 50
  ),
  PATTERNS = list(
    YAML_FILES = list(
      PLAIN = c(".yaml"),
      REGEX = "\\.ya(ml|yml)$"
    )
  ),
  STYLES = list(
    OPTIONS = list(
      NAME = cli::col_magenta,
      VALUE = cli::col_green,
      TYPE = cli::col_cyan,
      DEFAULT = cli::col_yellow
    )
  )
)
