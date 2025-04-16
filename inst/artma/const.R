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
    )
  ),
  CONFIG_SPECIAL_KEYS = c("description", "details", "type", "optional", "default", "values"),
  MOCKS = list(
    TMP_DATA_FILE_NAME = "tmp_data.csv",
    TMP_OPTIONS_FILE_NAME = "tmp_options.yaml"
  ),
  REGEX = list(
    OPTIONS_FILE_SUFFIX = "\\.yaml|\\.yml"
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
