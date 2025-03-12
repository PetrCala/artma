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
  OPTIONS_VALIDATION_ACTIONS = c(
    "abort_verbose",
    "abort_quiet",
    "return_errors_verbose",
    "return_errors_quiet"
  ),

  # Logger
  LOG_PATTERN = "^(INFO|WARN|ERROR|DEBUG|FATAL)\\s+\\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\]\\s*",
  LOG_LEVEL_MAP = list(
    DEBUG = logger::DEBUG,
    INFO  = logger::INFO,
    WARN  = logger::WARN,
    ERROR = logger::ERROR,
    FATAL = logger::FATAL
  ),

  # Config
  CONFIG_SPECIAL_KEYS = c("description", "details", "type", "optional", "default", "values"),

  # Regex
  REGEX = list(
    OPTIONS_FILE_SUFFIX = "\\.yaml|\\.yml"
  )
)
