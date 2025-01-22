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
    "DEBUG" = logger::DEBUG,
    "INFO"  = logger::INFO,
    "WARN"  = logger::WARN,
    "ERROR" = logger::ERROR,
    "FATAL" = logger::FATAL
  ),

  # Config
  CONFIG_SPECIAL_KEYS = c("description", "details", "type", "optional", "default", "values")
)
