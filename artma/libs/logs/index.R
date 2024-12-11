box::use(
  base / paths[PATHS],
  base / const[CONST],
  base / options[get_option],
)

#' Get the path to the logger file
#'
#' @param logger_name [character] The name of the logger
#' @return [character] The path to the logger file
#' @export
get_logger_path <- function(logger_name) {
  if (is.null(logger_name)) {
    rlang::abort("Logger name cannot be NULL")
  }
  if (!dir.exists(PATHS$DIR_LOGS)) {
    dir.create(PATHS$DIR_LOGS)
  }
  file.path(PATHS$DIR_LOGS, logger_name)
}

#' Flush all log files in the logs directory
#'
#' @param logger_name [character] The name of the logger to flush, if only one should be flushed.
#' @export
flush_log_files <- function(logger_name = NULL) {
  for (file in list.files(PATHS$DIR_LOGS)) {
    if (!is.null(logger_name) && file != logger_name) {
      next
    }
    logger_path <- get_logger_path(logger_name = file)
    if (file.exists(logger_path)) {
      file.remove(logger_path)
    }
  }
}

#' Setup logging for the project
#'
#' @export
setup_logging <- function() {
  log_to_console_only <- get_option("general.log_to_console_only")
  logger_name <- get_option("general.log_file_name")
  log_level <- get_option("dynamic_options.log_level")

  # Set the logging threshold based on the input string
  if (log_level %in% names(CONST$LOG_LEVEL_MAP)) {
    logger::log_threshold(CONST$LOG_LEVEL_MAP[[log_level]])
  } else {
    rlang::abort("Invalid log level specified. Choose from 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'.")
  }

  logger::log_appender(logger::appender_console) # Console logger

  # Set the file appender if logging to file is enabled
  if (!log_to_console_only && !is.null(logger_name)) {
    log_file <- get_logger_path(logger_name = logger_name)
    logger::log_appender(logger::appender_file(log_file, max_files = 1L), index = 2)
  }

  if (get_option("general.log_flush_on_setup")) {
    flush_log_files(logger_name = logger_name)
  }
}

#' Teardown the logger and remove the log file
#'
#' @param logger_name [character] The name of the logger to teardown
#' @export
teardown_logger_file <- function(logger_name) {
  # logger::remove_appender(logger_name) # This does not work
  flush_log_files(logger_name = logger_name)
}
