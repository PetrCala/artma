box::use(
  artma / paths[PATHS],
  artma / const[CONST]
)

#' Get the path to the logger file
#'
#' @param logger_name [character] The name of the logger
#' @return [character] The path to the logger file
#' @export
get_logger_path <- function(logger_name) {
  box::use(
    artma / libs / file_utils[ensure_folder_existence]
  )
  if (is.null(logger_name)) {
    rlang::abort("Logger name cannot be NULL")
  }
  ensure_folder_existence(PATHS$DIR_LOGS)
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
  logger_name <- getOption("artma.logging.log_file_name")
  log_level <- getOption("artma.logging.log_level")

  # Set the logging threshold based on the input string
  if (log_level %in% names(CONST$LOG_LEVEL_MAP)) {
    logger::log_threshold(CONST$LOG_LEVEL_MAP[[log_level]])
  } else {
    rlang::abort("Invalid log level specified. Choose from 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'.")
  }

  if (isTRUE(getOption("artma.logging.log_to_console"))) {
    logger::log_appender(logger::appender_console) # Enable console logging
  } else {
    logger::log_appender(NULL) # Disable console logging
  }

  # Set the file appender if a logger name is provided
  if (!is.null(logger_name)) {
    log_file <- get_logger_path(logger_name = logger_name)
    logger::log_appender(logger::appender_file(log_file, max_files = 1L), index = 2L)
  }

  if (getOption("artma.logging.flush_logs_on_setup")) {
    flush_log_files(logger_name = logger_name)
  }

  logger::log_debug("Logging setup complete")
}

#' Teardown the logger and remove the log file
#'
#' @param logger_name [character] The name of the logger to teardown
#' @export
teardown_logger_file <- function(logger_name) {
  flush_log_files(logger_name = logger_name)
}
