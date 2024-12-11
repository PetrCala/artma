# box::use(
#   testthat[ProgressReporter]
# )

#' @export
custom_reporter <- R6::R6Class(
  "custom_reporter",
  inherit = testthat::Reporter,
  public = list(
    start_file = function(name) {
      base::cat(name, " | ")
    },
    end_file = function() {
      base::cat("\n")
    },
    start_test = function(context, test) {
      base::invisible()
    },
    end_test = function(context, test) {
      if (self$skipped()) {
        base::cat(crayon::yellow("S"))
      } else if (self$failed()) {
        base::cat(crayon::red("F"))
      } else if (self$warning()) {
        base::cat(crayon::yellow("W"))
      } else {
        base::cat(crayon::green("."))
      }
    },
    add_result = function(context, test, result) {
      if (!is.list(self$results[[context]])) {
        self$results[[context]] <- list()
      }
      self$results[[context]][[test]] <- result
    },
    get_results = function() {
      self$results
    }
  ),
  private = list(
    results = list()
  )
)




#' A dot based reporter
#' @example
#' testthat::test_dir("path/to/tests", reporter = DOT_REPORTER)
#' @export
dot_reporter <- R6::R6Class(
  "DotReporter",
  inherit = testthat::ProgressReporter,
  public = list(
    add_result = function(context, test, result) {
      if (inherits(result, "expectation_skip")) {
        cat(cli::symbol$line)
        self$n_skip <- self$n_skip + 1
      } else if (inherits(result, "expectation_success")) {
        cat(".")
        self$n_ok <- self$n_ok + 1
      } else if (inherits(result, "expectation_failure")) {
        cat("F")
        self$n_fail <- self$n_fail + 1
      } else if (inherits(result, "expectation_warning")) {
        cat("W")
        self$n_warn <- self$n_warn + 1
      } else if (inherits(result, "expectation_error")) {
        cat("E")
        self$n_fail <- self$n_fail + 1
      }
      invisible()
    },
    end_reporter = function() {
      cat("\n")
      if (self$n_fail > 0) {
        cat(cli::rule("Failures", line = 2), "\n")
      }
      if (self$n_ok > 0) {
        cat(cli::col_green(paste0(self$n_ok, " tests passed")), "\n")
      }
      if (self$n_fail > 0) {
        cat(cli::col_red(paste0(self$n_fail, " tests failed")), "\n")
      }
      if (self$n_skip > 0) {
        cat(cli::col_yellow(paste0(self$n_skip, " tests skipped")), "\n")
      }
      if (self$n_warn > 0) {
        cat(cli::col_yellow(paste0(self$n_warn, " tests with warnings")), "\n")
      }
    }
  )
)
