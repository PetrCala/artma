# nolint start: r6_usage_linter.

# box::use(
#   testthat[ProgressReporter]
# )

#' @export
custom_reporter <- R6::R6Class(
  "custom_reporter",
  inherit = testthat::Reporter,
  public = list(
    start_file = function(name) {
      cli::cli_inform(name, " | ")
    },
    end_file = function() {
      cli::cat_line()
    },
    start_test = function(context, test) {
      base::invisible()
    },
    end_test = function(context, test) {
      if (self$skipped()) {
        cli::cli_inform(crayon::yellow("S"))
      } else if (self$failed()) {
        cli::cli_inform(crayon::red("F"))
      } else if (self$warning()) {
        cli::cli_inform(crayon::yellow("W"))
      } else {
        cli::cli_inform(crayon::green("."))
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
        cli::cli_inform(cli::symbol$line)
        self$n_skip <- self$n_skip + 1
      } else if (inherits(result, "expectation_success")) {
        cli::cli_inform(".")
        self$n_ok <- self$n_ok + 1
      } else if (inherits(result, "expectation_failure")) {
        cli::cli_inform("F")
        self$n_fail <- self$n_fail + 1
      } else if (inherits(result, "expectation_warning")) {
        cli::cli_inform("W")
        self$n_warn <- self$n_warn + 1
      } else if (inherits(result, "expectation_error")) {
        cli::cli_inform("E")
        self$n_fail <- self$n_fail + 1
      }
      invisible()
    },
    end_reporter = function() {
      cli::cat_line()
      if (self$n_fail > 0) {
        cli::cli_inform(cli::rule("Failures", line = 2), "\n")
      }
      if (self$n_ok > 0) {
        cli::cli_inform(cli::col_green(paste0(self$n_ok, " tests passed")), "\n")
      }
      if (self$n_fail > 0) {
        cli::cli_inform(cli::col_red(paste0(self$n_fail, " tests failed")), "\n")
      }
      if (self$n_skip > 0) {
        cli::cli_inform(cli::col_yellow(paste0(self$n_skip, " tests skipped")), "\n")
      }
      if (self$n_warn > 0) {
        cli::cli_inform(cli::col_yellow(paste0(self$n_warn, " tests with warnings")), "\n")
      }
    }
  )
)

# nolint end: r6_usage_linter.
