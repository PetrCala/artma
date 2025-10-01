#' @title Run linear publication bias tests
#' @description
#' Execute a suite of linear regression-based publication bias diagnostics
#' (OLS, fixed-effects, between-effects, random-effects, and weighted OLS
#' variants) on the supplied data frame. The function prepares the input
#' data, applies robust variance estimation, optionally performs clustered
#' bootstrap resampling for confidence intervals, and returns a tidy summary
#' of coefficients, standard errors, t-statistics, p-values, and formatted
#' presentation strings for each method.
linear_tests <- function(df) {
  box::use(
    artma / calc / linear_models[
      linear_model_specs,
      prepare_linear_model_data,
      run_linear_model_suite
    ],
    artma / libs / utils[get_verbosity],
    artma / libs / validation[assert, validate],
    artma / options / index[get_option_group]
  )

  validate(is.data.frame(df))

  specs <- linear_model_specs()
  verbosity <- get_verbosity()
  prepared_data <- prepare_linear_model_data(df, specs, verbosity = verbosity)

  opt <- get_option_group("artma.methods.linear_tests")

  conf_level <- opt$conf_level %||% 0.95
  add_marks <- if (is.null(opt$add_significance_marks)) TRUE else isTRUE(opt$add_significance_marks)
  bootstrap_replications <- opt$bootstrap_replications %||% 500
  verbose_option <- if (is.null(opt$verbose)) FALSE else isTRUE(opt$verbose)

  validate(
    is.numeric(conf_level), length(conf_level) == 1,
    is.logical(add_marks), length(add_marks) == 1,
    is.numeric(bootstrap_replications), length(bootstrap_replications) == 1,
    is.logical(verbose_option), length(verbose_option) == 1
  )

  assert(conf_level > 0 && conf_level < 1, "Confidence level must be between 0 and 1.")
  assert(bootstrap_replications >= 0, "Bootstrap replications must be greater than or equal to 0.")

  round_to <- getOption("artma.output.number_of_decimals", 3)
  validate(is.numeric(round_to), length(round_to) == 1)
  assert(round_to >= 0, "Number of decimals must be greater than or equal to 0.")

  emit_progress <- isTRUE(verbose_option) || verbosity >= 4

  results <- run_linear_model_suite(
    data = prepared_data,
    specs = specs,
    conf_level = conf_level,
    bootstrap_replications = bootstrap_replications,
    add_significance_marks = add_marks,
    round_to = round_to,
    verbosity = verbosity,
    emit_progress = emit_progress
  )

  if (verbosity >= 3) {
    cli::cli_h3("Linear test results")
    display <- results[, c("method", "term_label", "formatted_estimate", "formatted_std_error", "formatted_ci"), drop = FALSE]
    cli::cat_print(display)
  }

  results
}

box::use(
  artma / libs / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

run <- cache_cli_runner(
  linear_tests,
  stage = "linear_tests",
  key_builder = function(...) build_data_cache_signature()
)

box::export(run)
