# P-Hacking Tests Integration TODO

## Current Status

The p-hacking tests method has been partially implemented with:
- ✅ Elliott et al. (2022) tests fully integrated
- ✅ Caliper tests (Gerber & Malhotra, 2008) functions added to library
- ⚠️  MAIVE estimator needs full integration

## What's Already Done

### Files Created/Modified
1. `inst/artma/libs/p_hacking_tests.R` - Library with:
   - `run_single_caliper()` - Single caliper test implementation
   - `run_caliper_tests()` - Multiple threshold/width caliper tests
   - Elliott test wrappers (binomial, LCM, Fisher, discontinuity, Cox-Shi)
   - MAIVE imported from `artma/calc/methods/maive`

2. `inst/artma/methods/p_hacking_tests.R` - Runtime method interface

3. `inst/artma/const.R` - Added to execution order

4. `DESCRIPTION` - Added fdrtool, quadprog, rddensity dependencies

5. `inst/artma/options/templates/options_template.yaml` - Elliott test options

## What Needs To Be Done

### 1. Complete Main Runner Function

The `run_p_hacking_tests()` function needs to be updated to return results from all three test types:

```r
run_p_hacking_tests <- function(df, options) {
  # Current: Only runs Elliott tests
  # Needed: Run all three test suites and aggregate results

  results <- list()

  # 1. Caliper Tests
  if (options$include_caliper) {
    t_stats <- df$effect / df$se
    caliper_results <- run_caliper_tests(
      t_stats = t_stats,
      study_id = study_id,
      thresholds = options$caliper_thresholds,
      widths = options$caliper_widths,
      add_significance_marks = options$add_significance_marks,
      round_to = options$round_to
    )
    results$caliper <- build_caliper_summary(caliper_results, options)
  }

  # 2. Elliott Tests (already implemented)
  if (options$include_elliott) {
    # ... existing Elliott test code ...
    results$elliott <- elliott_summary
  }

  # 3. MAIVE Estimator
  if (options$include_maive) {
    maive_data <- prepare_maive_data(df)
    maive_results <- tryCatch(
      maive(
        dat = maive_data,
        method = options$maive_method,
        weight = options$maive_weight,
        instrument = options$maive_instrument,
        studylevel = options$maive_studylevel
      ),
      error = function(e) NULL
    )
    results$maive <- format_maive_results(maive_results, options)
  }

  # Build comprehensive summary
  summary <- build_comprehensive_summary(results, options)

  list(
    caliper = results$caliper,
    elliott = results$elliott,
    maive = results$maive,
    summary = summary,
    options = options
  )
}
```

### 2. Add Helper Functions

Need to add these functions to `inst/artma/libs/p_hacking_tests.R`:

```r
#' @title Prepare data for MAIVE
prepare_maive_data <- function(df) {
  # Convert artma format to MAIVE format
  # Columns: bs (effect), sebs (se), Ns (n_obs), studyid (study_id)
}

#' @title Build caliper summary table
build_caliper_summary <- function(caliper_results, options) {
  # Convert list of caliper results to formatted data frame
}

#' @title Format MAIVE results
format_maive_results <- function(maive_output, options) {
  # Extract and format MAIVE coefficients, F-test, Hausman test
}

#' @title Build comprehensive p-hacking summary
build_comprehensive_summary <- function(results, options) {
  # Combine summaries from all three test types into one table
}
```

### 3. Update Options Template

Add to `inst/artma/options/templates/options_template.yaml`:

```yaml
  p_hacking_tests:
    # Caliper options
    include_caliper:
      type: "logical"
      default: true
      help: |
        If `TRUE`, include Caliper tests for selective reporting around thresholds.

    caliper_thresholds:
      type: "numeric"
      default: [0, 1.96, 2.58]
      help: |
        T-statistic thresholds to test with Caliper method.

    caliper_widths:
      type: "numeric"
      default: [0.05, 0.1, 0.2]
      help: |
        Caliper interval widths to test.

    # Elliott options (already added)
    include_elliott:
      type: "logical"
      default: true
      help: |
        If `TRUE`, include Elliott et al. (2022) p-hacking tests.

    # ... existing Elliott options ...

    # MAIVE options
    include_maive:
      type: "logical"
      default: true
      help: |
        If `TRUE`, include MAIVE estimator (Irsova et al., 2023).

    maive_method:
      type: "enum: 1|2|3|4"
      default: 3
      help: |
        MAIVE method: 1=FAT-PET, 2=PEESE, 3=PET-PEESE, 4=EK.

    maive_weight:
      type: "enum: 0|1|2"
      default: 0
      help: |
        MAIVE weighting: 0=no weights, 1=standard, 2=adjusted.

    maive_instrument:
      type: "enum: 0|1"
      default: 1
      help: |
        MAIVE instrumenting: 0=no, 1=yes.

    maive_studylevel:
      type: "enum: 0|1|2"
      default: 2
      help: |
        MAIVE study-level correlation: 0=none, 1=fixed effects, 2=cluster.
```

### 4. Update Runtime Method

Update `inst/artma/methods/p_hacking_tests.R` to handle options for all three test types and display comprehensive output:

```r
# Extract options for all three test types
caliper_opts <- list(
  include_caliper = opt$include_caliper %||% TRUE,
  caliper_thresholds = opt$caliper_thresholds %||% c(0, 1.96, 2.58),
  caliper_widths = opt$caliper_widths %||% c(0.05, 0.1, 0.2)
)

elliott_opts <- list(
  # ... existing ...
)

maive_opts <- list(
  include_maive = opt$include_maive %||% TRUE,
  maive_method = opt$maive_method %||% 3,
  maive_weight = opt$maive_weight %||% 0,
  maive_instrument = opt$maive_instrument %||% 1,
  maive_studylevel = opt$maive_studylevel %||% 2
)

# Display results for each test type
if (verbosity >= 1) {
  cli::cli_h2("P-hacking tests")

  if (!is.null(results$caliper)) {
    cli::cli_h3("Caliper Tests")
    # Display caliper table
  }

  if (!is.null(results$elliott)) {
    cli::cli_h3("Elliott Tests")
    # Display elliott table
  }

  if (!is.null(results$maive)) {
    cli::cli_h3("MAIVE Estimator")
    # Display MAIVE results
  }
}
```

## Testing

After completing integration:
1. Run `make lint` to ensure code style
2. Run `make test` to verify all tests pass
3. Test with real data to ensure all three test types produce reasonable output
4. Check that options are properly read and applied

## References

- Gerber, A. S., & Malhotra, N. (2008). Do statistical reporting standards affect what is published? Publication bias in two leading political science journals. *Quarterly Journal of Political Science*, 3(3), 313-326.
- Elliott, G., Kudrin, N., & Wüthrich, K. (2022). Detecting p-hacking. *Econometrica*, 90(2), 887-906.
- Irsova, Z., Doucouliagos, H., & Stanley, T. D. (2023). Meta-analysis of social science research: A practitioner's guide. *Journal of Economic Surveys*.
