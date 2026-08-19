
<a name="v0.4.0"></a>

## [v0.4.0](https://github.com/PetrCala/artma/compare/v0.3.5...v0.4.0)

> 2026-08-19

### Bug Fixes

* keep lintr in Suggests, it is used by the linter unit tests
* drop LazyData and dead Suggests, honour absolute graphics export path
* **bma:** drop non-numeric moderators instead of failing inside scale()
* **bpe:** gate the "run BMA first?" prompt on the autonomy level
* **data:** derive study_label from a legible study name column when study keys are numeric
* **data:** write mock data to a durable user dir instead of tempfile
* **exogeneity:** default the IV instrument to 1/sqrt(n_obs) ([#450](https://github.com/PetrCala/artma/issues/450))
* **linear:** make between-effects standard errors robust and label every model's vcov
* **methods:** keep sequential method output on the console so prompts show
* **output:** add a closing run summary and stop reading integers as percentages
* **output:** resolve absolute graphics path in writers and escape literal braces in help
* **output:** normalise manifest paths to forward slashes for Windows
* **output:** drop zero counts from the run summary and winsorization notes
* **report:** split method skip contract into skip_reason and skipped_models

### Features

* **api:** rename cli.* to cli_* and keep cli.run as a permanent alias
* **api:** rename autonomy, viz, methods, report, results exports to snake_case
* **api:** rename config.* and data.preview with deprecated aliases
* **api:** rename options.* exports to options_* with deprecated aliases
* **api:** add lifecycle dependency and snake_case API naming guide
* **core:** add leveled verbosity log wrappers, fix drifted defaults
* **methods:** show the registered method metadata in methods.list()
* **options:** make options discoverable via help tree, list details and diff
* **output:** write a per-run run.json and drive the report and CLI from it
* **output:** roll the estimates slot out across the remaining runtime methods
* **output:** add an unrounded tidy estimates slot to the method contract


<a name="v0.3.5"></a>

## [v0.3.5](https://github.com/PetrCala/artma/compare/v0.3.3...v0.3.5)

> 2026-08-09

### Bug Fixes

* replace the homemade digest hash and stop the data cache invalidating itself
* align LCM defaults and wire chunk_size option in p_hacking_tests
* let column config data_type override type detection ([#253](https://github.com/PetrCala/artma/issues/253))
* stop false study_id warning on valid string labels ([#250](https://github.com/PetrCala/artma/issues/250))
* declare rjags in Suggests for the RoBMA test guard
* target the RoBMA 4.x API in the robma method
* make data config keys deterministic and invalidate the config df cache
* update the climenu version to 0.1.5 to avoid an error
* handle NULL var_name in config during column validation
* silence the bma and fma run if the verbose_output is FALSE
* prime data-config cache when data is passed directly to artma()
* quiet dependency and validation noise in real runs and logs
* requirements on bma-dependent methods
* strict bpe requirements
* schema reconcile
* keep MAIVE pin CRAN-resolvable and gate tests on the runtime floor
* require MAIVE 0.2.5 for unrounded estimates
* install MAIVE from CRAN instead of GitHub
* schema reconciliation messages
* make BPE recommendation fallback an explicit no-recommendation outcome
* declare runtime dependencies correctly
* locate linters.R without the here package
* recompute user-supplied precision after winsorization
* locate the package root when the checkout is not named artma
* implement puniform_method P (method of moments) branch
* correct FE intercept SE vcov and study-weighted OLS weights in linear tests
* use inverse-variance WAAP pilot weights and clustered WLS standard errors
* isolate runtime method failures and always export partial results
* quoted data paths, duplicate column mapping, exogeneity table gaps
* prevent bma/best_practice_estimate crash on small BMA models
* failing tests
* options correctness fixes and dead code removal
* draw mock standard errors from a strictly positive range
* honor the cache TTL option and add a template-to-code option parity test
* replace literal unicode escape text with ascii equivalents
* force sequential execution when already inside a forked worker
* restore RoxygenNote 7.3.3 and man page dropped by roxygen 8 regeneration
* drop unused fs Suggests dependency and its dir_create lint rule
* **bma:** align VIF masks, split multi-model params, keep unmatched regressor names
* **bma:** exclude derived effect/se encodings from auto-selected moderators
* **bma:** skip gracefully instead of crashing with a single moderator
* **bpe:** apply overrides and report estimates on the raw effect scale
* **bpe:** auto-detect dummy predictors for factor summary table
* **cache:** record base-graphics plots so cached reruns restore them
* **ci:** quote E2E test-file array so both fixture tests actually run
* **data:** abort on colliding column-rename targets in standardize_column_names
* **data:** treat NA options as unset and drop non-numeric required NAs under remove
* **data:** silence duplicate drop_conflicting_raw log in configure phase
* **data:** stop strategy no longer aborts on optional-column NAs
* **data:** only hard-require columns the requested methods need
* **data:** guard imputation of mostly-missing columns and drop constant BMA moderators
* **data:** import get_verbosity in standardize_column_names, update stale test calls
* **data:** stop treating lowercase na/n-a strings as missing values
* **data:** default zero-SE handling to remove-with-warning instead of abort
* **detection:** detect dummy/categorical type even with group_category set
* **e2e:** shell-quote CLI args so the Rscript subprocess starts under sh
* **effect-summary-stats:** correct weighted-mean SE, per-study weights, unrounded CI
* **effect_summary_stats:** default effect column when nothing configured
* **elliott:** use integer knot indices in pure R CDF simulation fallback
* **endo-kink:** use model df, not residual df, in heterogeneity variance divisor ([#371](https://github.com/PetrCala/artma/issues/371))
* **exogeneity:** use se^2 as p-uniform* study variance and fix selection threshold
* **exogeneity:** fill formatted columns when p-uniform* skips estimation
* **exogeneity:** stop p-uniform* LRT collapsing to L=0, fall back to P on ML failure
* **exogeneity:** rank IV instruments by first-stage F, drop backwards votes
* **exogeneity:** cluster IV diagnostics and AR test, signed p-uniform search, true bias test
* **fma:** correct Mallows criterion, Buckland SE centering, and data scaling
* **formatting:** stop wide summary tables splitting into two blocks
* **formatting:** apply wide-table print fix to remaining call sites
* **linear:** raise bootstrap replications and flag CI/star disagreement
* **maive:** guardrail AR interval when constant N disables instrument
* **maive:** align se option with MAIVE 0.2.4, report effective spec and AR guardrails
* **maive:** resolve add_significance_marks via shared helper restoring scalar check
* **maive:** flag weak first-stage instrument and document maive_first_stage
* **modules:** newline-terminate captured cli messages, NA-safe parallel option read
* **nonlinear:** skip degenerate corner-solution fits instead of tabulating them
* **nonlinear:** allow general selection cutoffs, cluster SEs by study, report pub probabilities
* **options:** flatten nested user_input on create, honor mock input, fix CONST scope
* **options:** merge nested user_input safely and validate before overwrite
* **options:** normalize .yaml suffix when loading options file
* **options:** keep the unified column store when migrating legacy options
* **options:** stop options rewrites from dropping column records and value shapes
* **output:** plain ellipsis, no raw NA cells, label standardized BPE values
* **output:** resolve auto output dir per options file and never persist it
* **p-hacking:** orient caliper tail, cluster by study, show direction
* **p-hacking:** rewrite Cox-Shi against canonical Elliott et al. implementation
* **p_hacking:** respecify caliper test as Gerber-Malhotra binomial test
* **p_hacking_tests:** seed LCM/MAIVE, real cpp fallback, skip reasons
* **p_hacking_tests:** quick wins from p-hacking suite review ([#257](https://github.com/PetrCala/artma/issues/257))
* **p_hacking_tests:** keep install/upgrade hints in MAIVE skip reason
* **preview:** skip utils::View() when no data viewer is available
* **rng:** strip BMS wall-clock reseed and key method RNG streams by name
* **run:** cap forked workers at the environment's process limit
* **run:** pin BLAS/OpenMP to one thread while forking method layers
* **run:** use a fork-safe graphics device so parallel runs can export plots
* **stem,robma:** cluster RoBMA by study, align stem MSE table and plot rows
* **tests:** probe an actual JAGS model fit to catch macOS oldrel crash
* **tests:** avoid rjags segfault in robma JAGS probe on CI
* **tests:** probe rjags for JAGS in a subprocess to avoid CI crash
* **variable_summary_stats:** obs column counts non-missing observations
* **vcov:** warn on silent clustering downgrades; sign-correct zero-SE t-stats
* **vcov:** warn on no-cluster single-step downgrade to stats::vcov()
* **viz:** skip plot previews in forked workers to avoid quartz fork crash
* **viz:** probe the cairo device instead of trusting capabilities()

### Code Refactoring

* add shared method registration helper and standard return contract
* make options.load pure and parse the options template once
* box plot max boxes per plot option
* simplify non-idiomatic constructs and reuse significance_mark
* unify bma and fma into a single table
* handle data columns through reconciliation
* replace the cli replay cache layer with plain memoise
* unify file-read dispatch and post-read normalization
* collapse the 5-level autonomy system into a 3-value enum
* vectorize row-wise loops and hoist repeated computation
* unify required, reserved, and computed column name lists
* declarative method metadata replacing execution order and dependency plumbing
* unify weak-instrument F threshold constant in exogeneity.R
* strict option access and fail-loud coercion
* split prepare_data into configure, compute, and persist phases
* consolidate R CMD check workarounds into one generated manifest
* unify column mapping and data config into one per-column store
* extract shared helpers for duplicated logic
* remove dead functions, unused imports and unreachable branches
* restore stats imports used via formula environments, with comment
* **bma:** extract shared unwrap_bma_result from bpe and fma
* **bpe:** split best_practice_estimate into econometric core and viz plots
* **data:** drop dead interactive branch, move column mapping persistence
* **data:** split schema_reconcile into detect, ui, persist layers
* **data:** extract shared column profiler into data/profile.R
* **data:** move na/se-zero configure resolvers into data/configure.R
* **econometric:** route four vcov ladders through shared robust_vcov
* **elliott:** make C++ CDF simulation the sole production path
* **formatting:** consolidate nonlinear formatting helpers into shared modules
* **formatting:** share one summary-table builder across econometric tests
* **methods:** extract shared resolve_variable_groups equal/gltl resolver
* **options:** add spec-driven option resolver and migrate method files
* **options:** template-sourced typed option accessors
* **options:** share one option type registry across coerce and validate
* **visualization:** unify plot export and tick-interval math
* **visualization:** drop ggtext, use plain element_text for ticks

### Features

* add opt-in RoBMA runtime method
* add bpe
* add BMA prior-comparison plot for multiple parameter sets
* attach STEM funnel and MSE plots to nonlinear_tests output
* support user-defined data subset conditions
* add economic significance table to best_practice_estimate
* add BPE per-factor density and sorted scatter plots
* add BPE summary stats grouped by factor levels
* **bma:** announce auto-included priority moderators during variable selection
* **cache:** harden cache signature and enable caching by default
* **cli:** add scriptable command-line interface with cli.run and cli.install
* **coverage:** measure inst/artma box modules, upload merged report to codecov
* **data:** reconcile column-mapping conflicts and give actionable collision errors
* **fma:** add cluster-robust standard errors clustered by study_id
* **funnel_plot:** report filtered observation count in subtitle and output
* **hooks:** add local git hooks for style/lint and commit-msg checks
* **linear:** precheck panel specs and skip with plain-language reasons
* **linear:** enable bootstrap CIs for the between effects spec
* **maive:** add automatic first-stage selection from sample-size spread
* **maive:** split MAIVE into its own method with sectioned, annotated output
* **nonlinear:** report effect heterogeneity (tau) for STEM, Hierarchical, and Endogenous Kink
* **output:** add LaTeX table export via output.table_formats
* **p-hacking:** make Elliott supports configurable and surface Cox-Shi skip reasons ([#369](https://github.com/PetrCala/artma/issues/369))
* **p_hacking:** raise default LCM iterations to 10000
* **report:** add self-contained HTML report renderer via report.render()
* **robma:** enable parallel MCMC chains when cores allow
* **run:** run independent runtime methods in parallel layers
* **visualization:** add optional interactive HTML plot export

### Performance Improvements

* **bpe:** split study row indices once instead of rescanning per study
* **elliott:** share pair lambdas across Cox-Shi bound orders
* **funnel_plot:** split rows by study once in aggregate_study_medians
* **linear:** fast-path random effects bootstrap via Swamy-Arora replication
* **linear:** fast-path bootstrap estimates without per-replication plm/vcov
* **linear:** skip vcov math and trim columns in bootstrap replications
* **linear:** hoist cluster split out of bootstrap loop
* **methods:** cache only expensive stages via cached opt-in on register_runtime_method
* **p_hacking_tests:** cache LCM critical-value simulation under a narrow key
* **prima_facie_graphs:** vectorize group column and median split construction
* **stem:** replace O(n^2) outer-product submatrix sums with cumsum
* **visualization:** use ragg for faster PNG export when available


<a name="v0.3.3"></a>

## [v0.3.3](https://github.com/PetrCala/artma/compare/v0.3.2...v0.3.3)

> 2026-02-11

### Bug Fixes

* parallel cdfs workers function in the elliott model calculations
* add missing global variables
* description versino control
* failing funnel plot tests
* duplicate data reading during the main pipeline
* attempt to remove extra printed rows
* recover stale options files with template defaults
* box imports around the elliott cpp functions
* maive output formatting issue
* set the artma open results default to false
* remaining lints in the package
* lints around the project
* lint setup
* elliott tests
* elliott options
* results.open folder path
* fma option template duplicates
* r cmd check notes/warnings
* box_plot detection
* failing tests
* add proper visualization text to gg plots
* return the main results of the artma method silently
* mice algorithm na handling
* pressing enter upon options value ask leads to file/folder choice
* box plot rendering, autonomy level detection
* update the methods return objects to print the output into console cleanly
* test example lib imports
* lints
* remove failing tests
* missing success print; create an options file if none exist when necessary
* the interactive columns mapping now works
* interactive mapping column selection
* data reading during data preprocessing
* more lint issues
* indentation issues
* ensure the system editors are available for editor opening
* options edge cases
* add computed columns to data config and suppress misleading warnings
* use full path when saving preferences to options file
* automatically remove aliased variables during BMA analysis
* display variable reason correctly in suggestion output
* correct data config import and add integration tests
* remove automatic addition of 'se' to BMA variables
* only exclude effect variable from BMA selection
* update bma coefficient extraction
* update the way bma objects are validated upon input
* update the maive package reference
* move AER and ivmodel from Imports to Suggests
* handle vector values in option NA validation

### Code Refactoring

* make study ID column detection smarter
* add a parallel cdfs computation
* make the data config option sparse
* unify and clean up visualization logic
* unify study_id vs. study usage
* clean up the autonomy system
* make the follow-up user choice prompts respect autonomy
* clean up the package structure
* redirect user to r universe links in more help
* unify the invocation public api methods to 'artma'
* the data source is now read in two-steps to enable column mapping
* add better whitespace na handling when reading data
* the na handling method now distinguishes between numeric and non-numeric columns
* updated column position verification
* the data preprocessing now removes empty columns based on quality, not position
* simplify editor detection
* change the maive installation to use the CRAN version
* the bma variables selection is now saved to the data config node
* modify bma variable selection; add prompts
* add explanatory messages and fully silence clubSandwich warning
* improve progress messages and silence clubSandwich warning
* add Caliper tests to p-hacking method (partial integration)
* remove 5 unnecessary dependencies with custom polyfills

### Features

* automatically open latest results through results.open by default
* add prima-facie graphs
* add rcpp, elliott cdfs faster calculation
* persist the temporary results directories in the user option files
* add a unified results handling
* add FMA
* add a t-stat histogram runtime method
* add viz public methods
* add a data.preview function
* add funnel plot
* add visualization module; box-plot
* add help suppression for some option prompts
* add the autonomy system
* add a welcome message
* implement the select all option of the climenu
* add a public method options.delete
* add options vignette, improve the ask/utils functions
* allow the user to use a mock source DF
* add auto columns detection
* add better column type detection
* add functionality for choosing a suitable editor
* enhance optional column computation with comprehensive auto-calculation
* add reusable save preference prompt for configuration choices
* add comprehensive missing value handling system with smart prompts
* add collinearity detection and priority mechanism for BMA variable suggestions
* implement automatic variable suggestion for BMA
* add interactive variable selection for effect summary stats
* add make stats command for repository statistics
* implement automatic variable suggestion for effect summary stats
* implement interactive BMA variable selection
* implement Bayesian Model Averaging (BMA) method
* add winsorization to data preprocessing
* add caliper_display_ratios option for Caliper tests
* enhance p-hacking tests output formatting
* integrate MAIVE package from GitHub
* integrate Caliper, Elliott, and MAIVE as unified p-hacking tests
* add p-hacking tests as a new runtime method

### Performance Improvements

* add progress bars and fix hash overflow warnings


<a name="v0.3.2"></a>

## [v0.3.2](https://github.com/PetrCala/artma/compare/v0.3.1...v0.3.2)

> 2025-10-07

### Bug Fixes

* disable auto-detection in standardize_column_names tests
* reduce false positives in column recognition and fix options flow

### Features

* add exogeneity tests as a new runtime method
* add a smart recognition system for input column names
* add intelligent data reading and column recognition


<a name="v0.3.1"></a>

## [v0.3.1](https://github.com/PetrCala/artma/compare/v0.3.0...v0.3.1)

> 2025-10-06

### Bug Fixes

* rewrite the menu select usage to reference the climenu package correctly
* udpate the fix_options.R script
* resolve linter warnings and style issues


<a name="v0.3.0"></a>

## [v0.3.0](https://github.com/PetrCala/artma/compare/v0.2.2...v0.3.0)

> 2025-10-06

### Bug Fixes

* add Makefile and related files to .Rbuildignore
* improve keyboard input handling in climenu

### Code Refactoring

* replace utils::select.list with climenu::menu

### Features

* replace run.sh with Makefile for build tasks
* add climenu sub-package for interactive CLI menus


<a name="v0.2.2"></a>

## [v0.2.2](https://github.com/PetrCala/artma/compare/v0.2.1...v0.2.2)

> 2025-10-01

### Bug Fixes

* satisfy R CMD check warnings
* the p-value significance mark formatting is now more robust
* tidy linear tests output table formatting
* align linear tests runtime behaviour
* update a number of custom methods and their tests
* isolate cache keys per runtime stage
* update broken code in the effect and summary stats parts
* update modules handling, add a function for finding the package path
* the run module now handles input better
* update options file behavior in edge cases
* enforce guard clause indentation width
* satisfy lint restrictions in cache helpers
* silence ttl refresh replay noise
* harden cache invalidation handling
* drop stale memoise entries per invocation
* use cli default handler fallback
* remove non-ascii characters

### Code Refactoring

* unify significance marks option
* fix the linear tests output
* update the rest of the methods modules
* finish the custom linear/nonlinear methods migration
* fix lint issues and test suite issues for the custom methods
* port legacy methods with regression tests
* use cache runner for effect stats
* update path handling
* move module functions to a standalone folder
* move the runtime method fetch function to a standalone modules folder
* modularize options handling
* add linters to tests, refactor fixtures
* wrap all verbosity fetching into a single function
* single out the function for printing options help text

### Features

* add non-linear publication bias diagnostics
* allow configuring runtime method order
* add linear regression diagnostics method
* add reusable cache_cli runner wrappers
* utilize the new caching system to cache data preprocessing
* harden cache cli system
* add a cli capture handler for print output
* add new methods, fix caching, add optional cache disabling
* add an explicit wrapper function for fetching cached artifacts
* implement the custom caching functions and add test suites
* add cache packages and test suites
* add caching to the variable summary stats function
* add a caching system
* add an option to select how many decimal places to round the output to
* control for verbosity in all of the existing cli logs
* add the option to choose verbosity
* implement the variable summary stats method


<a name="v0.2.1"></a>

## [v0.2.1](https://github.com/PetrCala/artma/compare/v0.2.0...v0.2.1)

> 2025-04-25

### Bug Fixes

* add missing explicit package imports back to the DESCRIPTION file
* fix more data preprocessing import errors
* fix an import error
* add a couple of objects to the globals list

### Code Refactoring

* standardize the column names of required and expected variable names
* rewrite data processing into a pipeline

### Features

* add valiadtion and value enforcement methods for data preprocessing
* rename reg_df -> reg_dof
* improve option parsing validation and type coercion
* add a utility function for calculating study sizes
* add a calc placeholder module and calculation for t-values
* add methods for computing study/obs ID optional columns
* in data processing, enforce the correct column types and names better
* deprecate the column standardization
* add further column validation to data preprocessing
* add preprocessing methods for column name standardization
* add the purrr package, add utility functions for parsing data
* add a function for fetching definitions from option templates
* add the 'study' column to the data recognized column names


<a name="v0.2.0"></a>

## [v0.2.0](https://github.com/PetrCala/artma/compare/v0.1.30...v0.2.0)

> 2025-04-24

### Bug Fixes

* fix the data config top level module import error
* parsing option templates is now done through leaf paths
* remove an unimported function
* add a missing CONST import to the options utils module
* add a missing argument to the main function
* parsing the user options now properly displays help, modify the template to accommodate that
* options now accept enum types

### Code Refactoring

* change the parsing of the options template to allow nested nodes
* change the way the data config parsing function is structured
* the 'assert' function now accepts a 'msg' argument

### Features

* partially enable data preprocessing, clean up cli messages
* add functions for modifying, fixing, and fetching data configurations
* add temporary options for better path awareness
* implement a function to get a data config from the options file
* prompts can now be defined through custom options
* remove the unused data config methods - update through opt files now

### BREAKING CHANGE


the new templates are now incompatible with old ones


<a name="v0.1.30"></a>

## [v0.1.30](https://github.com/PetrCala/artma/compare/v0.1.29...v0.1.30)

> 2025-04-16

### Bug Fixes

* add a missing import to the data config parsing function
* add a missing export from the string module
* rewrite the data config validation test to use the renamed function

### Code Refactoring

* rename the determine df type function (data_type -> df_type)
* rename the data config file update function to better reflect its purpose
* move the data config write function to a standalone module
* decouple logic in data config writing/parsing
* regex validation now happens against a more robust const object, add tests
* rename the data config filename validation function
* replace the internal 'is_empty' function with the in-built rlang function
* remove the test constant and move the data into CONST.R

### Features

* add the body of data config parsing, add verbose string creation function
* add various utility functions for parsing and writing the data config files
* by default, the options copy now asks for overwrite permission in interactive mode
* add functions for parsing data frames into data configs
* add a function that validates an existence of a data config file
* add a function for validating data config filenames
* add a placeholder folder for data config files handling
* remove the logger package, use 'cli' for all console logging


<a name="v0.1.29"></a>

## [v0.1.29](https://github.com/PetrCala/artma/compare/v0.1.28...v0.1.29)

> 2025-04-15

### Bug Fixes

* the cran submission workflow now parses the correct artifact name


<a name="v0.1.28"></a>

## [v0.1.28](https://github.com/PetrCala/artma/compare/v0.1.27...v0.1.28)

> 2025-04-15


<a name="v0.1.27"></a>

## [v0.1.27](https://github.com/PetrCala/artma/compare/v0.1.26...v0.1.27)

> 2025-04-15


<a name="v0.1.26"></a>

## [v0.1.26](https://github.com/PetrCala/artma/compare/v0.1.25...v0.1.26)

> 2025-04-15


<a name="v0.1.25"></a>

## [v0.1.25](https://github.com/PetrCala/artma/compare/v0.1.24...v0.1.25)

> 2025-04-15

### Bug Fixes

* add a missing newline to the release artifacts text file
* the github workflow artifacts read now uses a more robust code
* the build artifacts read workflow step now reads from a correct file
* the gh release artifacts are now read from a single source of truth file
* attempt to fix the build artifact upload path


<a name="v0.1.24"></a>

## [v0.1.24](https://github.com/PetrCala/artma/compare/v0.1.23...v0.1.24)

> 2025-04-14

### Bug Fixes

* the cran comments build workflow should now target the file correctly


<a name="v0.1.23"></a>

## [v0.1.23](https://github.com/PetrCala/artma/compare/v0.1.22...v0.1.23)

> 2025-04-14

### Bug Fixes

* change the release notes creation script to be more lenient
* the update news action now updates with all the existing GH tags
* replace the invalid slash in the git origin path in the news update workflow
* remove the unused 'replace' argument in mock df creation
* ArtmaBot now authenticates through the deploy key when updating the news file
* the random vector generation now generates floats (was int) from a uniform distribution
* use the official homebrew action to install homebrew
* provide explicit pacakge method assignment when reading data
* remove the homebrew install from the news update workflow
* minor changes in the open PR script
* comment out a PR open automation script irrelevant line
* remove the mishandled 'v' tag
* workflows to use a composite news creation action
* more workflow issues
* secret passing to the build workflow
* the news update secrets passing
* the cran submission workflow cli package call

### Features

* reading the data now validates the data frame as well


<a name="v0.1.22"></a>

## [v0.1.22](https://github.com/PetrCala/artma/compare/v0.1.21...v0.1.22)

> 2025-04-14

### Code Refactoring

* modify the build workflow to release from the CRAN version as opposed to the current one
* the runtime module methods invocation function is now better formatted


<a name="v0.1.21"></a>

## [v0.1.21](https://github.com/PetrCala/artma/compare/v0.1.20...v0.1.21)

> 2025-04-11


<a name="v0.1.20"></a>

## [v0.1.20](https://github.com/PetrCala/artma/compare/v0.1.19...v0.1.20)

> 2025-04-10


<a name="v0.1.19"></a>

## [v0.1.19](https://github.com/PetrCala/artma/compare/v0.1.18...v0.1.19)

> 2025-04-10


<a name="v0.1.18"></a>

## [v0.1.18](https://github.com/PetrCala/artma/compare/v0.1.17...v0.1.18)

> 2025-04-09


<a name="v0.1.17"></a>

## [v0.1.17](https://github.com/PetrCala/artma/compare/v0.1.16...v0.1.17)

> 2025-04-09


<a name="v0.1.16"></a>

## [v0.1.16](https://github.com/PetrCala/artma/compare/v0.1.15...v0.1.16)

> 2025-04-09


<a name="v0.1.15"></a>

## [v0.1.15](https://github.com/PetrCala/artma/compare/v0.1.14...v0.1.15)

> 2025-04-09


<a name="v0.1.14"></a>

## [v0.1.14](https://github.com/PetrCala/artma/compare/v0.1.13...v0.1.14)

> 2025-04-08


<a name="v0.1.13"></a>

## [v0.1.13](https://github.com/PetrCala/artma/compare/v0.1.12...v0.1.13)

> 2025-04-08


<a name="v0.1.12"></a>

## [v0.1.12](https://github.com/PetrCala/artma/compare/v0.1.11...v0.1.12)

> 2025-04-07


<a name="v0.1.11"></a>

## [v0.1.11](https://github.com/PetrCala/artma/compare/v0.1.10...v0.1.11)

> 2025-04-07


<a name="v0.1.10"></a>

## [v0.1.10](https://github.com/PetrCala/artma/compare/v0.1.9...v0.1.10)

> 2025-04-05


<a name="v0.1.9"></a>

## [v0.1.9](https://github.com/PetrCala/artma/compare/v0.1.8...v0.1.9)

> 2025-04-05


<a name="v0.1.8"></a>

## [v0.1.8](https://github.com/PetrCala/artma/compare/v0.1.7...v0.1.8)

> 2025-04-05


<a name="v0.1.7"></a>

## v0.1.7

> 2025-04-04

