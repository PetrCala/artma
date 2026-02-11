# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**artma** (Automatic Replication Tools for Meta-Analysis) is an R package that provides a unified interface for performing various meta-analysis methods. The package uses a modular architecture built on the `box` package, organizing most implementation code in `inst/artma/` rather than the traditional `R/` directory.

## Build and Development Commands

All major operations use `make`:

```bash
make setup                      # Install dependencies and set up environment
make test                       # Run all tests
make test-file FILE=<path>      # Run a specific test file
make test-filter FILTER=<pattern> # Run tests matching pattern
make test-e2e                   # Run end-to-end tests
make lint                       # Lint the entire package
make check                      # Run R CMD check via devtools
make document                   # Generate documentation with roxygen2
make coverage                   # Run code coverage report
make build                      # Build the package
make vignettes                  # Build vignettes
make all                        # Run document, test, lint, and check
make quick                      # Quick dev cycle (document + test)
```

For development in R:

```r
devtools::load_all()        # Load package for interactive development
devtools::test()            # Run tests from R console
```

See `.make-help.md` for a quick reference guide.

## Architecture

### Module System with `box`

The package uses the `box` package for Python-style module imports. Implementation code lives in `inst/artma/` and is organized into:

- `inst/artma/methods/` — Runtime methods (the core analytical functions)
- `inst/artma/libs/` — Shared utilities organized by category:
  - `libs/core/` — Fundamental utilities (validation, utils, string, number, file)
  - `libs/infrastructure/` — System-level functionality (cache, debug, polyfills)
  - `libs/formatting/` — Result formatting (results)
- `inst/artma/interactive/` — Interactive UI components (ask, editor, save_preference, effect_summary_stats, welcome)
- `inst/artma/variable/` — Variable analysis and suggestion (detection, suggestion, bma)
- `inst/artma/econometric/` — Econometric calculation helpers (bma, linear, nonlinear, exogeneity, p_hacking)
- `inst/artma/data/` — Data pipeline (read, preprocess, compute)
- `inst/artma/options/` — Options system and templates
- `inst/artma/data_config/` — Data configuration handling
- `inst/artma/calc/` — Computation engines for specific methods
- `inst/artma/modules/` — Higher-level orchestration modules
- `inst/artma/testing/` — Test fixtures and mocks

Import modules using:

```r
box::use(
  artma / libs / core / validation[validate, assert],
  artma / libs / core / utils[get_verbosity],
  artma / data / index[prepare_data],
  artma / econometric / bma[get_bma_formula, run_bma],
  artma / interactive / ask[ask_for_overwrite_permission],
  artma / variable / suggestion[suggest_variables_for_effect_summary]
)
```

Always reference external package functions explicitly: `pkg::function()` (never bare function names).

#### Global Variables Declaration (`R/globals.R`)

Because `box::use()` imports are invisible to R CMD check, every symbol imported via `box::use()` inside `R/` files must be declared in `R/globals.R` via `utils::globalVariables()`. When adding new `box::use()` imports in any `R/*.R` file — whether importing a module path segment (e.g., `core`, `output`) or a specific function (e.g., `resolve_output_dir`) — add the new symbol to `R/globals.R` in alphabetical order. Run `make check` periodically to catch any "no visible global function or variable" NOTEs caused by missing declarations.

### Runtime Methods System

Runtime methods are the main analytical functions users invoke via `artma::artma(methods = c("method_name"))`.

Each method is defined in `inst/artma/methods/<method_name>.R` and **must** export a `run` function with this signature:

```r
run <- function(df, ...) {
  # Implementation
}
```

The `df` parameter is the preprocessed data frame; additional arguments come from the options system.

Methods are auto-discovered at runtime by scanning `inst/artma/methods/`. Use `artma::methods.list()` to see available methods.

Execution order for methods is controlled by `CONST$RUNTIME_METHODS$EXECUTION_ORDER` in `inst/artma/const.R`.

### Options System

Options are stored in hierarchical YAML files. The system uses:

1. **Template** (`inst/artma/options/templates/`) — Defines all available options with types, defaults, and validation rules
2. **User options files** — Created at runtime in a temporary directory or user-specified location

Options are loaded temporarily into the R `options()` namespace for the duration of a function call, prefixed with `artma.`:

```r
# In options YAML:
# methods:
#   effect_summary_stats:
#     conf_level: 0.95

# In code:
conf_level <- getOption("artma.methods.effect_summary_stats.conf_level")
# Or use the helper:
opt <- get_option_group("artma.methods.effect_summary_stats")
conf_level <- opt$conf_level
```

Template structure for each option node:

- `name` — Option name
- `type` — R type (character, logical, numeric, etc.)
- `default` — Default value
- `fixed` — If true, user cannot override
- `allow_na` — Whether NA is permitted
- `prompt` — How to ask for value: "readline", "file", "directory"
- `help` — Help text

#### Options Access Convention

When reading options with `getOption()`, always provide a sensible default that
matches the template default:

```r
# CORRECT — always provide a default
precision_type <- getOption("artma.calc.precision_type", "1/SE")
round_to <- getOption("artma.output.number_of_decimals", 3)

# CORRECT — when using get_option_group, use %||% for each field
opt <- get_option_group("artma.methods.box_plot")
max_per_plot <- opt$max_boxes_per_plot %||% 60L

# WRONG — no default, will return NULL if option is not set
precision_type <- getOption("artma.calc.precision_type")
```

This keeps the package functional even when a user's options file is outdated
or missing newly added options. The only exceptions are runtime-populated
options (`artma.temp.*`, `artma.data.config`, `artma.data.source_path`) where
`NULL` is the expected "not yet set" sentinel.

### Caching System

Use `cache_cli()` to memoize expensive functions while preserving CLI output:

```r
box::use(artma / libs / infrastructure / cache[cache_cli, cache_cli_runner])

run_models <- cache_cli(
  .run_models_impl,
  extra_keys = list(pkg_version = utils::packageVersion("artma"))
)

# Or for reusable patterns:
run_summary <- cache_cli_runner(
  summary_impl,
  stage = "my_stage",
  key_builder = function(...) build_data_cache_signature()
)
```

Control caching behavior:

- `invalidate_fun` — Function that returns TRUE to bypass cache
- `max_age` — Time-to-live in seconds
- `options(artma.cache.use_cache = FALSE)` — Disable caching globally

### Data Pipeline

The data processing flow:

1. **Read** (`artma/data/read.R`) — Load data from CSV, Excel, JSON, Stata, or RDS
2. **Preprocess** (`artma/data/preprocess.R`) — Standardize column names, handle missing values
3. **Compute** (`artma/data/compute.R`) — Calculate derived columns (effect sizes, standard errors)
4. **Config** (`artma/data_config/`) — Data configuration defines which variables participate in which analyses

The main entry point is `prepare_data()` from `artma/data/index.R`.

### Validation Functions

Use custom validation helpers instead of base R stopifnot:

```r
box::use(artma / libs / core / validation[validate, assert])

validate(is.numeric(x), length(x) > 0, is.function(fn))
# Validates conditions; prints verbose messages for type checks

assert(x > 0, "x must be positive")
# Custom assertion with explicit error message
```

### Interactive Menus

For interactive CLI menus, use the `climenu` package (available as a sub-package in this repository):

```r
box::use(climenu[menu])

choice <- menu(
  choices = c("Option 1", "Option 2", "Option 3"),
  title = "Select an option"
)
```

Key features:

- Keyboard navigation with arrow keys and vim bindings (j/k)
- Search/filter functionality with `/`
- Multi-select support
- Customizable styling and prompts

Do NOT use other menu packages like `utils::menu()` or external packages for interactive selections.

### Verbosity Levels

Control output verbosity via `options(artma.verbose = <level>)`:

| Level | Description |
|-------|-------------|
| 1 | Errors only |
| 2 | Warnings + errors |
| 3 | Info (default) — progress and high-level info |
| 4 | Debug/trace — everything including internals |

Check verbosity in code:

```r
box::use(artma / libs / core / utils[get_verbosity])

if (get_verbosity() >= 3) {
  cli::cli_inform("Processing data...")
}
```

### Autonomy System

The autonomy system controls how much user interaction is required during analysis. It provides 5 levels of autonomy:

| Level | Name | Description |
|-------|------|-------------|
| 1 | Minimal | Maximum user control - prompt for all optional decisions |
| 2 | Low | Frequent prompts - ask for most non-critical decisions |
| 3 | Medium | Balanced - prompt for important decisions only |
| 4 | High | Mostly autonomous - minimal prompts for critical decisions only (default for interactive mode) |
| 5 | Full | Fully autonomous - no prompts, use all defaults and auto-detection (default for non-interactive mode) |

**Core Mechanism**: The `should_prompt_user(required_level)` function determines whether to prompt based on current autonomy level. If current level < required_level, prompt; otherwise use defaults.

**Usage in Code**:

```r
box::use(artma / libs / core / autonomy[should_prompt_user])

# Before prompting for variable selection
if (!should_prompt_user(required_level = 4)) {
  # Use automatic selection with defaults
  return(auto_select_variables(df, config))
}

# Show interactive menu
selected <- climenu::select(...)
```

**Required Level Guidelines**:

- `required_level=2`: Non-critical options, preferences
- `required_level=3`: Save preferences, overwrite confirmations
- `required_level=4`: Variable selection, method selection, column mapping
- `required_level=5`: Critical decisions (rarely used, would never prompt)

**Non-Interactive Mode**: In non-interactive mode (e.g., R scripts, batch jobs), level 5 is always enforced regardless of the setting, as user prompts are not possible.

**Setting Autonomy Level**:

```r
# Get current level
artma::autonomy.get()

# Set level
artma::autonomy.set(4)

# Check if fully autonomous
artma::autonomy.is_full()
```

The autonomy level is stored in the options file under `general.autonomy.level` and is automatically loaded when options are loaded via `options.load()`.

## Code Style

- **Formatting**: Use `styler::style_pkg()` before committing
- **Linting**: Project uses custom linters in `.lintr.R`
  - 2-space indentation
  - `snake_case` or `dotted.case` naming (max 40 characters)
  - No line length limit enforced by linter (but keep reasonable)
  - Prefer `cli::cli_*` over base messaging functions
- **Imports**: Always use `box::use()` for internal modules, `pkg::fun()` for external packages
- **Documentation**: Roxygen2 with type annotations: `@param x *\[character, optional\]* Description`

## Testing

- Unit tests: `tests/testthat/test-<feature>.R`
- E2E tests: `tests/E2E/`
- Parallel testing enabled: `Config/testthat/parallel: TRUE`

Run tests:

```bash
make test                        # All tests
make test-file FILE=test-foo.R   # Specific file
make test-filter FILTER="pattern" # Filtered tests
make coverage                    # Coverage report
```

## Commit Conventions

Follow Conventional Commits (`@commitlint/config-conventional`):

- `feat:` — New features
- `fix:` — Bug fixes
- `docs:` — Documentation changes
- `test:` — Test additions/changes
- `refactor:` — Code restructuring
- `chore:` — Maintenance tasks
- `perf:` — Performance improvements

The commit lint workflow will reject non-conforming commits. This enables automatic changelog generation via `git-chglog`.

## Key Constants

Global constants are defined in `inst/artma/const.R` and exported as `CONST`. Paths are in `inst/artma/paths.R` as `PATHS`. Both are available globally via `utils::globalVariables()`.

Access like:

```r
box::use(artma / const[CONST])
CONST$PACKAGE_NAME
CONST$DATA$TYPES
```

## Box Paths Configuration

For linting to work correctly with `box.linters`, set in your `.Rprofile`:

```r
options(box.path = "<path-to-artma>/inst")
```

This makes box imports resolve correctly during development.
