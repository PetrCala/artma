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
- `inst/artma/libs/` — Shared utilities (validation, caching, string manipulation, etc.)
- `inst/artma/data/` — Data pipeline (read, preprocess, compute)
- `inst/artma/options/` — Options system and templates
- `inst/artma/data_config/` — Data configuration handling
- `inst/artma/calc/` — Computation engines for specific methods
- `inst/artma/modules/` — Higher-level orchestration modules
- `inst/artma/testing/` — Test fixtures and mocks

Import modules using:

```r
box::use(
  artma / libs / validation[validate, assert],
  artma / data / index[prepare_data]
)
```

Always reference external package functions explicitly: `pkg::function()` (never bare function names).

### Runtime Methods System

Runtime methods are the main analytical functions users invoke via `artma::run(methods = c("method_name"))`.

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

### Autonomy System

The autonomy system controls how much user interaction is required during analysis. Higher levels mean less user interaction and more automatic decision-making.

**Autonomy Levels:**

1. **Level 1 - Minimal**: Maximum user control - prompt for all optional decisions
2. **Level 2 - Low**: Frequent prompts - ask for most non-critical decisions
3. **Level 3 - Medium**: Balanced - prompt for important decisions only
4. **Level 4 - High**: Mostly autonomous - minimal prompts for critical decisions only (default for interactive mode)
5. **Level 5 - Full**: Fully autonomous - no prompts, use all defaults and auto-detection (default for non-interactive mode)

**Key Functions:**

```r
# Get current autonomy level
level <- autonomy.get()

# Set autonomy level
autonomy.set(4)

# Check if set
autonomy.is_set()

# Get description
autonomy.describe()

# Check if fully autonomous
autonomy.is_full()
```

**Internal Usage:**

When implementing features that may require user input, use the `should_prompt_user()` function to check if prompting is appropriate:

```r
box::use(artma / libs / autonomy[should_prompt_user])

# Only prompt if autonomy level is below 4
if (should_prompt_user(required_level = 4)) {
  # Show interactive prompt
  choice <- climenu::select(...)
} else {
  # Use automatic default
  choice <- default_value
}
```

**Configuration:**

The autonomy level is stored in the options file under `autonomy.level` and is automatically loaded when options are loaded. In non-interactive mode, the level is always set to 5 (Full) regardless of the stored value, as prompts are not possible.

### Caching System

Use `cache_cli()` to memoize expensive functions while preserving CLI output:

```r
box::use(artma / libs / cache[cache_cli, cache_cli_runner])

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
box::use(artma / libs / validation[validate, assert])

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
if (get_verbosity() >= 3) {
  cli::cli_inform("Processing data...")
}
```

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
