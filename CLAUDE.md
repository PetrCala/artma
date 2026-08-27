# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**What belongs in this file**: durable truths an agent needs in every session: the architecture map, commands, and conventions. Anything that changes with a feature PR (contracts, schemas, edge cases) lives in `contributingGuides/` and is linked from here; patch the guide, not this file. If a test enforces a rule, state the rule in one line and let the test document itself.

## Project Overview

**artma** (Automatic Replication Tools for Meta-Analysis) is an R package that provides a unified interface for performing various meta-analysis methods. The package uses a modular architecture built on the `box` package, organizing most implementation code in `inst/artma/` rather than the traditional `R/` directory.

## Build and Development Commands

Daily targets (run `make help` for the full list):

```bash
make setup                        # Install dependencies and set up environment
make test                         # Run all tests
make test-file FILE=<path>        # Run a specific test file
make test-filter FILTER=<pattern> # Run tests matching pattern
make lint                         # Lint the package
make check                        # R CMD check via devtools
make document                     # Regenerate docs and the check manifest
make quick                        # Quick dev cycle (document + test)
```

For interactive development, `devtools::load_all()` loads the package into an R session.

## Architecture

### Module System with `box`

The package uses the `box` package for Python-style module imports. Implementation code lives in `inst/artma/`:

- `inst/artma/methods/`: runtime methods, the core analytical functions
- `inst/artma/libs/`: shared utilities (`core`, `infrastructure`, `formatting`)
- `inst/artma/interactive/`: interactive UI components
- `inst/artma/variable/`: variable analysis and suggestion
- `inst/artma/econometric/`: econometric calculation helpers
- `inst/artma/data/`: data pipeline
- `inst/artma/options/`: options system and templates
- `inst/artma/data_config/`: data configuration handling
- `inst/artma/calc/`: computation engines for specific methods
- `inst/artma/modules/`: higher-level orchestration modules

Import internal modules with `box::use()` paths relative to `inst`:

```r
box::use(
  artma / libs / core / validation[validate, assert],
  artma / data / index[prepare_data]
)
```

Always reference external package functions explicitly: `pkg::function()`, never bare names.

### Generated Check Manifest (`R/generated_check_manifest.R`)

`box::use()` imports and `inst/artma` code are invisible to R CMD check; the generated manifest declares the globals and keep-alive references that keep check quiet. Never hand-edit it. After changing a `box::use()` import in any `R/*.R` file, or adding an Imports dependency used only inside `inst/artma`, run `make document` (or `make generate-check-manifest`); a testthat test fails if the committed file drifts.

### Runtime Methods

Runtime methods are the analytical functions users invoke via `artma::artma(methods = c("method_name"))`. Each lives in `inst/artma/methods/<method_name>.R` as a plain implementation function; `register_runtime_method()` (from `inst/artma/modules/runtime_methods.R`) produces the exported `run` wrapper, adding the shared caching layer and declarative metadata:

```r
my_method <- function(df, bma_result = NULL, ...) {
  new_method_result(
    tables = list(summary = summary_df),    # rounded display tables
    estimates = my_method_estimates(model), # unrounded, fixed schema
    plots = list(),
    meta = list()
  )
}

run <- register_runtime_method(
  my_method,
  stage = "my_method",
  description = "One line on what the method does",
  depends_on = "bma",        # yields a bma_result parameter
  required_columns = c("effect", "se"),
  suggests = "BMS"           # optional packages the method needs
)

box::export(my_method, run)
```

Export `run` plus the implementation (tests import it). Methods are auto-discovered from `inst/artma/methods/`; `df` is the preprocessed data frame, other arguments come from the options system. Every method that reports numbers fills `estimates` with an unrounded long-format frame in the fixed shared schema (`linear_tests` is the reference implementation); rounding is a display concern and never touches the `estimates` path. Metadata semantics, the full return contract, export naming, and the estimates schema: [contributingGuides/METHODS.md](contributingGuides/METHODS.md).

### Options System

Options live in hierarchical YAML files generated from the self-describing template (`inst/artma/options/templates/`); when loaded, they sit in the R `options()` namespace prefixed with `artma.`. Always provide a default matching the template default:

```r
# CORRECT: always provide a default
round_to <- getOption("artma.output.number_of_decimals", 3)

# WRONG: returns NULL if the user's options file predates the option
round_to <- getOption("artma.output.number_of_decimals")
```

Runtime-populated options (`artma.temp.*`, `artma.data.config`, `artma.data.source_path`) are the exception, where `NULL` means "not yet set"; read those with `require_option()` when they must exist. Template node reference and `get_option_group()` usage: [contributingGuides/OPTIONS.md](contributingGuides/OPTIONS.md).

### Caching

`cache_cli()` (`inst/artma/libs/infrastructure/cache.R`) memoises expensive functions on disk while preserving their CLI output; caching is on by default, so the cache key must cover every input. Route new inputs through `build_data_cache_signature()` rather than relying on the argument hash, and register any new file-writing side effect in a cached path with `record_output_file()`. Signature contents, invalidation, and debugging: [contributingGuides/CACHING.md](contributingGuides/CACHING.md).

### Data Pipeline

1. Read (`artma/data/read.R`): load CSV, Excel, JSON, Stata, or RDS
2. Preprocess (`artma/data/preprocess.R`): standardize column names, handle missing values
3. Compute (`artma/data/compute.R`): derive columns (effect sizes, standard errors)
4. Config (`artma/data_config/`): per-column configuration of which variables join which analyses

The entry point is `prepare_data()` from `artma/data/index.R`.

Column auto-detection declines a required role rather than guess it. `data.mapping.external_command` is an opt-in hook that lets an external command propose the missing mappings over JSON on stdin/stdout; every proposal is verified against the data before it is used, and the hook is off unless the option is set. Payload and response schemas: [contributingGuides/COLUMN_MAPPING_HOOK.md](contributingGuides/COLUMN_MAPPING_HOOK.md).

### Session Hub

Interactive `artma()` calls without `methods` enter a menu loop (`inst/artma/interactive/hub.R`) that runs the pipeline steps repeatedly and returns the accumulated results; scripted paths stay linear. A call that names no options file enters *unbound*: the hub runs on template defaults, offers picking or managing the options file as its first menu item (`inst/artma/interactive/options_file_menu.R`), and prepares data lazily once a file is loaded. Entry conditions, the bound/unbound state, items, return contract, and extension points: [contributingGuides/HUB.md](contributingGuides/HUB.md).

### Autonomy System

Controls how much user interaction happens during analysis. `interactive()` is the hard gate: non-interactive sessions never prompt. Within interactive sessions the level (stored in the options file under `autonomy.level`) is one of `ask_more`, `balanced`, or `autonomous` (the default), ordered from most to least talkative. Gate prompts with `should_prompt_user(required_level)` from `artma / libs / core / autonomy`: use `required_level = "balanced"` for non-critical options, preferences, and save/overwrite confirmations; `required_level = "autonomous"` (the default) for variable selection, method selection, and column mapping. Public API: `artma::autonomy_get()`, `artma::autonomy_set()`, `artma::autonomy_is_full()` (also TRUE in non-interactive sessions).

## Conventions

- Exported API names: snake_case with the noun group first (`options_create`, `config_set`); never add new dotted exports. The dotted names are deprecated aliases; policy and shim pattern: [contributingGuides/API.md](contributingGuides/API.md).
- Validation: use `validate()` and `assert()` from `artma / libs / core / validation`, not `stopifnot()`. `assert(cond, "message")` takes an explicit error message.
- Interactive prompts: use `ask_text`/`ask_select`/`ask_yes_no` from `artma / interactive / input`, never raw `readline()` or direct `climenu` calls (climenu, a sub-package of this repository, stays the menu backend; never `utils::menu()`). Layout contract, retry/default semantics, and the injection testing pattern: [contributingGuides/PROMPTS.md](contributingGuides/PROMPTS.md).
- Verbosity: `options(artma.verbose = <1..4>)` scales from errors-only (1) to debug (4); default 3. Call the leveled wrapper (`log_error`/`log_warn`/`log_info`/`log_debug`, or `is_*_enabled()` to gate non-message output) from `artma / libs / core / log`; never compare verbosity numbers outside `libs/core`.
- Constants: `CONST` (`inst/artma/const.R`) and `PATHS` (`inst/artma/paths.R`), both globally declared; import with `box::use(artma / const[CONST])`.
- Style: 2-space indentation; `snake_case` or `dotted.case` names (max 40 chars); prefer `cli::cli_*` over base messaging; custom linters live in `.lintr.R`.
- Documentation: roxygen2 with type annotations: `@param x *\[character, optional\]* Description`.
- Commits: Conventional Commits, enforced by the commitlint workflow; the changelog is generated from them via `git-chglog`.
- Ad-hoc scripts: write outputs to absolute paths; never derive a save path from `commandArgs()`.

## Testing

- Unit tests: `tests/testthat/test-<feature>.R`; E2E tests: `tests/E2E/` (run with `make test-e2e`).
- Parallel testing is enabled (`Config/testthat/parallel: TRUE`).

## Pre-commit Checklist

Run `make hooks` once per clone; it installs git hooks (`.githooks/`) that then style, lint, and validate commit messages automatically on every commit.

1. `styler::style_pkg()` (or style the changed files).
2. If you changed `box::use()` imports in `R/*.R` or added an Imports package used only in `inst/artma`: `make document` (see Generated Check Manifest).
3. `make lint` and `make test`.
4. Run lint/check under a UTF-8 locale (e.g. `LC_ALL=en_US.UTF-8`); the C locale corrupts UTF-8 characters in generated `man/*.Rd` files.

## Environment

For `box.linters` to resolve imports during development, set in your `.Rprofile`:

```r
options(box.path = "<path-to-artma>/inst")
```
