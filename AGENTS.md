# Repository Guidelines

## Project Structure & Module Organization
Source exports live in `R/`, while modular implementation code is grouped under `inst/artma` to support box-style imports. Tests reside in `tests/testthat` for unit coverage and `tests/E2E` for installation-level checks. Generated documentation belongs in `man/`, long-form guides in `vignettes/`, and helper scripts in `scripts/`. Run assets or large examples under `inst/` so they ship with the package.

## Autonomy System
The package includes an **autonomy system** that controls how much user interaction is required during analysis. When implementing new features, consider the autonomy level to determine whether to prompt the user or use automatic defaults.

**Autonomy Levels (1-5):**
- **Level 1 (Minimal)**: Maximum user control - prompt for all optional decisions
- **Level 2 (Low)**: Frequent prompts - ask for most non-critical decisions
- **Level 3 (Medium)**: Balanced - prompt for important decisions only
- **Level 4 (High)**: Mostly autonomous - minimal prompts for critical decisions only (default for interactive mode)
- **Level 5 (Full)**: Fully autonomous - no prompts, use all defaults and auto-detection (default for non-interactive mode)

**When to Consider Autonomy:**
- Before prompting the user with interactive menus or confirmations
- When deciding between manual configuration and automatic detection
- When choosing whether to ask for confirmation of defaults

**Implementation Pattern:**
```r
box::use(artma / libs / autonomy[should_prompt_user])

if (should_prompt_user(required_level = 4)) {
  # Show interactive prompt for levels 1-3
  choice <- climenu::select(...)
} else {
  # Use automatic default for levels 4-5
  choice <- default_value
}
```

The autonomy level is stored in the options file (`autonomy.level`) and automatically loaded when options are loaded. In non-interactive mode, the level is always 5 (Full) regardless of the stored value.

## Build, Test, and Development Commands
- `make setup` — install package dependencies and local tooling.
- `make lint` — load the package and run `lintr::lint_package()` with the project presets.
- `make test` — invoke `devtools::test()`.
- `make test-file FILE=<name>` — run a specific test file.
- `make test-filter FILTER=<pattern>` — run tests matching a pattern.
- `make test-e2e` — execute end-to-end scenarios from `tests/E2E`.
- `make check` — wrap `devtools::check()` for a CRAN-style check.
- `make document` — refresh roxygen docs and the `NAMESPACE` file.
- `make coverage` — compute package coverage via `covr::package_coverage()`.
- `make all` — run document, test, lint, and check in sequence.

## Coding Style & Naming Conventions
Format R code with `styler::style_pkg()` or your IDE's styler integration before submitting. The lintr profile enforces 2-space indentation, ≤120 character lines, and `snake_case` (or dotted.case) identifiers; long-lived objects should stay under 40 characters. Prefer `cli::cli_*` messaging over base `print()`, `message()`, `cat()`, or `warning()`, and import modules with `box::use()` paths relative to `inst`. Avoid functions flagged by the project undesirable-function linter (see `linter.R`) so future code generation stays lint-clean. Run `make lint` locally and address all reported findings.

Always reference functions from external packages with the explicit `pkg::fun()` syntax so call sites remain unambiguous.

## Testing Guidelines
Add unit tests beside the feature in `tests/testthat`, using `test-<feature>.R` filenames and helper fixtures. End-to-end flows belong in `tests/E2E` and should validate real installation behavior. Run `make test` for smoke checks, `make test-filter FILTER=<pattern>` when iterating, and `make coverage` before merging significant work. Aim to keep coverage stable and clean up any temporary artifacts your tests create.

## Commit & Pull Request Guidelines
Follow Conventional Commits (`feat:`, `fix:`, `docs:`, etc.); the commit lint workflow (`@commitlint/config-conventional`) will block PRs that deviate. Squash noisy WIP commits locally before pushing. Each PR should describe the change, reference related issues, note breaking impacts, and attach screenshots or CLI transcripts for user-facing updates. Run `make lint` and `make check` beforehand, update `NEWS.md` or vignettes when behavior changes, and ensure `make document` is reflected in tracked files.

## Documentation & Release Tips
When adding new exports, include roxygen examples and regenerate docs with `make document`. Update `vignettes/` for workflow changes and verify they build via `make vignettes`. Version bumps go through the version bump script in `scripts/`, which aligns with the automated changelog and release cycle.
