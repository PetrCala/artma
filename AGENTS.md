# Repository Guidelines

## Project Structure & Module Organization

Source exports live in `R/`, while modular implementation code is grouped under `inst/artma` to support box-style imports. The module structure is organized by domain:

- **`inst/artma/libs/`** — Shared utilities organized by category:
  - `libs/core/` — Fundamental utilities (validation, utils, string, number, file)
  - `libs/infrastructure/` — System-level functionality (cache, debug, polyfills)
  - `libs/formatting/` — Result formatting (results)
- **`inst/artma/interactive/`** — Interactive UI components (ask, editor, save_preference, effect_summary_stats, welcome). These components respect the autonomy system (`inst/artma/libs/core/autonomy.R`) which controls user interaction levels.
- **`inst/artma/variable/`** — Variable analysis and suggestion (detection, suggestion, bma)
- **`inst/artma/econometric/`** — Econometric calculation helpers (bma, linear, nonlinear, exogeneity, p_hacking)
- **`inst/artma/methods/`** — Runtime methods (the core analytical functions)
- **`inst/artma/data/`** — Data pipeline (read, preprocess, compute)
- **`inst/artma/options/`** — Options system and templates
- **`inst/artma/data_config/`** — Data configuration handling
- **`inst/artma/calc/`** — Computation engines for specific methods
- **`inst/artma/modules/`** — Higher-level orchestration modules
- **`inst/artma/testing/`** — Test fixtures and mocks

Tests reside in `tests/testthat` for unit coverage and `tests/E2E` for installation-level checks. Generated documentation belongs in `man/`, long-form guides in `vignettes/`, and helper scripts in `scripts/`. Run assets or large examples under `inst/` so they ship with the package.

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
