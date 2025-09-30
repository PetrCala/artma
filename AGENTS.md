# Repository Guidelines

## Project Structure & Module Organization
Source exports live in `R/`, while modular implementation code is grouped under `inst/artma` to support box-style imports. Tests reside in `tests/testthat` for unit coverage and `tests/E2E` for installation-level checks. Generated documentation belongs in `man/`, long-form guides in `vignettes/`, and helper scripts in `scripts/`. Run assets or large examples under `inst/` so they ship with the package.

## Build, Test, and Development Commands
- `./run.sh setup` — install package dependencies and local tooling.
- `./run.sh lint` — load the package and run `lintr::lint_package()` with the project presets.
- `./run.sh test` — invoke `devtools::test()`; pass `--filter` or `--file` to scope runs.
- `./run.sh test-e2e` — execute end-to-end scenarios from `tests/E2E`.
- `./run.sh check` — wrap `devtools::check()` for a CRAN-style check.
- `./run.sh document` / `./run.sh namespace` — refresh roxygen docs and the `NAMESPACE` file.
- `./run.sh coverage` — compute package coverage via `covr::package_coverage()`.

## Coding Style & Naming Conventions
Format R code with `styler::style_pkg()` or your IDE’s styler integration before submitting. The lintr profile enforces 2-space indentation, ≤120 character lines, and `snake_case` (or dotted.case) identifiers; long-lived objects should stay under 40 characters. Prefer `cli::cli_*` messaging over base `print()`, `message()`, `cat()`, or `warning()`, and import modules with `box::use()` paths relative to `inst`. Avoid functions flagged by the project undesirable-function linter (see `linter.R`) so future code generation stays lint-clean. Run `./run.sh lint` locally and address all reported findings.

## Testing Guidelines
Add unit tests beside the feature in `tests/testthat`, using `test-<feature>.R` filenames and helper fixtures. End-to-end flows belong in `tests/E2E` and should validate real installation behavior. Run `./run.sh test` for smoke checks, `./run.sh test --filter <pattern>` when iterating, and `./run.sh coverage` before merging significant work. Aim to keep coverage stable and clean up any temporary artifacts your tests create.

## Commit & Pull Request Guidelines
Follow Conventional Commits (`feat:`, `fix:`, `docs:`, etc.); the commit lint workflow (`@commitlint/config-conventional`) will block PRs that deviate. Squash noisy WIP commits locally before pushing. Each PR should describe the change, reference related issues, note breaking impacts, and attach screenshots or CLI transcripts for user-facing updates. Run `./run.sh lint` and `./run.sh check` beforehand, update `NEWS.md` or vignettes when behavior changes, and ensure `run.sh document` is reflected in tracked files.

## Documentation & Release Tips
When adding new exports, include roxygen examples and regenerate docs with `./run.sh document`. Update `vignettes/` for workflow changes and verify they build via `./run.sh vignettes`. Version bumps go through `./run.sh bump-version`, which aligns with the automated changelog and release cycle.
