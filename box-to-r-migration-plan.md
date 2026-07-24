# Migration plan: inst/artma to R/ with a plain package namespace

Status: proposal (issue #322, item F1). This document is a costed plan, not a commitment. Nothing in this PR changes code.

## Verdict up front

**Go, with sequencing conditions.** The migration is large but mostly mechanical, the box-specific coupling in the codebase is measurably thin (details below), and the payoff is permanent: it deletes an entire class of meta-infrastructure and restores static analysis and coverage over roughly 26,000 lines that are invisible to R CMD check and covr today. Estimated effort: 8 to 12 focused working days across about 10 tranche PRs, calendar 2 to 3 weeks. Conditions:

1. Land the in-flight #322 refactors first (B/C/E items). They churn the same files; running both at once multiplies conflicts.
2. Accept a freeze on other structural work in whichever directory a tranche is migrating.
3. B2 and B7 (the two known cross-module name collisions) must be merged before the affected tranches, or the collisions get renamed during the move.

## 1. Current state, measured

Numbers from this checkout (origin/master at 091d9b5):

| Metric | Value |
|---|---|
| R files under `inst/artma` | 95 |
| Lines under `inst/artma` | 25,912 |
| Lines under `R/` | 2,575 (126 of them the generated check manifest) |
| `box::use()` occurrences (R/ + inst) | 317 |
| `box::export()` calls | 88 |
| Top-level definitions in `inst/artma` | 651 |
| Names defined in more than one module | 3 (see section 5) |
| External-package attachments inside `box::use()` in inst | 5 (`stats` x4, `utils` x1) |
| Whole-module aliased imports | 2 (`calc = artma / calc / index`, `editor_mod = artma / interactive / editor` in `R/options.R:535`) |
| Dynamic (string-built) box imports | 1 (`inst/artma/options/template.R:244`) |
| testthat files | 78, of which 77 use `box::use()` |
| Exported public API functions (NAMESPACE) | 28 |
| roxygen comment lines in inst (never processed by roxygen today) | 4,040 |
| Stray `#' @export` tags in inst (currently inert) | 42 |

The single most important measurement: almost every external call in `inst/artma` is already written as `pkg::fun()` (the repo convention). Only 5 `box::use()` specs attach external package functions by name. So the "map the box graph to `@importFrom`" step is nearly trivial; the real work is elsewhere (discovery, paths, fingerprints, tests).

## 2. What gets deleted outright

Each entry verified against the file named.

### 2.1 The check-manifest generator

- `scripts/R/generate_check_manifest.R` (233 lines). It parses every `R/*.R` for `box::use()` calls, declares every bare identifier inside them via `utils::globalVariables()`, and emits one keep-alive `pkg::fun` reference per Imports package used only inside `inst/artma` (`.cm_collect_inst_only_keepalive_refs`). Both problems exist only because check cannot see box imports or inst code. Gone.
- `R/generated_check_manifest.R` (126 lines, generated).
- `tests/testthat/test-generated-check-manifest.R` (the drift test).
- The regeneration step in `.github/workflows/lint.yaml:49` and the `generate-check-manifest` Make target (`Makefile:120`), plus the `document: generate-check-manifest` dependency (`Makefile:115`). Item A2's CI staleness check dies with it, as the issue predicted.

### 2.2 `get_valid_boxpath` and the box.path plumbing

- `R/zzz.R:7-28` (`get_valid_boxpath`) and the `options(box.path = ...)` call in `.onLoad` (`R/zzz.R:47`).
- The box.path shim in `tests/testthat/setup.R` (prepends the checkout's `inst/` and `tests/testthat/modules` so tests exercise the code they sit next to).
- The box.path override in `.lintr.R:26` and the `box.linters::box_usage_linter` wiring (`.lintr.R:78`) including the tests/ exemption (`.lintr.R:88`).
- The developer requirement to set `options(box.path = ...)` in `.Rprofile` (documented in CLAUDE.md/README-dev). This is a recurring worktree footgun today: an `.Rprofile` pinned to the main checkout silently serves stale modules to ad-hoc scripts run in a worktree.

### 2.3 Most of `paths.R`

`inst/artma/paths.R` (117 lines). Deleted: `.find_package_root` (walks parents looking for a DESCRIPTION), `.normalize_box_paths`, `get_pkg_path` (probes box.path entries, then the package root, then `find.package`), the load-time `PACKAGE_PATH <- get_pkg_path()` plus its assert, and the package-tree entries of `PATHS` (`PACKAGE_PATH`, `PROJECT_ROOT`, `DIR_CONFIG`, `DIR_METHODS`, `DIR_OPTIONS`, `DIR_OPTIONS_TEMPLATES`). What survives, as a small runtime function rather than load-time state: the `tools::R_user_dir` user directories (data/config/cache/tmp), the mocks file paths, and the options-template path, which becomes a `system.file()` call. `inst/artma/options/templates/options_template.yaml` is the only non-R asset in the whole tree; it moves to something like `inst/options/templates/` and is resolved with `system.file("options/templates/options_template.yaml", package = "artma")`.

### 2.4 The runtime module crawler

- `inst/artma/modules/utils.R` (100 lines): `crawl_and_import_modules` lists `*.R` files in a directory, builds a `box::use()` statement per file via `turn_path_into_box_import`, `eval()`s it, and checks the result inherits `box$mod`; `validate_runtime_method_modules` then checks each module exposes `run`.
- `inst/artma/modules/path.R` (137 lines): `turn_path_into_box_import` and the candidate-path guessing machinery (`.generate_candidate_paths`, `.get_path_context`, `.resolve_module_path`) that exists to turn a filesystem path back into a box import expression.
- `inst/artma/modules/index.R` (a re-export shim for the two above).
- `tests/testthat/test-modules-path.R`.
- `get_runtime_method_modules` (`inst/artma/modules/runtime_methods.R:291`) survives in name but its body changes from "crawl and eval" to "read the registry" (section 4.1). The `modules_dir` dependency-injection parameter on `invoke_runtime_methods` (`R/artma.R:252`) is test-only and becomes a registry-injection parameter.

### 2.5 box itself

`box (>= 1.2.0)` leaves Imports in DESCRIPTION. `box.linters` leaves the lint toolchain. `tests/testthat/test-box-testthat-imports.R` (an audit that exists because box_usage_linter cannot parse test files) is deleted.

## 3. How the box graph maps onto the package namespace

Rules, in decreasing order of frequency:

| box construct | Occurrences | Replacement |
|---|---|---|
| `box::use(artma / path / mod[fn1, fn2])` | The bulk of the 317 | Deleted. All functions share one namespace; call sites already use bare names, which keep working. |
| `pkg::fun()` calls | Thousands (repo convention) | Unchanged. Needs nothing in NAMESPACE; the package stays in Imports/Suggests. |
| `box::export(...)` | 88 | Deleted. Package-internal visibility becomes flat; the public API stays the 28 roxygen-`@export`ed wrappers in `R/`. The export lists degrade into documentation only (see risk R6). |
| `box::use(pkg[fn, ...])` (external attach) | 5 in inst (`stats` x4, `utils` x1) | `@importFrom stats setNames` etc. in a single `R/artma-package.R` block, or rewrite to `stats::fun()` to match the house style. Recommendation: rewrite to `pkg::fun()`, keeping one convention. |
| `alias = artma / path / mod` (whole-module alias, `alias$fn` calls) | 2 | Rewrite `alias$fn` call sites to bare `fn` names. |
| Dynamic import: `sprintf("box::use(prompts = artma / options / prompts[%s])", opt$prompt_function)` then `eval(parse(...))` (`inst/artma/options/template.R:244-247`) | 1 | `getFromNamespace(opt$prompt_function, "artma")` guarded by an allowlist of prompt-function names. Strictly an improvement: today a template can name any module symbol. |
| `box::file()` (`inst/artma/calc/methods/elliott_cache.R:24`, `MODULE_DIR`) | 1 | Removed with the fingerprint redesign (section 4.3). |
| `box::unload()` in tests (`tests/testthat/test-run.R:58`, `test-runtime-methods.R:145-147`) | 4 | Removed; those tests inject a registry instead of crawling temp directories. |

Net NAMESPACE impact: near zero. Likely additions are `@importFrom rlang .data` (ggplot pronouns, already used consistently, e.g. `inst/artma/methods/funnel_plot.R:365`) and whatever the two Rcpp directives already provide. No `@importFrom` explosion; the issue title's framing ("with @importFrom") overstates what is actually needed because the codebase already qualifies calls.

## 4. What must be redesigned, not just moved

These are the parts where the migration has actual design content. Everything else is file movement plus mechanical rewriting.

### 4.1 Method discovery: crawler to registry

Today `get_runtime_method_modules()` discovers methods by listing `PATHS$DIR_METHODS` and importing each file. In a package namespace there is no directory to crawl at runtime (installed packages ship a lazyload DB, not loose source files).

Replacement: a static registry. One file, `R/methods_registry.R`, holding a named list mapping stage name to the implementation function plus the metadata that `register_runtime_method()` (`inst/artma/modules/runtime_methods.R:91`) attaches today (`depends_on`, `required_columns`, `suggests`, `opt_in`, `label`). The cache wrapping (`cache_cli_runner` + `method_source_hash`) happens at call time inside `get_runtime_method_modules()`, exactly as it effectively does now, so nothing cache-related is baked at install time. Adding a method becomes: add the file, add one registry line. A testthat test asserts the registry covers every `R/method_*.R` file and vice versa, replacing auto-discovery with an enforced convention.

The per-file `run` wrapper (`box::export(my_method, run)`) disappears; the registry holds bare implementations. This also dissolves the largest name collision (15 `run` definitions, section 5).

### 4.2 Load-time state and install-time evaluation

In a package, top-level expressions evaluate once at install time and get serialized into the lazyload DB. Three current load-time computations are unsafe under that model and must become runtime calls:

- `PACKAGE_PATH <- get_pkg_path()` and the `PATHS` list (`inst/artma/paths.R:77-116`): would bake the build machine's paths. Becomes an `artma_paths()` function (memoisable per session) built on `system.file()` and `tools::R_user_dir()`.
- `MODULE_DIR <- box::file()` (`elliott_cache.R:24`): same class of bug.
- `CONST` (`inst/artma/const.R`) is a static list with no filesystem content and is safe to keep as a package constant.

A migration gate should scan for top-level calls other than function definitions and literal constants in every migrated file (a 20-line parse script; the generator's own `.cm_parse_file` pattern is reusable).

### 4.3 Cache fingerprints

`package_source_fingerprint()` (`inst/artma/libs/infrastructure/source_fingerprint.R`) hashes every `.R` file under `PATHS$PROJECT_ROOT`; `method_source_hash()` hashes `DIR_METHODS/<stage>.R`. Installed packages do not ship those files, so both break.

Replacement: hash deparsed function bodies from the namespace. `package_source_fingerprint()` becomes a hash over `deparse()` of all namespace functions sorted by name; `method_source_hash(stage)` hashes the registry entry's implementation (plus its declared helpers if we want precision). This works identically under `devtools::load_all()` and an installed package, and is more precise than today: comment-only edits stop invalidating caches. `elliott_cache.R` additionally fingerprints the C++ kernel's source file; `src/` is also absent from installed packages, so that entry folds in `packageVersion()` plus the deparsed R wrapper of `simulate_cdfs_block_cpp` instead. Every existing user cache invalidates once when the fingerprint scheme changes; that is the safe direction.

### 4.4 roxygen exposure

The 4,040 roxygen comment lines in inst have never been processed by roxygen2. Once the files sit in `R/`, roxygen parses all of them: every documented object without `@noRd` generates an Rd file, and the 42 stray `#' @export` tags (in `paths.R`, `const.R`, `options/utils.R`, `options/template.R`, `variable/suggestion.R`, `variable/detection.R`, `output/ma_table.R`, `libs/core/utils.R`, `libs/core/file.R`, `libs/core/validation.R`, `libs/core/autonomy.R`, `libs/infrastructure/polyfills.R`, `interactive/effect_summary_stats.R`, `interactive/save_preference.R`, `data/mock.R`, `econometric/bma.R`, `econometric/fma.R`) would silently widen the public API. Mitigation is mechanical: scrub `@export` from internal blocks and mass-add `@noRd` during each tranche's move script. A NAMESPACE snapshot test (exported set == the 28 public names + Rcpp entries) rides along from tranche 1.

### 4.5 Lint and style toolchain

`object_usage_linter` is disabled today as incompatible with box (`.lintr.R:43`) and replaced by `box_usage_linter`. Post-migration the default linter comes back on, giving real usage analysis over all 26k lines for the first time; expect an initial wave of findings (unused arguments, misspelled variables in rarely-hit branches) that today nothing can catch. Budgeted in the estimate. Also: `styler::style_pkg()` finally covers everything (today it skips inst/artma, a documented footgun requiring per-file `style_file` calls).

## 5. Name collisions

Measured by parsing every top-level assignment in the 95 files: 651 definitions, exactly 3 names defined in more than one module:

- `run`: 15 times, one per `inst/artma/methods/*.R`. Dissolved by the registry (section 4.1).
- `build_summary_table`: `econometric/linear.R` and `econometric/nonlinear.R`. Already issue item B2 (shared builder); if B2 lands first the collision is gone, otherwise rename during the move.
- `format_estimate`: `econometric/nonlinear.R` vs `libs/formatting/results.R`. Already item B7, same handling.

This is the strongest single piece of evidence for "go": the module system's namespacing is doing almost no isolation work. In a flat package namespace, duplicate top-level definitions silently overwrite each other in collation order, so a duplicate-name gate (the same parse script as the measurement, as a testthat test) stays in the suite permanently.

## 6. File-by-file migration order

Strategy: bottom-up by dependency layer, one tranche per PR, full gates per tranche. The mechanism that makes tranches independent is a **shim**: when a module moves to `R/`, its old box file is replaced by a two-line shim (`fn <- getFromNamespace("fn", "artma")` per export, then `box::export(...)`), so every not-yet-migrated module and every test keeps importing the old path unchanged. Shims are deleted in the final tranche. This avoids touching the 77 test files and the remaining modules until their own tranche.

| Tranche | Contents | Files | Notes |
|---|---|---|---|
| 0 | Prep: rewrite scripts (box::use stripping, `@noRd` mass-add, shim generator, duplicate-name and top-level-state gates as tests), E2E baseline run recorded | 0 moved | The generator's parser (`.cm_find_box_use_calls`) is reusable for the rewrite tooling |
| 1 | `const.R`, `libs/core/` (validation, utils, string, file, autonomy) | 6 | Leaf layer; nothing here imports other artma modules except paths (autonomy reads options only) |
| 2 | `libs/formatting/`, `libs/infrastructure/` (cache, output_files, source_fingerprint), `paths.R` | 6 | Fingerprint redesign (4.3) and `artma_paths()` (4.2) land here; template yaml moves under `inst/` |
| 3 | `options/` (9) minus templates, `data_config/` (6) | 15 | Dynamic prompt import rewritten (section 3); `R/options.R` and `R/aaa.R` imports collapse |
| 4 | `data/` (13), `variable/` (3) | 16 | `cache_signatures.R` follows the tranche-2 fingerprint API |
| 5 | `econometric/` (7), `calc/` (8) | 15 | elliott_cache fingerprint rework; numeric outputs pinned by existing characterization tests before the move |
| 6 | `visualization/` (7), `output/` (3), `interactive/` (7) | 17 | |
| 7 | `modules/`: `method_execution.R`, `runtime_methods.R` as registry; delete `utils.R`, `path.R`, `index.R`; update `R/artma.R` orchestrator | 5 | The one tranche with real design content (4.1) |
| 8 | `methods/` (15 files, 5,609 lines) + registry entries; drop `run` wrappers | 15 | Mostly mechanical; method outputs pinned by existing tests and E2E |
| 9 | Tests: rewrite the 77 files' `box::use()` headers to `artma:::` bindings; convert `tests/testthat/modules/testing` scaffolding to testthat `helper-*.R`; delete `setup.R` box.path shim, `test-box-testthat-imports.R`, box::unload usage | 0 (pkg) | Mechanical, scripted; review rule: no logic edits allowed in this tranche |
| 10 | Teardown: delete all shims, generator + manifest + drift test, `get_valid_boxpath`, lint.yaml regeneration step, box from Imports, box.linters from `.lintr.R`, re-enable `object_usage_linter`; update CLAUDE.md/AGENTS.md/README-dev; codecov threshold adjustment; NEWS entry; minor version bump | | Lint-wave cleanup happens here |

Ordering constraints inside the table: 2 after 1 (infrastructure imports core), 4 after 3 (data reads options), 7 after 2 (registry uses cache + fingerprints), 8 after 7, 9 and 10 last. Tranches 3 to 6 are internally reorderable.

## 7. testthat implications

- 77 of 78 test files import internal modules via `box::use()`. Until tranche 9 they keep working through the shims; tranche 9 rewrites headers to `artma:::` bindings (works under both `devtools::load_all()` and the installed package that R CMD check tests against). Parallel testthat (`Config/testthat/parallel: TRUE`) is unaffected.
- Tests that exercise the crawler by writing temp method files (`test-run.R`, `test-runtime-methods.R`) are rewritten against registry injection; they get simpler (no `eval`, no `box::unload` cache surgery).
- The test-only scaffolding (`tests/testthat/modules/testing/`: fixtures, mocks) becomes plain `helper-*.R` files, the standard testthat mechanism; that removes the second box.path root the current `setup.R` maintains.
- `test-box-testthat-imports.R`, `test-modules-path.R`, `test-generated-check-manifest.R`: deleted with the machinery they audit.
- New permanent tests: duplicate-top-level-name gate, NAMESPACE snapshot, registry-covers-method-files, no-filesystem-state-at-top-level scan.

## 8. covr and R CMD check implications

**covr.** `covr::package_coverage()` (Makefile:165, `.github/workflows/test-coverage.yaml`) instruments the package namespace. Box modules are sourced at runtime from installed data files and are invisible to it, so today's coverage number measures the ~2.4k lines of `R/` wrappers plus `src/`, not the 25.9k lines that do the work. After migration all of it is instrumented. Consequences: the reported percentage will change discontinuously (in either direction) and codecov.yml thresholds need a one-time reset; the coverage CI job gets slower (instrumentation across 95 more files); coverage-guided work becomes meaningful for the first time.

**R CMD check.** Gains, which are the point of the exercise: codetools sees every symbol (undefined names, unused locals, wrong-arity calls in dead branches), Imports usage is checked for real (the keep-alive hack dies), Rd cross-checks apply. Costs: an initial wave of NOTEs from code that has never been statically analyzed; a small hand-maintained `globalVariables()` for `.data` if not imported from rlang (ggplot calls already use the `.data$` pronoun consistently, so the NSE exposure is minimal, verified in the six plotting files that call `aes()`); install time grows (byte-compiling 26k more lines) while load time shrinks (no box crawling and `eval` at first use). CRAN posture strictly improves: shipping the implementation as loose scripts under inst is unusual and hides everything from check.

## 9. Risk register

| # | Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|---|
| R1 | Behavioral drift during a move (load-time vs call-time evaluation, environment capture differences) | Medium | High | Shim strategy keeps each tranche small; full `make test` + E2E per tranche; existing characterization tests pin method outputs; pure-move rule: no logic edits in move commits |
| R2 | Cache under-invalidation after the fingerprint redesign (stale results served) | Low | High | New fingerprints are strictly derived from the namespace, and the whole-package hash still covers shared helpers; unit tests for the new hashing; one deliberate global invalidation on release |
| R3 | Silent last-write-wins on duplicate top-level names, now or in the future | Low (3 known today, all accounted for) | High | Permanent duplicate-name gate test from tranche 0; B2/B7 land first or renames happen in-move |
| R4 | Accidental API widening via the 42 stray `@export` tags or roxygen processing | Medium | Medium | Mechanical `@export` scrub + `@noRd` mass-add in the move script; NAMESPACE snapshot test |
| R5 | Long-running migration collides with parallel feature work | Medium | Medium | Tranche PRs merge within days; per-directory freeze during that directory's tranche; sequencing condition 1 |
| R6 | Loss of module-boundary information (box::export lists documented intended visibility) | Certain | Low | Dot-prefix convention for private helpers during the move; the measured coupling (section 5) shows boundaries carry little load today |
| R7 | Dev-ergonomics regressions: no per-module `box::reload`, worktree box.path habits invalidated | Certain | Low | `devtools::load_all()` covers reload; the worktree box.path footgun class disappears entirely, a net win |
| R8 | Install-time baking of filesystem state (section 4.2) | Medium | High | Top-level-state scan as a gate test; the three known offenders redesigned in tranches 2 and 5 |
| R9 | Coverage/check NOTE churn destabilizes CI for a while | High | Low | Threshold resets and the lint/NOTE cleanup are explicit tranche-10 work items, not surprises |
| R10 | Estimate blows up on unknown box semantics deep in options/interactive code | Low | Medium | The prep tranche's rewrite scripts surface exotic constructs (dynamic import, aliases) before any move; the full-tree grep in this document found one dynamic import and two aliases |

## 10. Effort estimate

Basis: 95 files / 25.9k lines to move, but the rewriting is scriptable (strip `box::use` headers, strip `box::export`, add `@noRd`, generate shims), and gates are already automated.

| Tranche | Estimate |
|---|---|
| 0 (tooling + gates) | 1 day |
| 1, 2 (core + infra, incl. fingerprint and paths redesign) | 1.5 days |
| 3 to 6 (options, data, econometric, viz; mechanical) | 2 to 3 days total |
| 7 (registry redesign) | 1 day |
| 8 (methods) | 0.5 to 1 day |
| 9 (tests) | 1 day |
| 10 (teardown + lint wave + docs) | 1 to 1.5 days |
| Contingency (~30%) | 2 days |
| **Total** | **8 to 12 working days, ~10 PRs, 2 to 3 calendar weeks** |

The estimate assumes agent-assisted execution with the repo's existing gate discipline (styler, lint, full test suite, E2E on method-touching tranches) and no parallel structural work in the directories being moved.

## 11. What we give up

Honesty section. box currently provides: per-module private scope (measured value: low, 3 collisions in 651 names), lazy per-module loading (replaced by lazyload DB, which is faster), runtime discovery of method files (replaced by an explicit registry plus a conformance test; third-party drop-in method directories were never a supported feature, `modules_dir` is test-only DI per its own docs in `R/artma.R:239`), and Python-style import ergonomics (a genuine style preference some contributors may miss). The migration also deletes the sub-systems listed in section 2, which exist only to make box coexist with R tooling; none of them are features.

## 12. Recommendation

Go. Schedule it as its own arc after the current #322 tranches merge, execute strictly in the tranche order above, and treat tranche 0's gate tests as non-negotiable. The package pays a permanent tax for box (a 233-line generator producing a 126-line file to keep check quiet, three path-resolution mechanisms, a linter carve-out, invisible coverage) while the measured benefit of module isolation is three name collisions and five import specs. The migration converts a recurring cost into a bounded one.
