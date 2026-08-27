# The interactive session hub

The hub is the menu loop `artma()` enters instead of the linear
select-run-exit flow: one session, many runs. It lives in
`inst/artma/interactive/hub.R` (`run_session_hub()`); `R/artma.R` wires it to
the pipeline steps (`prepare_run_context()` -> `execute_run()` ->
`summarize_run()`).

## Entry conditions

The hub opens only when `interactive() && is.null(methods)`. Every other path
stays linear and unchanged:

- `artma(methods = ...)` runs the given methods once and returns.
- Non-interactive sessions never see the hub (nor any prompt).
- The CLI is unaffected.

The hub itself is not gated by `should_prompt_user()`: it is the interactive
experience the user asked for by calling `artma()` without methods. Autonomy
keeps governing the prompts inside the flows the hub launches (column mapping,
package installs, and so on).

## Menu items (v1)

Rendered with `ask_select` over value-keyed choices: labels are the padded
item name plus a dim description, values are stable action keys, and the loop
dispatches on values only. A header rule above the menu names the options file
(`artma.temp.file_name`) and the prepared data's dimensions.

- **Run methods**: opens the metadata-decorated method picker
  (`interactive/method_picker.R`) fed by `build_methods_table(available_for =
  df)`, then runs the selection through the full run and summarize steps. The
  confirmed selection is remembered for preselection (also mirrored into
  `artma.temp.last_methods`) and for Re-run. An empty confirmation returns to
  the menu without running.
- **Re-run last selection**: repeats the previous selection without the
  picker. Hidden until a first run happened; its description names the
  selection it would repeat, plus the options changed since that run.
- **Adjust options** (`run_adjust_options()`): a picker over the curated
  re-tuning knobs (`CURATED_OPTION_PATHS`: winsorization, NA handling,
  precision measure, seed, decimals, report) plus the `methods.<name>` groups
  of the last selection, each labeled with its current value and, when it
  deviates, the template default. A "Browse all options" entry walks the whole
  template (group menu, then leaf menu). Edits go through
  `prompt_user_for_option_value()` (typed validation, current value as the
  default) and apply session-wide via `options()` immediately;
  `prompt_save_preference()` (with `respect_autonomy = FALSE`; the user
  explicitly opened the flow) then decides session-only vs writing the options
  YAML. Editing any `data.*` option marks the prepared frame stale: the hub
  re-prepares it lazily, right before the next run (via the injected
  `rebuild_data`, wired to `prepare_run_context()` in `R/artma.R` so the run
  pipeline picks the fresh frame up too), and says so. Non-data edits never
  trigger a rebuild.
- **Preview data**: prints a textual summary of the prepared frame (rows,
  columns, study count when the config resolved `study_id`, missing-value
  counts, effect and SE ranges), then offers the spreadsheet viewer (via
  `artma::data_preview()`) when one is available.
- **Results**: submenu with "Open results folder" (`artma::results_open()`)
  and "Render HTML report" (`artma::report_render()` on the accumulated
  results). Both print a friendly message and do nothing before the first run.
  Every hub run overwrites the previous one's output in place, as scripted
  runs always have; with `output.run_subdirectories` enabled each run instead
  gets its own `<output.dir>/runs/<timestamp>/` (claimed in `execute_run()`,
  so one prepared context still yields one directory per run) and both
  submenu items act on the latest one.
- **Settings**: submenu of session-level toggles, each showing its current
  value. Visualization theme (`artma::viz_set(theme = ...)` over
  `artma::viz_themes()`), verbosity (`artma.verbose`, 1-4), autonomy level
  (`artma::autonomy_set()`), and result caching (`artma.cache.use_cache`,
  which `cache_cli()` reads at call time). All of them take effect for the
  session only; the options file on disk is untouched.
- **Switch options file**: picker over `artma::options_list(details = TRUE)`
  with decorated, value-keyed labels: file name, data source basename, last
  run time, and the count of non-default options; the session's current file
  is marked. See "Switching the options file" below for the semantics.
- **Help**: submenu with the methods overview (`print_methods_table()` on the
  same frame the picker uses), the options overview (`artma::options_help()`
  plus a pointer to its single-option form), and the browser links that used
  to live in the welcome flow: the Getting Started and Options Files
  vignettes and the package website. The welcome message itself is now just a
  first-run banner pointing at this submenu (`is_first_time_user` /
  `mark_welcome_as_shown` semantics are unchanged).
- **Exit**: leaves the loop. Cancelling the menu (Esc) behaves exactly like
  Exit; the accumulated results are the user's work and survive a cancel.

## Switching the options file

`runtime_setup()` applies the loaded options via `withr::local_options()`,
which restores the caller's options when `artma()` returns. A mid-session
switch therefore applies the new file's options with plain `options()`: the
values survive until the hub exits, and `runtime_setup()`'s frame still
restores the caller's state afterwards (the option keys are the same template
keys, so the restore list covers them).

The handler lives in `R/artma.R` (`switch_options` wired into
`run_session_hub()`): it migrates legacy files, loads the new file
(`options_load(should_add_temp_options = TRUE)`, so the header names the new
file), applies the options, and re-runs `prepare_run_context()`. The current
run context lives in the same mutable `session_state` environment
`rebuild_data` uses, so subsequent hub runs see the latest context; the fresh
prepare step's capture frame nests inside the session frame the first prepare
opened, which the `on.exit` safety net closes together with everything above
it. On success the hub swaps in the freshly prepared frame, clears any
pending data staleness from earlier option edits, and drops its self-built
methods table so availability is re-checked against the new data (an injected
`methods_table` is kept as-is). A failed listing, load, or re-preparation
reports and leaves the session on its current file.

Each run goes through the full run step exactly as a linear run does: export,
manifest, and run summary. `R/artma.R` opens a fresh output-file capture frame
per hub run (nested inside the session frame the prepare step opened), so
every `run.json` describes its own run. A run that errors is reported and
returns to the menu; it never tears down the session.

## Return contract

The hub returns (invisibly) the accumulated results:

- One entry per method, keeping the latest result per method across runs (the
  existing `artma()` contract).
- `failed_methods` / `skipped_methods` attributes hold the latest status per
  method: re-requesting a method drops its previous entries before the new
  run's entries are appended.
- `run_info` describes the latest run (unchanged shape).
- A `runs` attribute lists every run: one entry with `methods`, `seed`,
  `timestamp`, and `options_changed` (the options edited since the previous
  run).

Exiting before any run returns an empty list with an empty `runs` attribute.

## Extension points

- New menu items: add an entry in `hub_menu_items()` and a matching value
  branch in the `run_session_hub()` loop. Keep choices value-keyed; never
  match on labels.
- Everything side-effectful is injectable for tests: `select_fn` /
  `checkbox_fn` (menu backends), `run_methods` (the pipeline), `rebuild_data`
  (the stale-data re-preparation), `edit_option` / `save_preference` (the
  adjust-options prompts), `view_data`, `open_results`, `render_report`,
  `methods_table` (the picker frame), `template_path`, `list_options` /
  `switch_options` (the options-file switch), `set_theme` / `set_autonomy`
  (settings), and `show_options_help` / `open_url` (help).
  `tests/testthat/test-session-hub.R` shows the scripted-backend pattern.
