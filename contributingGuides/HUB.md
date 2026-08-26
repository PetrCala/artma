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
- **Exit**: leaves the loop. Cancelling the menu (Esc) behaves exactly like
  Exit; the accumulated results are the user's work and survive a cancel.

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
  `methods_table` (the picker frame), and `template_path`.
  `tests/testthat/test-session-hub.R` shows the scripted-backend pattern.
- Planned growth (issue #496): settings, switch options file, and help items.
