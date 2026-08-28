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

A call that names no options file tries to resume the last used one before
opening the menu; see "Resuming the last used file" below.

The hub itself is not gated by `should_prompt_user()`: it is the interactive
experience the user asked for by calling `artma()` without methods. Autonomy
keeps governing the prompts inside the flows the hub launches (column mapping,
package installs, and so on).

## Bound and unbound sessions

A hub session is **bound** when an options file is loaded and **unbound** when
none is. `artma(options = "x.yaml")` enters bound, with its data already
prepared; `artma()` with no options file first tries to resume the last used
one (see below) and enters unbound, with `df = NULL` and no options file
behind it, when that yields nothing.

### Resuming the last used file

Every successful bind writes the options file's name into a
`.last_options_file` marker inside the options directory (next to the
`.welcome_shown` flag, so it is per-directory automatically): the menu's
`bind()` helper writes it through `file_actions$remember_last_used`, and the
linear path (`artma(options = "x.yaml")`, or a prompted pick) writes it at the
top of `main()` in `R/artma.R`. The marker helpers live in
`inst/artma/options/last_used.R` and tolerate a missing or unreadable marker.

A bare interactive `artma()` call reads the marker back
(`restore_last_options_file()` in `R/artma.R`, inside the `runtime_setup()`
extent so the loaded options apply the way a mid-session bind does). When the
marker names an existing file, the session enters bound to it with a one-line
notice ("Resuming on <file>"); its options are loaded eagerly but its data is
not prepared: `ensure_data()` prepares lazily on first need, so a moved data
source cannot wreck session entry. On any failure (no marker, the file is
gone, the load errors) one info line is printed and the session enters unbound
exactly as before; a marker naming a file that no longer exists is cleared,
while a load error keeps it. An explicit `options` argument never consults the
marker.

Deleting options files through the menu prunes the marker
(`file_actions$prune_last_used`): a marker naming a deleted file is cleared,
whether or not that file was the one the session ran on.

### The unbound entry

A session that does enter unbound (nothing was restored) does not open on the
menu. Before the first menu pass, `run_session_hub()` runs
`run_unbound_entry()` (`inst/artma/interactive/options_file_menu.R`):

- **No options files exist** (a true first-timer): the guided create flow
  (`options_create()` via `file_actions$create`) runs straight away, after the
  welcome banner. On success the new file is bound immediately and the session
  lands in the bound menu ready to run; a cancel or failure falls back to the
  unbound menu.
- **Files exist**: the file picker (`ask_for_file_to_load()`, which carries
  its own create and back entries) opens directly. Picking a file binds it and
  lands in the bound menu; backing out lands in the unbound menu.

Either way the unbound menu remains reachable, so the user can still create,
repair, compare or delete files from its options-file item before committing
to one.

Unbound is not a stripped-down mode with its own rules; it is the ordinary
state minus the items that need data:

- `runtime_setup(allow_unbound = TRUE)` applies the template defaults through
  `unbound_runtime_options()` (`R/aaa.R`), with `artma.temp.file_name`
  explicitly `NULL`. Every `getOption("artma.<x>", <default>)` in the package
  therefore reads what it would read under a fresh options file, and
  `artma.temp.file_name` is the single marker of the unbound state.
- `hub_menu_items()` offers only the options-file item (first, so the cursor
  opens on it), Settings, Help and Exit. `Run methods`, `Re-run`, `Adjust
  options`, `Preview data` and `Results` appear once a file is loaded. There is
  no preselection call involved: `ask_select(default = ...)` also decides what
  an empty selection returns, and the hub needs an empty selection to mean
  Exit, so item order alone puts the cursor where it belongs.
- Data is prepared lazily by `ensure_data()`, the same path an edited `data.*`
  option takes: "no frame yet" and "the frame is stale" are one state machine.
  A failed preparation reports and returns to the menu instead of aborting the
  session, so a bad data source can be fixed and retried in place.

## Menu items

Rendered with `ask_select` over value-keyed choices: labels are the padded
item name plus a dim description, values are stable action keys, and the loop
dispatches on values only (`compose_menu_choices()` in
`interactive/menu.R`, shared with the submenus). A header rule above the menu
names the options file and the prepared data's dimensions, or says "no options
file loaded" / "data not prepared yet" when the session has neither.

The items below are the bound session's. An unbound one shows only the
options-file item, Settings, Help and Exit.

- **Run methods**: opens the metadata-decorated method picker
  (`interactive/method_picker.R`) fed by `build_methods_table(available_for =
  df)`, then runs the selection through the full run and summarize steps. The
  picker always opens with nothing preselected; the confirmed selection is
  remembered for Re-run only (also mirrored into `artma.temp.last_methods`,
  which the linear path reads as its own default). An empty confirmation
  returns to the menu without running.
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
- **Preview data**: prepares the data if the session has none yet, then prints
  a textual summary of the frame (rows,
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
- **Options file** (`interactive/options_file_menu.R`): the submenu behind
  everything to do with options files. First in an unbound session, after
  Settings in a bound one (labelled with the loaded file, "switch, create,
  edit, delete"); a bound session always keeps the item, so an auto-restored
  or entry-bound session is never trapped on its file. While unbound the
  item's wording follows the file count (`hub_menu_items(n_options_files =
  ...)`, fed by a `file_actions$list()` call per unbound menu pass): "Get
  started" with a create-your-options-file description when no files exist,
  "Choose options file" when some do. See "The options-file menu" below.
- **Help**: submenu with the methods overview (`print_methods_table()` on the
  same frame the picker uses), the options overview (`artma::options_help()`
  plus a pointer to its single-option form), and the browser links that used
  to live in the welcome flow: the Getting Started and Options Files
  vignettes and the package website. The welcome message itself is now just a
  first-run banner pointing at this submenu (`is_first_time_user` /
  `mark_welcome_as_shown` semantics are unchanged).
- **Exit**: leaves the loop. Cancelling the menu (Esc) behaves exactly like
  Exit; the accumulated results are the user's work and survive a cancel.

## The options-file menu

`run_options_file_menu()` (`inst/artma/interactive/options_file_menu.R`) is a
loop over the file actions. It returns as soon as the session's options file
changes (the hub has a header to redraw and data to prepare) or the user backs
out; the management actions keep it open, so a create can be followed by a
select without leaving the submenu. Its return value is `list(file, changed)`:
the session's file afterwards (`NULL` when it was deleted) and whether the
loaded options changed.

- **Select / Switch file**: the decorated picker over
  `options_list(details = TRUE)` (file name, data source basename, last run
  time, count of non-default options; the current file marked), plus a create
  entry so an unbound session never dead-ends. Hidden when no files exist.
- **Create, Duplicate, Edit, Repair, Compare, Open, Delete**: thin glue over
  `options_create()`, `options_copy()`, `options_modify()`, `options_fix()`,
  `options_diff()`, `options_open()` and `options_delete()`, bound to the
  session's options directory by `default_file_actions()`. A failure is
  reported and leaves the menu open.
- A newly created file is loaded straight away when the session is unbound
  (it is the thing the session was missing) and offered when it is bound.
- `run_unbound_entry()` lives in the same module and reuses the picker and
  the bind-with-reporting behavior for the unbound session's entry shortcut
  (see "Bound and unbound sessions" above). It returns the same
  `list(file, changed)` contract as `run_options_file_menu()`.
- Editing or repairing the file the session is running on **reloads** it, so
  the session cannot keep values the file no longer holds.
- Deleting the file the session is running on leaves the session unbound; the
  hub also clears `artma.temp.file_name`, so nothing keeps deriving paths from
  a file that no longer exists.

### Loading a file mid-session

`runtime_setup()` applies the loaded options via `withr::local_options()`,
which restores the caller's options when `artma()` returns. A mid-session load
therefore applies the new file's options with plain `options()`: the values
survive until the hub exits, and `runtime_setup()`'s frame still restores the
caller's state afterwards (the option keys are the same template keys, so the
restore list covers them).

The handler lives in `R/artma.R` (`bind_options` wired into
`run_session_hub()`): it migrates legacy files, offers the same outdated-file
repair the linear path offers, loads the file
(`options_load(should_add_temp_options = TRUE)`, so the header names it) and
applies the options. It never prepares data. The hub invalidates the frame
instead and calls `rebuild_data` (`prepare_run_context()` in `R/artma.R`),
which keeps loading and preparing separable: a file that loads but whose data
fails to prepare leaves the session on the new file with no frame, ready to
retry after an option edit.

The run context lives in a mutable `session_state` environment shared by
`rebuild_data` and the run closure, so subsequent hub runs see the latest
context. Each fresh prepare step's capture frame nests inside the first one the
session opened, which the `on.exit` safety net closes together with everything
above it. Every data change also drops the hub's self-built methods table so
availability is re-checked against the new frame (an injected `methods_table`
is kept as-is).

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
- New options-file actions: add an entry in `options_file_menu_items()`, a
  branch in `run_options_file_menu()`, and the real implementation in
  `default_file_actions()`.
- Everything side-effectful is injectable for tests: `select_fn` /
  `checkbox_fn` (menu backends), `run_methods` (the pipeline), `rebuild_data`
  (the lazy preparation), `edit_option` / `save_preference` (the
  adjust-options prompts), `view_data`, `open_results`, `render_report`,
  `methods_table` (the picker frame), `template_path`, `bind_options` /
  `file_actions` / `options_file` (the options-file menu; `file_actions` also
  carries `remember_last_used` / `prune_last_used`, the last-used-marker
  maintenance), `set_theme` / `set_autonomy` (settings), and
  `show_options_help` / `open_url` (help).
  `tests/testthat/test-session-hub.R` and
  `tests/testthat/test-options-file-menu.R` show the scripted-backend
  pattern.
