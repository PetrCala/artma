# Interactive prompts

All interactive input in artma goes through the helpers in
`inst/artma/interactive/input.R`. Do not call `readline()`,
`climenu::select()`, or `climenu::checkbox()` directly in new code; wrap the
question in one of these instead:

| Helper | Use for | Returns |
| --- | --- | --- |
| `ask_text()` | free-text answers | `character` |
| `ask_select()` | picking one item from a menu | the selected value, or `character(0)` |
| `ask_checkbox()` | picking several items from a menu | the selected values, or `character(0)` |
| `ask_yes_no()` | yes/no confirmations | `logical` |

## Layout contract

Every prompt renders the same way:

```text
Question line (short; cli inline markup allowed)
  dim hint line: examples, format notes, pointers to full help

[default] > _
```

- The question is one short line. Context, examples, and pointers go into
  `hints`, which render dim and indented. Do not print `cli_h1` banners or
  paragraph blocks per question; a `cli_h3` section header once per flow is the
  most a prompt should add.
- Text prompts show the default inside the input row (`[default] > `); menus
  preselect it. Never write "press Enter to accept the default" hints.
- The input row of `ask_text` is ANSI-free on purpose: styled readline prompts
  break line editing in some terminals.
- For menus, hints render above the question because `climenu` prints the
  question directly over the choices. This holds for both `ask_select` and
  `ask_checkbox`.
- Menu labels stay plain and short; per-item annotations go into the
  `descriptions` argument, which `climenu` renders as an aligned dim column
  (and never echoes or returns). Do not pad, truncate, or `cli::col_*` a
  label yourself: `compose_menu_choices()` (`inst/artma/interactive/menu.R`)
  turns menu-item lists into the `choices` + `descriptions` pair.

## Semantics

- **Empty answer**: falls back to `default` when one is given. Without a
  default, `ask_text` re-asks (unless `allow_empty`) and `ask_select` returns
  `character(0)`, leaving the decision to the caller. `ask_checkbox` is the
  exception: its `default` only preselects items, and a cancelled menu or an
  empty confirmation always returns `character(0)`, because "the user
  deselected everything" is a real answer.
- **Retries**: `ask_text` re-asks on empty or invalid answers up to
  `max_retries` (default 3) and then gives up with `""`. Callers treat `""` as
  "no answer" and decide whether to warn, skip, or abort.
- **Validation**: pass `validate = function(answer) NULL | "error message"` to
  `ask_text`; rejected answers show the message and re-ask. Pass
  `sanitize = function(answer) answer` to clean raw input (it runs before the
  default substitution, so a sanitizer that empties the answer still falls
  through to the default). The default value itself is never sanitized or
  validated.
- **Aborting on "No"** stays at the call site: the caller owns the
  context-specific abort message. `ask_yes_no` only returns a logical.
- **Non-interactive sessions**: the helpers abort. Callers gate prompting with
  `interactive()` and `should_prompt_user()` (see the autonomy system in
  `CLAUDE.md`); the helpers deliberately do not check autonomy themselves.
- **Confirmation echo**: `climenu` echoes the plain label of the picked item
  once; the helpers add nothing. Never route a styled or composed display
  string through `{.val}`-class inlines (`{.val}`, `{.file}`, `{.path}`,
  `{.url}`, `{.email}`): they `encodeString()` their input, so ANSI styling
  surfaces as literal `\033[90m...` text wrapped in quotes. Use `{.strong}`
  or plain interpolation for anything already styled or formatted.

## Testing

The backends are injectable, which is the sanctioned mocking pattern (box
module namespaces are locked, so binding mocks do not work):

```r
ask_text("Name", read_input = function(prompt) "typed answer")
ask_select("Pick", choices = c("A" = "a"), select_fn = function(choices, prompt, selected, descriptions) "a")
ask_checkbox("Pick", choices = c("A" = "a"), checkbox_fn = function(choices, prompt, selected, allow_select_all, descriptions) "a")
```

The backends receive the named `choices` vector as-is and return the selected
value(s), not labels; `descriptions` arrives as a parallel character vector or
`NULL`.

Higher-level prompts thread these through: `prompt_user_for_option_value()`
takes `read_input` and `choose_path`, and the `prompt_*` functions in
`inst/artma/options/prompts.R` forward `...` to `ask_select`. See
`tests/testthat/test-interactive-input.R` and
`tests/testthat/test-options-prompt-value.R`.

## Option value prompts

`prompt_user_for_option_value()` (`inst/artma/options/template.R`) renders
template options with: the option name as the question, the type and a one-line
help summary as hints, and a pointer to `artma::options_help("<name>")` for the
full help text. File/directory prompts add the `choose` token (graphical
picker) and honor `prompt_hint` from the template. Custom menus are declared
with `prompt: "function"` in the template and live in
`inst/artma/options/prompts.R`.

## Not yet migrated

The existing multi-select sites (`inst/artma/options/ask.R`,
`inst/artma/methods/best_practice_estimate.R`, `inst/artma/methods/bma.R`,
`inst/artma/interactive/effect_summary_stats.R`,
`inst/artma/interactive/prima_facie_graphs.R`,
`inst/artma/data/interactive_mapping.R`, `R/artma.R`) still call
`climenu::checkbox` directly, as do the bespoke menus in
`inst/artma/data/schema_ui.R`, `inst/artma/data/interactive_mapping.R`
(provisional-mapping menus), `inst/artma/interactive/welcome.R`, and
`inst/artma/interactive/box_plot.R`. New code uses `ask_checkbox` and the rest
of this contract; migrating the sites above is opportunistic.
