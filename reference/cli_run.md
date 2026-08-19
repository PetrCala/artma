# Run the artma command-line interface

Scriptable entry point behind the `artma` launcher and
`Rscript -e 'artma::cli_run()'`. It parses an argument vector,
dispatches to the matching public function
([`artma()`](https://petrcala.github.io/artma/reference/artma.md),
[`methods_list()`](https://petrcala.github.io/artma/reference/methods_list.md),
[`options_validate()`](https://petrcala.github.io/artma/reference/options_validate.md)
/
[`options_create()`](https://petrcala.github.io/artma/reference/options_create.md)
/
[`options_list()`](https://petrcala.github.io/artma/reference/options_list.md),
or the package version) and returns an exit code. No analysis logic
lives here: each subcommand is a thin translation to the existing API.

The function never calls [`quit()`](https://rdrr.io/r/base/quit.html);
it returns the exit code invisibly so it can be driven in-process by
tests. The launcher script turns that code into a process exit status.

Exit codes:

- `0` success (including `--help`).

- `1` an R error was raised while dispatching (message printed to
  stderr).

- `2` a usage error (unknown command, unknown flag, malformed value);
  usage is printed to stderr.

Subcommands: `run`, `methods`, `options` (with sub-actions `validate`,
`create`, `list`) and `version`. Every subcommand accepts `--help`.

Flags for `run` become an in-session
[`options()`](https://rdrr.io/r/base/options.html) overlay applied
around the
[`artma()`](https://petrcala.github.io/artma/reference/artma.md) call,
so the user's YAML options file is never mutated: `--data` sets
`artma.data.source_path`, `--output-dir` sets `artma.output.dir`,
`--verbose` sets `artma.verbose`, `--no-cache` sets
`artma.cache.use_cache` to `FALSE`, and `--report` sets
`artma.output.report` to `TRUE`. `--options`, `--options-dir` and
`--methods` are forwarded as
[`artma()`](https://petrcala.github.io/artma/reference/artma.md)
arguments.

In `--json` mode stdout carries only a JSON run manifest (`methods_run`,
`methods_skipped` with reasons, `output_dir`, `exported_files`, `seed`,
`package_version`), read back from the `run.json` the run wrote; all
human-readable output is routed to stderr.

## Usage

``` r
cli_run(args = commandArgs(trailingOnly = TRUE))
```

## Arguments

- args:

  *\[character, optional\]* The argument vector to parse. Defaults to
  `commandArgs(trailingOnly = TRUE)`.

## Value

*\[integer\]* The exit code (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
# List available methods
artma::cli_run("methods")

# Run two methods against an options file, emitting a JSON manifest
artma::cli_run(c(
  "run", "--options", "my_analysis.yaml",
  "--methods", "funnel_plot,effect_summary_stats", "--json"
))
} # }
```
