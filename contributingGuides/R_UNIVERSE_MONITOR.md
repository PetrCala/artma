# R-universe monitor

The package is published on r-universe at
<https://petrcala.r-universe.dev/artma>. R-universe rebuilds from the master
branch automatically, so merging a fix to master is the release; there is no
separate publish step.

The `R-universe Monitor` workflow (`.github/workflows/r-universe-monitor.yaml`)
runs daily and on demand. It has two stages.

## Detection

`.github/scripts/r_universe_monitor.py` fetches the unauthenticated JSON API
(`https://petrcala.r-universe.dev/api/packages/artma`, latest build only) and
collects every `_jobs` entry whose check status is ERROR or WARNING. The wasm
build reports a null status; null is not a failure.

- On failures it opens an issue labelled `r-universe-failure` with the failing
  configs, links to the build logs in the `r-universe/petrcala` repo, and a
  fingerprint (package version plus sorted `config:status` pairs) embedded as
  an HTML comment.
- Deduplication compares that fingerprint against open `r-universe-failure`
  issues: an identical failure set files nothing, a changed one files a new
  issue and closes the superseded ones.
- Once all checks are green again, open `r-universe-failure` issues are
  commented on and closed.

To exercise the script locally without touching GitHub:

```bash
python3 .github/scripts/r_universe_monitor.py --dry-run
```

## Automated fix

When the `ANTHROPIC_API_KEY` repository secret is set, a second job runs
`anthropics/claude-code-action@v1` on the fresh issue: it reads the failing
build logs, makes a minimal fix, runs the relevant tests, and opens a PR
against master that closes the issue via `Fixes #N`.

Without the secret the workflow degrades to issue-only; pick the issue up
locally (for example in Claude Code) and open the fix PR yourself.

Note: PRs created with the default `GITHUB_TOKEN` do not trigger CI workflows
on their own. Close and reopen the PR, or push an empty commit, to start the
checks before merging.
