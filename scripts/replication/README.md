# Replicating published meta-analyses with artma

A harness for re-running published meta-analyses (and IES theses) through artma
and comparing the results against the numbers their authors report. Its point
is proof by contact with real literature: every row of the committed results
table is a published number artma either reproduces or does not.

One YAML manifest per paper describes where the data lives, how its columns map
onto artma's canonical names, which artma options match the paper's spec, which
methods to run, and, transcribed by hand from the paper, what the authors
reported. The runner turns that into a side-by-side comparison table.

Current results: [RESULTS.md](RESULTS.md).

## Layout

| Path | What it is |
| --- | --- |
| `manifests/*.yaml` | one manifest per paper; `_TEMPLATE.yaml` documents the schema |
| `lib/manifest.R` | manifest reading and validation |
| `lib/compare.R` | matching artma estimates to reported numbers, and the verdict logic |
| `run_replication.R` | the runner |
| `test_harness.R` | checks for everything that does not need artma |
| `RESULTS.md` | the committed comparison table from the latest full run |
| `data/`, `work/`, `output/` | generated, git-ignored |

## Running

```bash
Rscript scripts/replication/run_replication.R                # every manifest
Rscript scripts/replication/run_replication.R --ids bajzik2020_armington
Rscript scripts/replication/run_replication.R --refresh-data # re-download datasets
```

Output lands in `output/`: one `<id>.md` and `<id>_artma_estimates.csv` per
paper, plus a combined `comparison_all.csv` and `SUMMARY.md`. After a full run
whose verdicts you accept, copy `output/SUMMARY.md` over `RESULTS.md` and
commit it.

The manifest and comparison layers depend on nothing but base R and `yaml`, so
they can be checked without artma installed:

```bash
Rscript scripts/replication/test_harness.R
```

The runner itself needs R >= 4.4 and an installed artma.

## CI

The `Replication` workflow (`.github/workflows/replication.yaml`) re-runs every
manifest on demand, on each published release, and monthly. It fails when any
comparison lands outside the `close` tolerance, and files (or updates) a
`ci: replication run failing` issue so scheduled failures are seen. It is
deliberately not a PR gate: the runs are slow, need network access, and a red
result is information to investigate, not a reason to block an unrelated merge.
Datasets are checksum-pinned in the manifests, so a silently re-uploaded
upstream file is a hard error rather than a quiet drift.

## The roster

Papers earn a manifest by meeting all of:

1. **Public, stable data.** The dataset is downloadable, complete for the
   compared estimators, and pinned by SHA-256.
2. **Reproducible spec.** The paper's numbers can be tied to a concrete
   estimator artma implements, including input transformations (winsorization,
   subsamples, weights). When the text and the replication code disagree,
   the code wins.
3. **Unambiguous pooled numbers.** Only pooled (or clearly conditioned)
   point estimates are transcribed, from the paper's tables, never from the
   abstract. Conditional counterfactuals ("best-practice" headline values) are
   not comparable to artma's pooled estimators and are never targeted.

Keep the roster small (4-6 papers). Every manifest is a maintenance liability:
when a release deliberately changes numerics, each one must be re-run and
re-judged. That cost is the point (it forces a review of numeric changes), but
it only stays payable if the roster stays curated.

Current roster:

| Manifest | Paper | Compared quantities |
| --- | --- | --- |
| `bajzik2020_armington` | Bajzik, Havranek, Irsova & Schwarz (2020), JIE | 8 (FAT-PET OLS/FE/study-weighted, hierarchical Bayes) |
| `elminejad2025_risk` | Elminejad, Havranek & Irsova (2025), JoES | 5 (WLS, WAAP, selection model, endogenous kink) |
| `reckova2015_climate` | Reckova & Irsova (2015), Energy & Environment | 1 (uncorrected mean; see the manifest for why nothing else) |

Evaluated and excluded:

- **Matousek, Havranek & Zeynalova (2022), discount rates.** The baseline
  tables use standard errors the authors bootstrapped at the study level for
  388 of 927 estimates; those values are not in the public dataset, so the
  published numbers cannot be reproduced from public data. The 539-estimate
  robustness subset is only reported in a working-paper version that is no
  longer easily obtainable.

Rows can also be excluded within an included paper; the manifests document
each such call (e.g. the risk paper's stem-based estimate, produced by
Furukawa's own code with an unrecoverable spec, and the climate paper's
corrected estimates, whose specifications include controls artma's FAT-PET
deliberately does not have).

## How the comparison works

Every artma method emits its numbers in one fixed long-format schema
(`contributingGuides/METHODS.md`):

```
method, model, term, estimate, std_error, statistic, p_value,
conf_low, conf_high, n_obs, n_clusters, note
```

That shared schema is what makes this method-agnostic. Each `reported` entry in
a manifest names a `(artma_method, artma_model, artma_term)` triple; the runner
binds the estimates from every method that ran and looks the triple up.
`artma_model` and `artma_term` accept a literal name or an anchored regular
expression. Note that `model` in the estimates frame holds the internal model
name (`ols`, `fe`, `ols_precision_weighted`, `hierarchical`, ...), not the
display label; run `--ids <id>` and read `output/<id>_artma_estimates.csv` to
see the exact strings.

Each row gets a verdict:

| Verdict | Meaning |
| --- | --- |
| `replicated` | within 1% of the reported value |
| `close` | within 10% |
| `differs` | further apart than that |
| `sign flip` | opposite signs; the substantive conclusion changed |
| `not_produced` | artma ran but never emitted that quantity |
| `ambiguous` | the triple matched more than one estimate; pin `artma_model` |
| `no_estimates` | no method reported any numbers |
| `unknown` | a value was missing on one side |

Point estimates rarely reproduce bit-for-bit -- software versions, cluster
definitions and bootstrap seeds all move the last digits -- so `replicated` is a
tolerance, not an equality test. Thresholds are `TOL_EXACT` and `TOL_CLOSE` in
`lib/compare.R`.

`not_produced` deserves care: it means artma did not emit the quantity, which is
a different thing from artma disagreeing. The runner prints any method artma
skipped or errored on, so check that output before reading a wall of
`not_produced` as a failed replication.

## Adding a paper

1. Check it against the roster criteria above; get the replication code, not
   just the PDF.
2. Copy `manifests/_TEMPLATE.yaml` to `manifests/<id>.yaml`.
3. Fill in identity and `data.url`. Pin `data.sha256` once you have the file.
4. Map the paper's column names onto artma's canonical ones under `columns`.
   Only mapped columns reach artma; map an extra column under its own name if
   a `data.subset_conditions` filter needs it.
5. Mirror the paper's spec under `options` (winsorization level, subsamples,
   method options). This step decides whether the comparison means anything;
   read the paper's code, not only its text.
6. List the `methods` the paper actually reports and transcribe the numbers
   into `reported`, one entry per comparable quantity, each with a `source`
   (table and page).
7. Run `--ids <id>`, reconcile every `differs` to either a spec fix or a
   documented exclusion, then regenerate `RESULTS.md`.

## Network requirements

Datasets and papers used by the current roster live on `meta-analysis.cz`;
CRAN (`cloud.r-project.org`) is needed for dependencies. Thesis-based
manifests would additionally need `dspace.cuni.cz` / `is.cuni.cz`.
