# Replication harness

Re-runs published IES (Charles University) bachelor's and master's meta-analysis
theses through `artma` and compares the output against the numbers their authors
printed, one manifest per thesis.

The point is to exercise `artma` against real, messy, third-party data with a
known answer attached. A `mismatch` is not automatically an artma bug — the
thesis may have used a different specification, a different subsample, or a
different estimator — but every mismatch is a question worth asking.

## Running it

```bash
Rscript scripts/replication/run_replication.R              # every manifest
Rscript scripts/replication/run_replication.R --only=<id>  # a single thesis
Rscript scripts/replication/run_replication.R --list       # list the manifests
Rscript scripts/replication/test_harness.R                 # harness unit tests
```

`run_replication.R` writes:

- `SUMMARY.md` — the report, one section per thesis.
- `out/<id>/estimates.csv` — everything artma emitted for that thesis.
- `out/<id>/claims.csv` — the claim-by-claim comparison.
- `data/<id>/` — the cached download, so reruns are offline.

`data/` and `out/` are generated and git-ignored; the manifests and `SUMMARY.md`
are checked in. The harness sources live in `harness/` rather than `lib/`, which
the repository `.gitignore` reserves for build artefacts.

Requires R >= 4.4 with `artma` and `yaml` installed; `readxl` is needed for
theses that publish `.xlsx`, and `digest` to verify dataset checksums.

## Writing a manifest

One YAML file per thesis in `manifests/`, named `<id>.yaml` where `<id>` matches
the manifest's own `id` field (the test suite enforces this).

```yaml
id: simpartl-2023-military-growth

thesis:
  author: "Simpartl, Josef"
  title: "Military expenditure and economic growth: A meta-analysis"
  degree: master                 # bachelor | master
  year: 2023
  advisor: "Havránek, Tomáš"
  handle: "20.500.11956/183183"
  url: "https://dspace.cuni.cz/handle/20.500.11956/183183"

dataset:
  url: "https://dspace.cuni.cz/bitstream/handle/.../120437192.zip"
  archive_member: "Milex_Meta_Dataset.csv"   # omit when the URL is the data file
  sha256: "0265fafe034b..."                  # optional but recommended
  decimal_mark: ","                          # Czech-locale exports need this
  delim: ","                                 # omit to sniff
  encoding: "UTF-8"
  sheet: 1                                   # xlsx only
  row_filter: "Milk == 1"                    # restrict to the panel being replicated
  code_published: true                       # did the author publish code too?
  code_files: ["RCode.txt"]

# Optional: artma options for this run, as flat dotted paths. Use sparingly and
# say why in a comment -- these change how artma behaves.
artma_options:
  data.na_handling: "median"

columns:            # artma canonical name -> column in the published file
  effect: "PCC"
  se: "PCCSE"
  study_id: "ID"
  n_obs: "Nobs"

claims:
  - id: pet_ols
    label: "PET (effect beyond bias), OLS"
    reported: 0.008
    reported_se: 0.010          # optional
    source: "Table 5.1, p. 35"  # must cite a page; enforced by the validator
    method: linear_tests
    artma_model: "^ols$"
    artma_term: "^effect$"
    tolerance: {abs: 0.01}      # optional, overrides the default
```

### Rules the validator enforces

- `thesis.degree` is `bachelor` or `master` — this harness is about student
  theses, so dissertations are out of scope.
- `columns` must map all four of artma's required columns: `effect`, `se`,
  `study_id` and `n_obs`. `study_id` is what `linear_tests` clusters on. artma
  aborts when a required column has gaps; `artma_options: {data.na_handling:
  median}` is the usual escape hatch, but only take it when you can say why the
  imputation cannot move the numbers being compared.
- Every claim needs a `source` containing a page number. A reported number with
  no pointer back into the PDF cannot be audited, which would defeat the
  exercise.
- `reported` must be a finite number transcribed from the thesis. Never put a
  guess here: if a thesis does not report a quantity, leave the claim out.
- `artma_model` and `artma_term` must be valid regexes, and together must select
  exactly one row.

## Matching claims to artma output

`artma` returns a long-format `estimates` frame per method with the shared
schema (`method`, `model`, `term`, `estimate`, `std_error`, ...). A claim names
the `method` and gives regexes for `model` and `term`.

The vocabulary is artma's own, and it is *not* the wording of the printed
table — `linear_tests` prints a column headed "OLS" but records the model as
`ols`. The values available at the time of writing:

| method | `model` | `term` |
| --- | --- | --- |
| `effect_summary_stats` | `All Data`, plus one per flagged subgroup | `mean`, `weighted_mean`, `median`, `min`, `max`, `sd` |
| `linear_tests` | `ols`, `fe`, `be`, `re`, `ols_study_weighted`, `ols_precision_weighted` | `effect`, `publication_bias` |
| `nonlinear_tests` | `waap`, `top10`, `stem`, `selection`, `endogenous_kink` | `effect`, `publication_bias`, `effect_heterogeneity`, `pub_prob_1` |
| `exogeneity_tests` | `iv`, `puniform` | `effect`, `publication_bias`, `publication_bias_test` |
| `p_hacking_tests` | `threshold_1.645`, `threshold_1.96`, `threshold_2.58`, `binomial`, `lcm`, `fisher`, `discontinuity`, `cox_shi` | varies by model |
| `bma` | `NA` | one per moderator, e.g. `Se`, `Study size`, `Intercept` |
| `best_practice_estimate` | `author`, `study` | `Author`, or the study index |

Rather than trusting that table, run the thesis once and read
`out/<id>/estimates.csv`: it is the authoritative list. A claim whose regexes
select zero or several rows is reported as `no match` / `ambiguous` in
`SUMMARY.md`, together with the model/term pairs that *were* available — that
listing is the intended way to fix the regex.

`terms` in `linear_tests` are easy to get backwards: `effect` is the
bias-corrected effect (PET, the intercept), and `publication_bias` is the funnel
asymmetry slope (FAT). A thesis usually prints them the other way round.

### One systematic difference worth knowing

artma's `ols_precision_weighted` fits with weights `precision^2`, and since
`precision_type` defaults to `1/SE`, that is **inverse-variance** weighting
(`1/SE²`). Most of these theses label a column "Precision" but define it in
their table notes as weighting by `1/SE` — a different estimator. Prokš (2026)
happens to report both conventions in separate tables, which pins this down: his
inverse-variance table matches artma, his "precision-weighted" table does not.

Where a thesis's note states the convention, the manifest says so in a comment.
The mapping is left pointing at artma's nearest labelled counterpart rather than
being quietly retargeted to whichever row agrees, so the disagreement stays
visible in `SUMMARY.md`.

## Reading the published files

These are spreadsheets exported by students, and the harness absorbs the usual
damage so a manifest does not have to:

- **Czech-locale decimals.** `"0,2065"` inside quoted fields. Set
  `decimal_mark: ","`; combine with `delim: ";"` where both apply.
- **UTF-8 BOM.** Stripped automatically, so the first column is `bs` rather than
  an invisible `﻿bs`.
- **Repeated column names.** Wide "one block per outcome" layouts repeat `se`
  after each outcome. The first keeps its bare name; later ones become `se__2`,
  `se__3`, so a manifest can name the one it means instead of silently getting
  the first.
- **Subgroup panels.** Most theses report several panels from one pooled file.
  `row_filter` is an R expression over the raw columns; check it reproduces the
  panel's stated N before trusting the comparison.

A good sanity check before writing any claims: confirm the filtered sample size
and mean match the thesis's descriptive-statistics table. If those disagree, the
column mapping or the filter is wrong and every other number will be too.

## Verdicts

| Verdict | Meaning |
| --- | --- |
| `match` | within tolerance — by default 0.005 absolute or 5% relative, whichever is looser |
| `close` | within 3x tolerance, same sign |
| `mismatch` | outside that, or the sign flipped |
| `no match` / `ambiguous` | the regexes selected zero or several rows — a manifest bug |
| `error` | artma returned a non-finite estimate |

The default tolerance is deliberately loose because theses print rounded tables;
tighten it per claim when a thesis reports more digits.

## Scope and honesty

Only theses whose dataset is genuinely downloadable are included. If a dataset
cannot be obtained, or a number cannot be found in the PDF, the thesis or the
claim is dropped rather than approximated — `SUMMARY.md` should never contain a
number that was not either transcribed from a thesis or produced by artma.
