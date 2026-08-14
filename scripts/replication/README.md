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
Rscript scripts/replication/audit_estimators.R             # artma vs reference implementations
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

### Weighting conventions, and which one is right

artma's `ols_precision_weighted` fits `effect ~ se` with weights `precision^2`,
and since `precision_type` defaults to `1/SE` that is **inverse-variance**
weighting (1/SE²). This is correct, and the evidence is not a judgement call.

**The field's own guidance.** Irsova, Havránek, Zeynalova & Kolcunova,
*Meta-Analysis of Social Science Research: A Practitioner's Guide*
([JoES 2024](https://doi.org/10.1111/joes.12595), hosted at
[meta-analysis.cz/guidelines](https://meta-analysis.cz/guidelines/)):

> you should opt for unrestricted weighted least squares (UWLS), which dominate
> both fixed-effect and random-effects meta-analysis estimators (Stanley &
> Doucouliagos, 2015, 2017; Stanley, Ioannidis et al., 2023)

> The optimal meta-analysis weight is based on inverse variance [...] You should
> use the classical inverse-variance weight as the starting point.

**The reference implementations.** Across the 21 published replication packages
on meta-analysis.cz, every explicit precision weighting squares the precision,
and there is not one unsquared instance:

| Weight as written | Occurrences |
| --- | ---: |
| `[aweight=1/(se*se)]` | 16 |
| `[aweight=precision_w*precision_w]` | 11 |
| `[aweight=1/(se_coefficient*se_coefficient)]` | 10 |
| `[aweight=precision*precision]` and similar | 8 |
| unsquared precision (`[aw=precision]`, `[aw=1/se]`) | **0** |

`precision` is defined as `1/se` in every one of them, so the weight is 1/SE²
throughout. The R package for Bajzik, Havránek, Iršová & Novák (2025) states it
in the same form artma uses:

```r
# activism.R, meta-analysis.cz/activism/
OLS_w_precision <- lm(pcc_w ~ se_pcc_w, data = dataset,
                      weight = c(dataset$se_precision_w * dataset$se_precision_w))
```

**Why it is right on the merits.** WLS of `effect ~ se` with 1/SE² weights is
algebraically identical to OLS of `t ~ 1/SE`, the Egger funnel-asymmetry
regression (verified numerically here to 1e-10), and it is the GLS/BLUE choice
under `Var(e_i) ∝ SE_i²`. Weighting by 1/SE is neither.

Study weighting agrees too: the published papers use `1/n_estimates_per_study`
unsquared (`gen inv_nest = 1/nest`, `[aweight=inv_nest]`), which is artma's
`weights = 1/study_size`.

**The theses are not the standard.** Three of the ten (Simpartl, Pokorná,
Juračková) weight by 1/SE, which no published paper or guideline in this
literature does; Křenková, Kozlíková, Hatalová, Prokš and Horák square it as
artma does. Their table notes are unreliable either way — several say "weighted
by the inverse of the standard error" over code that squares it. Where a thesis
used 1/SE, the divergence from artma is that thesis departing from the standard
estimator, and the manifest says so rather than retargeting the claim to
whichever artma row happens to agree.

### Winsorization has to be matched per thesis

artma winsorizes `effect` and `se` at `data.winsorization_level`, default 0.01.
The theses vary: 1%, 2.5%, none, and in Kozlíková's case an asymmetric
(0.01, 0.95) on the effect only. It moves estimates a long way — her
precision-weighted FAT is −16.10 as reported, −16.31 unwinsorized, and −5.67
under artma's default. Each manifest sets `data.winsorization_level` to match its
thesis's code, with the evidence quoted in a comment.

Worth noting against artma's default: the practitioner's guide's tenth
recommendation is "inspect outliers and influence points but *be careful about
deleting or winsorizing them*", which is an argument for winsorization being
opt-in rather than on at 1% out of the box.

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

## What the estimator audit found

`audit_estimators.R` compares artma against from-scratch reference
implementations rather than against the theses, so it separates "artma is wrong"
from "the thesis is wrong". Current result:

```
120 comparisons over 10 datasets
  be / fe / ols / ols_precision_weighted / ols_study_weighted / re   20/20 agree each
No disagreements: every artma linear estimator reproduces the reference exactly.
```

Specifically verified:

- All six linear estimators match base R / `plm` implementations of the Stata
  specifications to machine precision, on every dataset.
- The fixed-effects "effect beyond bias" equals Stata's `xtreg, fe` `_cons`
  (`mean(y) - b*mean(x)`). artma reaches it via `plm::within_intercept()`; the
  two agree exactly. This matters because a within estimator has no natural
  intercept, so it is a plausible place for a bug — there isn't one.
- Clustered standard errors from `sandwich`'s HC1 equal Stata's `cluster()`
  finite-sample convention (relative difference ~1e-16).
- `ols_study_weighted`'s bootstrap uses a dedicated `1/study_size` path rather
  than the squared-weight convention the other weighted models use, which is
  correct and easy to get wrong.

All 26 `linear_tests` mismatches against the theses have artma equal to the
independent reference. None is an artma defect.

### One genuine finding: the IV instrument is chosen by first-stage strength

`exogeneity_tests` defaults to `iv_instrument: "automatic"`, which builds four
candidates — `1/sqrt(n_obs)`, `1/n_obs`, `1/n_obs^2`, `log(n_obs)` — and picks
whichever has the strongest first-stage F. `1/sqrt(n_obs)` is used only to break
ties (`WEAK_INSTRUMENT_TIEBREAK`).

The literature does not do this. The practitioner's guide motivates the
instrument theoretically (precision scales with `1/sqrt(N)`), and the published
replication packages hard-code it: `ivreg pcc (sepcc = root)`,
`gen inv_sqrt_nobs = sqrt(inv_nobs)`, `1/sqrt(nobs)`.

On Simpartl (2023), which reports an IV column:

| Instrument | FAT | PET |
| --- | ---: | ---: |
| thesis, Table 1 | 0.376 | −0.088 |
| `1/sqrt(n_obs)` (reference) | 0.347 | **−0.08806** |
| `1/n_obs` (what artma picks) | 0.109 | −0.066 |
| `log(n_obs)` | 0.574 | −0.109 |

The reference instrument reproduces the reported PET to four decimals; artma's
choice does not. Two problems, one practical and one methodological:

1. It silently departs from the field standard, so an artma IV column is not
   comparable to a published one.
2. Selecting an instrument by its in-sample first-stage F is a specification
   search over the same data used for inference. It biases toward strong-looking
   first stages and understates the resulting uncertainty — the pathology IV
   exists to avoid. The code carefully argues against selecting on R-squared,
   Wu-Hausman and Sargan, but selecting on F is subject to the same objection.

Suggested change: default `iv_instrument` to `1/sqrt(n_obs)` and keep
`"automatic"` as an explicit opt-in for exploration.

### A softer one: winsorization is on by default

`data.winsorization_level` defaults to `0.01`, so artma winsorizes `effect` and
`se` unless told not to. The practitioner's guide's tenth recommendation is to
"inspect outliers and influence points but be careful about deleting or
winsorizing them". The effect is not small — Kozlíková's precision-weighted FAT
is −16.10 as reported, −16.31 unwinsorized, and −5.67 under artma's default.
Opt-in would match the guidance and make runs comparable to published tables by
default.

## Scope and honesty

Only theses whose dataset is genuinely downloadable are included. If a dataset
cannot be obtained, or a number cannot be found in the PDF, the thesis or the
claim is dropped rather than approximated — `SUMMARY.md` should never contain a
number that was not either transcribed from a thesis or produced by artma.
