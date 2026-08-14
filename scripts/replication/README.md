# Replicating IES meta-analysis theses with artma

A harness for re-running IES bachelor/master meta-analysis theses through artma
and comparing the results against the numbers their authors report.

One YAML manifest per thesis describes where the data lives, how its columns map
onto artma's canonical names, which methods to run, and — transcribed by hand
from the thesis — what the author reported. The runner turns that into a
side-by-side comparison table.

## Layout

| Path | What it is |
| --- | --- |
| `manifests/*.yaml` | one manifest per thesis; `_TEMPLATE.yaml` documents the schema |
| `lib/manifest.R` | manifest reading and validation |
| `lib/compare.R` | matching artma estimates to reported numbers, and the verdict logic |
| `run_replication.R` | the runner |
| `test_harness.R` | checks for everything that does not need artma |
| `data/`, `work/`, `output/` | generated, git-ignored |

## Running

```bash
Rscript scripts/replication/run_replication.R                # every manifest
Rscript scripts/replication/run_replication.R --ids kantova2025_finlit
Rscript scripts/replication/run_replication.R --refresh-data # re-download datasets
```

Output lands in `output/`: one `<id>.md` and `<id>_artma_estimates.csv` per
thesis, plus a combined `comparison_all.csv` and `SUMMARY.md`.

The manifest and comparison layers depend on nothing but base R and `yaml`, so
they can be checked without artma installed:

```bash
Rscript scripts/replication/test_harness.R
```

The runner itself needs R >= 4.4 and an installed artma.

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
`artma_model` and `artma_term` are anchored regular expressions, so a manifest
can pin a name exactly or accept a couple of spellings.

Each row gets a verdict:

| Verdict | Meaning |
| --- | --- |
| `replicated` | within 1% of the reported value |
| `close` | within 10% |
| `differs` | further apart than that |
| `sign flip` | opposite signs — the substantive conclusion changed |
| `not_produced` | artma ran but never emitted that quantity |
| `ambiguous` | the triple matched more than one estimate; pin `artma_model` |
| `no_estimates` | no method reported any numbers |
| `unknown` | a value was missing on one side |

Point estimates rarely reproduce bit-for-bit — software versions, cluster
definitions and bootstrap seeds all move the last digits — so `replicated` is a
tolerance, not an equality test. Thresholds are `TOL_EXACT` and `TOL_CLOSE` in
`lib/compare.R`.

`not_produced` deserves care: it means artma did not emit the quantity, which is
a different thing from artma disagreeing. The runner prints any method artma
skipped or errored on, so check that output before reading a wall of
`not_produced` as a failed replication.

## Adding a thesis

1. Copy `manifests/_TEMPLATE.yaml` to `manifests/<id>.yaml`.
2. Fill in identity and `data.url`. Pin `data.sha256` once you have the file —
   it stops a silently re-uploaded dataset from changing your results unnoticed.
3. Map the thesis's column names onto artma's canonical ones under `columns`.
   Only mapped columns reach artma, so a stray column cannot shadow a canonical
   name.
4. List the `methods` the thesis actually reports. Listing more just fills the
   table with rows that have no author-side counterpart.
5. Transcribe the author's numbers into `reported`, one entry per comparable
   quantity, each with a `source` (table and page) so a reviewer can check it.

Step 5 is the only part a machine cannot do for you, and the whole comparison
rests on it. Transcribe from the thesis PDF, not from an abstract.

Run `--ids <id>` and inspect `output/<id>_artma_estimates.csv` to find the exact
`model` and `term` strings artma produced — that is the quickest way to get the
`artma_model` / `artma_term` regexes right.

## Candidate theses

**None of the following is verified.** They came from web search while
`dspace.cuni.cz`, `is.cuni.cz`, `ies.fsv.cuni.cz` and `meta-analysis.cz` were all
unreachable, so supervisor, year, degree and — critically — whether data and code
are published at all still need confirming against the repository. The search
summariser demonstrably conflated theses with published papers of similar title,
so treat every row as a lead to check, not a fact.

| Lead | Author | Supervisor (claimed) | Year | Notes |
| --- | --- | --- | --- | --- |
| Financial Literacy and Retirement Planning | Klára Kantová | Havránková | 2025 | handle `20.500.11956/199780`; MA |
| Democracy and Economic Growth | Tereza Markalousová | Havránek | ~2024/25 | MA |
| The Effects of Quantitative Easing | Matyáš Horák | Havránek | ~2024 | BA |
| How Does ESG Performance Affect Earnings? | Karel Maryško | Havránek | ~2024 | BA |
| The Effectiveness of Foreign Aid on Economic Growth | ? | ? | ? | handle `20.500.11956/203033` |
| Lawsuit events and stock returns | Tomáš Suchomel | Bajzík | 2024 | likely **out of scope** — different supervisor |

The IES supervisor pages (`ies.fsv.cuni.cz`) list supervised theses per person
and are the reliable way to build this set to ten; the search-based list above is
a stopgap.

## Network requirements

The datasets and theses live behind hosts that a restricted egress policy will
block. A run needs, at minimum:

- `dspace.cuni.cz`, `is.cuni.cz` — thesis PDFs and data attachments
- `meta-analysis.cz` — where IES meta-analyses usually publish data and code
- `ies.fsv.cuni.cz` — supervisor thesis lists
- `cloud.r-project.org` — CRAN, for R >= 4.4 and artma's dependencies
  (`box` and `climenu` are not packaged in Ubuntu)

Some replication packages are on `osf.io` or `zenodo.org` instead.
