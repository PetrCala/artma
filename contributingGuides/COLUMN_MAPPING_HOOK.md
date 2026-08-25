# External column-mapping hook

Column auto-detection declines a required role rather than guess it (see
`inst/artma/data/column_recognition.R`). This hook is the opt-in way to let an
external tool (an LLM agent, a lookup table, a lab-specific script) answer that
decline. The package stays pure R: no network code, no API clients, no new
dependencies. Everything lives in `inst/artma/data/external_mapping.R`.

With `data.mapping.external_command` unset, nothing here runs and a run
produces exactly the mapping it produced before.

## Contract

1. Recognition declines one or more required roles.
2. artma writes a JSON request to the command's **stdin**.
3. The command writes a JSON response to its **stdout** and exits 0.
4. Every proposal is verified against the data before it is used.
5. Verified proposals are confirmed by the user in an interactive session, and
   applied directly outside one (configuring the command is the consent).
6. Accepted mappings persist through the per-column store (`data.columns`), so
   the command runs once per dataset, not once per session.

Any failure (nonzero exit, unreadable output, timeout, missing `jsonlite`)
logs a warning and leaves the roles unmapped. The hook can never abort a run.

## Configuration

```r
artma::options_modify(
  user_input = list(
    data.mapping.external_command = "Rscript /path/to/mapper.R",
    data.mapping.external_timeout = 30
  )
)
```

The command is a command line (split on whitespace, quotes honoured) or a
character vector of the executable and its arguments. `external_timeout` is in
seconds.

## Request schema (`artma.column_mapping_request/1`)

Written to stdin as a single JSON object.

| Field | Type | Meaning |
| --- | --- | --- |
| `schema` | string | Always `artma.column_mapping_request/1`. |
| `dataset.n_rows` / `dataset.n_columns` | number | Shape of the data frame. |
| `requested_roles` | array of string | The required roles left unmapped. Propose only these. |
| `mapped` | object | Roles already mapped, as `role -> column`. |
| `columns` | array of object | One summary per column of the data (below). |
| `declined` | object | The decline evidence per requested role (below). |

Each entry of `columns`:

| Field | Type | Meaning |
| --- | --- | --- |
| `name` | string | The column name in the data file. |
| `class` | string | R class of the column. |
| `n` | number | Non-missing values. |
| `coverage` | number | Share of rows populated, in [0, 1]. |
| `n_distinct` | number | Distinct non-missing values. |
| `numeric` | boolean | Whether the values are numeric-like; the fields below are present only when true. |
| `quantiles` | object | `min`, `q25`, `median`, `q75`, `max`. |
| `integer_share` | number | Share of values that are whole numbers. |
| `negative_share` | number | Share of values below zero. |
| `zero_share` | number | Share of values equal to zero. |
| `uniqueness_ratio` | number | `n_distinct / n`. |
| `is_id_like` | boolean | Whether the values look like an identifier (sequence, counter, year, near-unique code). |

**No raw values are ever sent**: names, counts, shares, and quantiles only.
What the configured command does with the payload is the user's choice.

Each entry of `declined` is the machine-readable evidence recognition produced
for that role: `role`, `reason`, `required_confidence`, and a `candidates`
array of the ranked columns it considered, each with `column`, `score`,
`name_score`, and `evidence`. A wide-format or (t, df)-derivation decline also
carries `layout` or `derivation`.

Example (trimmed):

```json
{
  "schema": "artma.column_mapping_request/1",
  "dataset": { "n_rows": 100, "n_columns": 5 },
  "requested_roles": ["effect"],
  "mapped": { "se": "se", "study_id": "study", "n_obs": "nobs" },
  "columns": [
    {
      "name": "eis",
      "class": "numeric",
      "n": 100,
      "coverage": 1,
      "n_distinct": 98,
      "numeric": true,
      "quantiles": { "min": -1.2, "q25": -0.1, "median": 0.2, "q75": 0.5, "max": 1.6 },
      "integer_share": 0,
      "negative_share": 0.34,
      "zero_share": 0,
      "uniqueness_ratio": 0.98,
      "is_id_like": false
    }
  ],
  "declined": {
    "effect": {
      "role": "effect",
      "reason": "no candidate cleared the acceptance threshold",
      "required_confidence": 0.7,
      "candidates": [{ "column": "eis", "score": 0.5, "name_score": 0, "evidence": 1 }]
    }
  }
}
```

## Response schema (`artma.column_mapping_response/1`)

Written to stdout as a single JSON object:

```json
{
  "schema": "artma.column_mapping_response/1",
  "mappings": { "effect": "eis" }
}
```

- `mappings` maps a requested role to a column name in the data.
- A role may also name an object with a `column` field, so a command can carry
  its own extra fields: `{"effect": {"column": "eis", "confidence": 0.8}}`.
  artma reads `column` and ignores the rest.
- A bare `role -> column` object with no `mappings` wrapper is accepted too.
- Roles that were not requested are ignored. Propose nothing by returning
  `{"mappings": {}}`.
- Anything written to stderr is discarded, except that the first line is quoted
  back in the warning when the command exits nonzero. Do not write progress
  messages to stdout: they would make the response unreadable.

## Verification

A proposal is a suggestion, and it is checked exactly like a candidate
auto-detection found itself. In order:

1. The column exists in the data.
2. The column is not already mapped to another role.
3. `check_mapping_plausibility()`: a measured role (effect, se) must not be
   whole numbers only, and the values must not contradict the role
   (evidence above `CONTRADICTORY_EVIDENCE`).
4. For `effect` and `se`, on a frame with at least `MIN_ROWS_FOR_EVIDENCE`
   rows: value evidence at or above `PROVISIONAL_THRESHOLDS$min_evidence`, and,
   where the counterpart role is mapped, pair consistency at or above
   `PROVISIONAL_THRESHOLDS$min_pair` (see `inst/artma/data/role_evidence.R`).

A proposal that fails is rejected with a warning naming the check that failed.

## Writing a command

A minimal stub, useful as a template and as the shape the tests use:

```r
# mapper.R
request <- jsonlite::fromJSON(file("stdin"), simplifyVector = FALSE)
roles <- unlist(request$requested_roles)
# ... decide, however you like, which column answers each role ...
cat(jsonlite::toJSON(list(mappings = list(effect = "eis")), auto_unbox = TRUE))
```

Tests live in `tests/testthat/test-data-external-mapping.R` and drive real
`Rscript` stubs through the accepted, rejected, malformed, failing, and
timed-out paths.
