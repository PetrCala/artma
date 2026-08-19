# Preview data

Open a data frame in R's viewer. Data can be supplied as a file path, a
data frame, or loaded from an options file (with the same prompt flow as
[`artma()`](https://petrcala.github.io/artma/reference/artma.md) when no
options are given).

## Usage

``` r
data_preview(
  data = NULL,
  options = NULL,
  options_dir = NULL,
  preprocess = TRUE
)
```

## Arguments

- data:

  *\[character, data.frame, optional\]* Either `NULL`, a length-one
  character path to a data file, or a data frame. If `NULL`, data is
  loaded from the options file (you will be prompted to select or create
  one in interactive mode).

- options:

  *\[character, optional\]* Name of the options file (with or without
  `.yaml` extension). If `NULL` and options are required, you will be
  prompted in interactive mode.

- options_dir:

  *\[character, optional\]* Directory containing the options file. If
  `NULL`, uses the default options directory.

- preprocess:

  *\[logical, optional\]* If `TRUE` (default), data is run through the
  full pipeline (read, preprocess, compute) so the viewer shows what
  runtime methods receive. If `FALSE`, only the raw file read (for a
  path) or the given data frame is shown, without options-dependent
  standardization or preprocessing.

## Value

Invisible `NULL`. Opens the data in the standard R viewer
([`utils::View()`](https://rdrr.io/r/utils/View.html)). When no viewer
is available (a non-interactive session, or a Unix session with no
reachable display), the first rows are printed instead.

## Details

Three data sources are supported:

- **Path**: pass a length-one character path to a data file. With
  `preprocess = FALSE`, the file is read without loading options (raw
  read). With `preprocess = TRUE`, options are loaded and the full
  pipeline is applied.

- **Data frame**: pass a data frame. With `preprocess = FALSE`, it is
  viewed as-is. With `preprocess = TRUE`, options are loaded and
  preprocess + compute are applied before viewing.

- **NULL**: data comes from the chosen options file (same "select or
  create options file" flow as
  [`artma()`](https://petrcala.github.io/artma/reference/artma.md) with
  empty arguments). With `preprocess = TRUE` (default), the full
  pipeline (read, preprocess, compute) is run. With
  `preprocess = FALSE`, only the data as read from file (with column
  standardization from options) is shown, without preprocessing or
  computed columns.

In non-interactive mode, when `data` is `NULL` and `options` is `NULL`,
no viewer is shown (consistent with
[`artma()`](https://petrcala.github.io/artma/reference/artma.md)).

## See also

- [`artma`](https://petrcala.github.io/artma/reference/artma.md) - Run
  meta-analysis methods

- `prepare_data()` - Prepare data manually

- [`options_load`](https://petrcala.github.io/artma/reference/options_load.md) -
  Load options

## Examples

``` r
if (FALSE) { # \dontrun{
# Preview data from options file (prompts for file if NULL)
data_preview(options = "my_analysis.yaml")

# Preview raw file without loading options
data_preview("/path/to/data.csv", preprocess = FALSE)

# Preview preprocessed data from a path (uses options for standardization)
data_preview("/path/to/data.csv", options = "my_analysis.yaml")

# Preview a data frame as-is
data_preview(mtcars, preprocess = FALSE)
} # }
```
