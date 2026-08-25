PACKAGE_NAME <- "artma"
PACKAGE_NAME_VERBOSE <- "Automatic Replication Tools for Meta-Analysis"
RUN_URL <- "https://petrcala.r-universe.dev/"

#' @export
CONST <- list(
  PACKAGE_NAME = PACKAGE_NAME,
  PACKAGE_NAME_VERBOSE = PACKAGE_NAME_VERBOSE,
  DATA = list(
    # A list of recognized data (meaning data frame) types.
    # All formats go through one read dispatch (read_by_type) and one shared
    # post-read normalization (normalize_read_df: NA-string replacement,
    # whitespace-to-NA, and type coercion). Text formats (csv, tsv, and Excel,
    # which is read as text) are fully coerced from strings. json must flatten
    # to a tabular record set or a clear error is raised. dta and rds keep the
    # types their native readers return and pass through NA normalization only.
    TYPES = c("csv", "tsv", "xlsx", "xls", "xlsm", "json", "dta", "rds"),
    # Strings that should be interpreted as NA when reading data files.
    # Matching is exact (case-sensitive): only the canonical, R-convention
    # spellings ("NA", the base R and write.csv default; "N/A", "NULL",
    # "null") are treated as missing-value sentinels. All-lowercase "na" and
    # "n/a" were deliberately dropped: "na" in particular is a common
    # real-world category value (e.g. "no functional form assumed"), and
    # matching it case-insensitively silently mislabeled genuine data as
    # missing (see #402). Empty strings and whitespace-only cells are always
    # treated as missing regardless of this list (normalize_whitespace_to_na).
    NA_STRINGS = c("", "NA", "N/A", "NULL", "null"),
    # Stata-style missing-value markers that survive into a raw text column:
    # the bare "." and the extended ".a" through ".z" codes. Matched as a
    # pattern rather than listed in NA_STRINGS because the extended codes are
    # a range, and because read.table's `na.strings` takes fixed strings.
    # Unlike the ambiguous "na" dropped from NA_STRINGS above, a value that is
    # nothing but a dot is a missing marker in every dataset we have seen, so
    # matching it does not risk swallowing a genuine category.
    STATA_MISSING_PATTERN = "^\\.[a-z]?$",
    # Standard column names the pipeline recognizes. These keys identify the
    # role records in the unified per-column store (`data.columns`); every
    # other record key is a moderator variable. Accessors live in
    # `data/utils.R` (`get_standardized_colnames()`, `get_required_colnames()`).
    STANDARD_COLNAMES = c(
      "obs_id", "study_id", "effect", "se", "t_stat",
      "n_obs", "study_size", "reg_dof", "precision"
    ),
    # The subset of standard columns the analysis cannot run without.
    REQUIRED_COLNAMES = c("study_id", "effect", "se", "n_obs"),
    # Standard column names derived or computed by the data pipeline (never
    # user moderator variables).
    COMPUTED_COLNAMES = c("obs_id", "study_label", "t_stat", "study_size", "reg_dof", "precision")
  ),
  DATE_FORMAT = "%Y-%m-%d %H:%M:%S",
  DATE_ONLY_FORMAT = "%Y-%m-%d",
  AUTONOMY = list(
    # Ordered from most to least user interaction. interactive() is the hard gate;
    # this only controls how eagerly to prompt within an interactive session.
    LEVELS = c("ask_more", "balanced", "autonomous"),
    DEFAULT = "autonomous"
  ),
  DATA_CONFIG = list(
    KEYS = list(
      SOURCE_NAME = "source_name",
      DROP_CONFLICTING_RAW = "drop_conflicting_raw",
      IS_COMPUTED = "is_computed",
      VAR_NAME = "var_name",
      VAR_NAME_VERBOSE = "var_name_verbose",
      VAR_NAME_DESCRIPTION = "var_name_description",
      DATA_TYPE = "data_type",
      GROUP_CATEGORY = "group_category",
      VARIABLE_SUMMARY = "variable_summary",
      EFFECT_SUM_STATS = "effect_sum_stats",
      EQUAL = "equal",
      GLTL = "gltl",
      BMA = "bma",
      BMA_REFERENCE_VAR = "bma_reference_var",
      BMA_TO_LOG = "bma_to_log",
      BMA_ALLOW_DERIVED = "bma_allow_derived",
      BPE = "bpe",
      BPE_SUM_STATS = "bpe_sum_stats",
      BPE_EQUAL = "bpe_equal",
      BPE_GLTL = "bpe_gltl"
    ),
    DATA_TYPES = c("dummy", "category", "int", "float", "perc", "empty", "unknown"),
    SETUP_TYPES = c("auto", "manual")
  ),
  OPTIONS = list(
    VALIDATION_ACTIONS = c(
      "abort_verbose",
      "abort_quiet",
      "return_errors_verbose",
      "return_errors_quiet"
    ),
    PROMPT_TYPES = list(
      READLINE = "readline",
      FILE = "file",
      DIRECTORY = "directory",
      FUNCTION = "function"
    ),
    # These are the names of the template files that are used to generate the options template.
    TEMPLATE_NAMES = c("template.yaml", "template.yml", "options_template.yaml", "options_template.yml"),
    # These are the names of the keywords that are recognized in the options template.
    RECOGNIZED_KEYWORDS = c("default", "help", "prompt", "allow_na", "confirm_default"),
    DEFAULT_PROMPT_TYPE = "readline",
    # How many times a single option prompt is re-asked before giving up, used
    # when the answer asks for a file picker that cannot be opened or that the
    # user closed without picking anything.
    MAX_PROMPT_ATTEMPTS = 3L
  ),
  VARIABLE_SUMMARY_STATS = list(
    NAMES = c(
      "Var Name", "Var Class", "Mean", "Median",
      "Min", "Max", "SD", "Obs", "Missing obs"
    ),
    DESIRED_VARS = c("effect", "se", "n_obs", "reg_dof")
  ),
  EFFECT_SUMMARY_STATS = list(
    NAMES = c(
      "Var Name", "Var Class", "Mean", "CI lower", "CI upper", "Weighted Mean",
      "WM CI lower", "WM CI upper", "Median", "Min", "Max", "SD", "Obs"
    ),
    DESIRED_VARS = c("effect", "se", "n_obs", "reg_dof")
  ),
  MOCKS = list(
    DATA_FILE_NAME = "mock-data.csv",
    TMP_DATA_FILE_NAME = "tmp_data.csv",
    TMP_OPTIONS_FILE_NAME = "tmp_options.yaml",
    MOCK_DF_NROWS = 1000,
    MOCK_DF_NSTUDIES = 50,
    MOCK_DF_SEED = 123
  ),
  PATTERNS = list(
    YAML_FILES = list(
      PLAIN = c(".yaml"),
      REGEX = "\\.ya?ml$"
    )
  ),
  DEFAULT_VERBOSITY = 3,
  STYLES = list(
    OPTIONS = list(
      NAME = cli::col_magenta,
      VALUE = cli::col_green,
      TYPE = cli::col_cyan,
      DEFAULT = cli::col_yellow
    )
  ),
  URLS = list(
    BASE = RUN_URL,
    PACKAGE_BASE = paste0(RUN_URL, "/", PACKAGE_NAME),
    VIGNETTE_BASE = paste0(RUN_URL, "/articles/", PACKAGE_NAME),
    PACKAGE_PDF = paste0(RUN_URL, "/", PACKAGE_NAME, "/", PACKAGE_NAME, ".pdf")
  )
)
