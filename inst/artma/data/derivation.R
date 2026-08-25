box::use(
  artma / data / column_recognition[
    clean_column_name,
    get_column_patterns,
    looks_like_continuous_measure,
    score_name_for_role
  ]
)

#' @title Cleaned-name patterns that identify a degrees-of-freedom column
#' @description Matched against `clean_column_name()` output, so every
#'   separator has already collapsed to an underscore (`d.f.` reads as `d_f_`).
#'   Deliberately narrower than a keyword match: `df` is a short, generic token
#'   and only an anchored name earns the role.
DOF_NAME_PATTERNS <- c(
  "^d_?f_?$",
  "^dof$",
  "^d_o_f$",
  "^deg(rees)?_?free(dom)?$",
  "^deg(rees)?_?of_?freedom$",
  "^reg_?d[eo]f$",
  "^(reg|resid|residual)_?d_?f$",
  "^d_?f_?(reg|resid|residual)$"
)

#' Share of rows a degrees-of-freedom column must populate to be usable.
MIN_DOF_COVERAGE <- 0.5

#' Distinct values below which a "df"-named column reads as a flag, not a count.
MIN_DOF_DISTINCT <- 3L

#' Above this, the values are not residual degrees of freedom of a regression.
MAX_PLAUSIBLE_DOF <- 1e8

#' Members a horizon family needs before it counts as a wide-format block.
WIDE_MIN_MEMBERS <- 3L

#' Distinct families a sheet needs before it reads as wide format.
WIDE_MIN_FAMILIES <- 2L

#' @title Check a column against the values degrees of freedom predict
#' @description Residual degrees of freedom are positive whole numbers that
#'   vary across regressions. A column of fractions, of zeros, or of two or
#'   three repeated codes is a flag or a transformed quantity, whatever its
#'   name says.
#' @param values *\[vector\]* Column values to judge.
#' @param n_rows *\[integer, optional\]* Rows in the frame the column came
#'   from, used for the coverage check. Defaults to `length(values)`.
#' @return *\[logical\]* TRUE when the values could be degrees of freedom.
looks_like_dof <- function(values, n_rows = length(values)) {
  numeric_values <- suppressWarnings(as.numeric(values))
  numeric_values <- numeric_values[is.finite(numeric_values)]

  if (length(numeric_values) == 0) {
    return(FALSE)
  }
  if (length(numeric_values) / max(as.numeric(n_rows), 1) < MIN_DOF_COVERAGE) {
    return(FALSE)
  }
  if (mean(abs(numeric_values - round(numeric_values)) < 1e-8) < 0.95) {
    return(FALSE)
  }
  if (min(numeric_values) < 1 || max(numeric_values) > MAX_PLAUSIBLE_DOF) {
    return(FALSE)
  }
  if (length(unique(numeric_values)) < MIN_DOF_DISTINCT) {
    return(FALSE)
  }

  TRUE
}

#' @title Find the degrees-of-freedom companion of a t-statistic column
#' @description Name evidence plus value plausibility, both required. When
#'   several columns qualify, the one populated on the most rows wins.
#' @param df *\[data.frame\]* The data frame.
#' @param exclude *\[character, optional\]* Columns already claimed by another
#'   role. Defaults to none.
#' @return *\[character\]* The column name, or `NULL` when none qualifies.
find_dof_column <- function(df, exclude = character(0)) {
  candidates <- setdiff(names(df), exclude)
  if (length(candidates) == 0) {
    return(NULL)
  }

  named <- candidates[vapply(candidates, function(col) {
    col_clean <- clean_column_name(col)
    any(vapply(DOF_NAME_PATTERNS, function(p) grepl(p, col_clean), logical(1)))
  }, logical(1))]

  named <- named[vapply(named, function(col) looks_like_dof(df[[col]], nrow(df)), logical(1))]
  if (length(named) == 0) {
    return(NULL)
  }

  coverage <- vapply(named, function(col) {
    mean(is.finite(suppressWarnings(as.numeric(df[[col]]))))
  }, numeric(1))

  named[[which.max(coverage)]]
}

#' @title Derive a partial correlation and its standard error from (t, df)
#' @description The standard conversion used to put regression coefficients
#'   reported in incompatible units on one scale:
#'   `r = t / sqrt(t^2 + df)` and `se(r) = sqrt((1 - r^2) / df)`. The pair is
#'   self-consistent by construction: `r / se(r)` returns the original `t`.
#' @param t_stat *\[vector\]* The t-statistics.
#' @param dof *\[vector\]* The matching degrees of freedom.
#' @return *\[list\]* With `effect` (the partial correlation) and `se`.
derive_pcc_from_t_dof <- function(t_stat, dof) {
  t_values <- suppressWarnings(as.numeric(t_stat))
  dof_values <- suppressWarnings(as.numeric(dof))

  # Zero or negative degrees of freedom make both formulas undefined; the rows
  # are dropped downstream by the missing-value handling rather than here.
  dof_values[!is.na(dof_values) & dof_values <= 0] <- NA_real_

  pcc <- t_values / sqrt(t_values^2 + dof_values)
  se <- sqrt((1 - pcc^2) / dof_values)

  list(effect = pcc, se = se)
}

#' @title Whether a column earned its role from an exact name pattern
#' @description The regex patterns in `get_column_patterns()` are the canonical
#'   names of a role (`se`, `pcc`, `estimate`); a keyword match is a weaker,
#'   suffixed or compound name (`SE_L`, `COEF_L`, `Estimate...17`). The
#'   distinction is what keeps the derived route from overriding a dataset that
#'   plainly names its effect and standard error.
#' @param column *\[character\]* The data column.
#' @param role *\[character\]* The standard column it was mapped to.
#' @return *\[logical\]* TRUE when the name matched a canonical pattern.
role_name_is_canonical <- function(column, role) {
  patterns <- get_column_patterns()
  if (is.null(patterns[[role]])) {
    return(FALSE)
  }
  identical(score_name_for_role(clean_column_name(column), patterns[[role]])$method, "regex")
}

#' @title Whether an existing column already holds the derived values
#' @description When the mapped effect column *is* the partial correlation the
#'   (t, df) route would compute, deriving it again gains nothing and the
#'   mapping stands.
#' @keywords internal
reproduces_derived <- function(existing, derived, tol = 1e-3) {
  a <- suppressWarnings(as.numeric(existing))
  b <- suppressWarnings(as.numeric(derived))
  ok <- is.finite(a) & is.finite(b)
  if (sum(ok) < 5) {
    return(FALSE)
  }
  mean(abs(a[ok] - b[ok]) <= tol * pmax(1, abs(b[ok]))) >= 0.9
}

#' @title Detect that effect and se are better derived from (t, df)
#' @description Some meta-analyses report regression coefficients in units that
#'   are not comparable across studies and analyse partial correlations
#'   recomputed from the t-statistic and the residual degrees of freedom
#'   instead. The file then carries an internally consistent
#'   coefficient/SE/t triple that recognition happily maps, and the mapping is
#'   wrong in a way no single-column choice can fix.
#'
#'   The route is offered when a t-statistic column is mapped, a
#'   degrees-of-freedom companion exists, and the effect and standard error are
#'   either unmapped or carry non-canonical names (`COEF_L`, `SE_L`,
#'   `Estimate...17`). A dataset that names its columns `pcc`/`se`/`estimate`
#'   keeps its mapping, as does one whose effect column already equals the
#'   derived partial correlation.
#' @param df *\[data.frame\]* The data frame.
#' @param mapping *\[list\]* The mapping recognition arrived at.
#' @return *\[list\]* With `t_stat`, `dof`, `coverage`, and `replaces` (the
#'   effect/se columns the route displaces, possibly empty), or `NULL`.
detect_tdf_derivation <- function(df, mapping) {
  t_col <- mapping[["t_stat"]]
  if (is.null(t_col) || !t_col %in% names(df)) {
    return(NULL)
  }

  effect_col <- mapping[["effect"]]
  se_col <- mapping[["se"]]

  if (!is.null(effect_col) && role_name_is_canonical(effect_col, "effect")) {
    return(NULL)
  }
  if (!is.null(se_col) && role_name_is_canonical(se_col, "se")) {
    return(NULL)
  }

  dof_col <- find_dof_column(df, exclude = unlist(mapping, use.names = FALSE))
  if (is.null(dof_col)) {
    return(NULL)
  }

  derived <- derive_pcc_from_t_dof(df[[t_col]], df[[dof_col]])
  coverage <- mean(is.finite(derived$effect) & is.finite(derived$se))
  if (coverage < MIN_DOF_COVERAGE) {
    return(NULL)
  }

  if (!is.null(effect_col) && reproduces_derived(df[[effect_col]], derived$effect)) {
    return(NULL)
  }

  replaces <- list()
  if (!is.null(effect_col)) replaces$effect <- effect_col
  if (!is.null(se_col)) replaces$se <- se_col

  list(
    t_stat = t_col,
    dof = dof_col,
    coverage = coverage,
    replaces = replaces
  )
}

#' @title Stem of a column name with its horizon number blanked out
#' @description `m12_res` and `m36_res` share the stem `m#_res`. Names that are
#'   nothing but digits, and names with no digits at all, have no stem.
#' @keywords internal
horizon_family_stem <- function(col_name_clean) {
  match <- regexpr("[0-9]+", col_name_clean)
  if (match == -1) {
    return(NA_character_)
  }
  stem <- paste0(
    substr(col_name_clean, 1, match - 1),
    "#",
    substr(col_name_clean, match + attr(match, "match.length"), nchar(col_name_clean))
  )
  if (!grepl("[a-z]", stem)) {
    return(NA_character_)
  }
  stem
}

#' @title Find families of response columns indexed by a horizon
#' @description A wide-format sheet stores one response per horizon in its own
#'   column: `m3_res, m6_res, m12_res, ...` beside `SE3, SE6, SE12, ...`. Each
#'   such family is a set of columns sharing a stem, differing only in an
#'   embedded number, and holding continuous measurements rather than the
#'   flags and counters that also carry numeric suffixes.
#' @param df *\[data.frame\]* The data frame.
#' @return *\[list\]* One entry per family: `stem`, `columns`, `horizons`.
find_horizon_families <- function(df) {
  cols <- names(df)
  if (length(cols) == 0) {
    return(list())
  }

  clean <- vapply(cols, clean_column_name, character(1), USE.NAMES = FALSE)
  stems <- vapply(clean, horizon_family_stem, character(1), USE.NAMES = FALSE)
  horizons <- vapply(clean, function(nm) {
    match <- regexpr("[0-9]+", nm)
    if (match == -1) NA_real_ else as.numeric(regmatches(nm, match))
  }, numeric(1), USE.NAMES = FALSE)

  keep <- !is.na(stems)
  if (!any(keep)) {
    return(list())
  }

  families <- list()
  for (stem in unique(stems[keep])) {
    idx <- which(keep & stems == stem)
    members <- cols[idx]
    member_horizons <- horizons[idx]

    if (length(unique(member_horizons)) < WIDE_MIN_MEMBERS) next

    usable <- vapply(members, function(col) {
      values <- suppressWarnings(as.numeric(df[[col]]))
      mean(is.finite(values)) >= MIN_DOF_COVERAGE && looks_like_continuous_measure(df[[col]])
    }, logical(1), USE.NAMES = FALSE)

    if (sum(usable) < WIDE_MIN_MEMBERS) next

    order_idx <- order(member_horizons[usable])
    families[[stem]] <- list(
      stem = stem,
      columns = members[usable][order_idx],
      horizons = sort(unique(member_horizons[usable]))
    )
  }

  families
}

#' @title Detect a wide-format (one column per horizon) sheet
#' @description Recognized positively rather than by exhaustion: several
#'   families of continuous response columns indexed by a horizon or lag, and
#'   no long-format effect/se pair for recognition to map. Reshaping such a
#'   sheet into one row per (study, horizon) is the user's job; artma declines
#'   the roles and says why.
#' @param df *\[data.frame\]* The data frame.
#' @param mapping *\[list\]* The mapping recognition arrived at.
#' @return *\[list\]* With `families` and `columns`, or `NULL`.
detect_wide_format <- function(df, mapping) {
  if (!is.null(mapping[["effect"]]) || !is.null(mapping[["se"]])) {
    return(NULL)
  }

  families <- find_horizon_families(df)
  if (length(families) < WIDE_MIN_FAMILIES) {
    return(NULL)
  }

  list(
    families = families,
    columns = unlist(lapply(families, function(f) f$columns), use.names = FALSE)
  )
}

#' @title Describe a wide-format detection in one line
#' @param detection *\[list\]* The result of `detect_wide_format()`.
#' @param max_families *\[integer, optional\]* Families to name before
#'   summarizing the rest. Defaults to 3.
#' @return *\[character\]* A single sentence naming the layout and the families.
describe_wide_format <- function(detection, max_families = 3L) {
  families <- detection$families
  shown <- utils::head(families, max_families)

  described <- vapply(shown, function(f) {
    sprintf("%s (%s)", f$stem, paste(f$columns, collapse = ", "))
  }, character(1), USE.NAMES = FALSE)

  rest <- length(families) - length(shown)
  if (rest > 0) {
    described <- c(described, sprintf("and %d more famil%s", rest, if (rest == 1) "y" else "ies"))
  }

  sprintf(
    "the sheet is in wide format, one column per horizon: %s",
    paste(described, collapse = "; ")
  )
}

#' @title Whether effect and se are configured as derived partial correlations
#' @description Reads the `data.derive_pcc` option. The data pipeline consults
#'   it in three places: the hard-required raw column set, the configure phase,
#'   and the compute phase.
#' @return *\[logical\]* TRUE when the derived route is configured.
pcc_derivation_active <- function() {
  isTRUE(getOption("artma.data.derive_pcc", FALSE))
}

#' @title Derive the effect and standard error columns from (t, df)
#' @description Compute-phase step, run on the standardized frame before the
#'   data config is primed so the derived columns are configured, cleaned,
#'   winsorized and validated exactly like read ones. A no-op unless
#'   `data.derive_pcc` is set.
#' @param df *\[data.frame\]* The standardized data frame.
#' @return *\[data.frame\]* The frame, with `effect` and `se` derived.
derive_pcc_columns <- function(df) {
  box::use(artma / libs / core / log[log_info])

  if (!pcc_derivation_active()) {
    return(df)
  }

  missing_inputs <- setdiff(c("t_stat", "reg_dof"), colnames(df))
  if (length(missing_inputs) > 0) {
    cli::cli_abort(c(
      "x" = "Cannot derive {.field effect} and {.field se}: {.val {missing_inputs}} {?is/are} missing.",
      "i" = "The {.field data.derive_pcc} option needs both a t-statistic and a degrees-of-freedom column mapped.",
      "i" = "Map them with {.code artma::config_set(\"t_stat\", source_name = \"<column>\")} and the same for {.val reg_dof}, or set {.field data.derive_pcc} to {.val FALSE}."
    ))
  }

  derived <- derive_pcc_from_t_dof(df$t_stat, df$reg_dof)
  df$effect <- derived$effect
  df$se <- derived$se

  log_info(
    "Derived {.field effect} and {.field se} as partial correlations from {.field t_stat} and {.field reg_dof}."
  )

  df
}

box::export(
  DOF_NAME_PATTERNS,
  WIDE_MIN_FAMILIES,
  WIDE_MIN_MEMBERS,
  derive_pcc_columns,
  derive_pcc_from_t_dof,
  describe_wide_format,
  detect_tdf_derivation,
  detect_wide_format,
  find_dof_column,
  find_horizon_families,
  looks_like_dof,
  pcc_derivation_active,
  role_name_is_canonical
)
