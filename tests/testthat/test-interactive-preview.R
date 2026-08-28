box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_true,
    test_that
  ]
)

box::use(
  artma / interactive / preview[
    column_overview_frame,
    count_studies,
    preview_menu_items,
    render_data_overview,
    render_study_summary,
    run_preview_menu,
    study_overview_frame
  ]
)

# A prepared frame with known health metrics: t values 5, 2, 5.833 and -0.5,
# so 3 of the 4 estimates clear the 5% threshold.
preview_df <- function() {
  data.frame(
    study_id = c("s1", "s1", "s2", "s2"),
    effect = c(0.5, 0.3, 0.7, -0.2),
    se = c(0.1, 0.15, 0.12, 0.4),
    country = c("cz", "cz", "de", NA),
    stringsAsFactors = FALSE
  )
}

# Sequenced single-select backend, as in test-session-hub.R: each call
# consumes the next script entry and returns the value of the first label
# containing it.
make_select_fn <- function(script) {
  index <- 0L
  function(choices, prompt, selected = NULL, descriptions = NULL) {
    index <<- index + 1L
    stopifnot(index <= length(script))
    pattern <- script[[index]]
    if (is.na(pattern)) {
      return(character(0))
    }
    labels <- names(choices) %||% choices
    matches <- unname(choices[grepl(pattern, labels, fixed = TRUE)])
    stopifnot(length(matches) >= 1L)
    matches[[1]]
  }
}

test_that("the overview reports size, missingness and the health panel", {
  messages <- testthat::capture_messages(render_data_overview(preview_df()))
  text <- paste(messages, collapse = "\n")

  expect_true(grepl("4 rows, 4 columns", text))
  expect_true(grepl("2 studies", text))
  expect_true(grepl("1 missing value", text))
  expect_true(grepl("Mean effect: 0.325", text))
  expect_true(grepl("precision-weighted", text))
  expect_true(grepl("Sign split: 3 positive, 1 negative", text))
  expect_true(grepl("Significant at 5%: 3 of 4 estimates (75%)", text, fixed = TRUE))
  expect_true(grepl("Largest |t|: 5.833", text, fixed = TRUE))
  expect_true(grepl("Effect range: -0.2 to 0.7", text))
  expect_true(grepl("SE range: 0.1 to 0.4", text))
})

test_that("the overview degrades without numeric effect and se columns", {
  df <- data.frame(x = c("a", "b"), stringsAsFactors = FALSE)
  messages <- testthat::capture_messages(render_data_overview(df))
  text <- paste(messages, collapse = "\n")

  expect_true(grepl("2 rows, 1 column", text))
  expect_true(grepl("No missing values", text))
  expect_false(grepl("Mean effect", text))
  expect_false(grepl("Effect range", text))
})

test_that("count_studies is NA without the study column", {
  expect_equal(count_studies(preview_df()), 2L)
  expect_true(is.na(count_studies(data.frame(x = 1:3))))
})

test_that("column_overview_frame assigns roles, sources and summaries", {
  withr::local_options(list(
    artma.data.columns = list(effect = list(source_name = "Coef"))
  ))

  frame <- column_overview_frame(preview_df())

  expect_equal(frame$column, c("study_id", "effect", "se", "country"))
  expect_equal(frame$role, c("required", "required", "required", "moderator"))
  # Only the genuinely renamed column names its source.
  expect_equal(frame$source, c("", "Coef", "", ""))
  expect_equal(frame$missing, c(0L, 0L, 0L, 1L))
  expect_true(grepl("min -0.2, median 0.4, max 0.7", frame$summary[[2]]))
  expect_equal(frame$summary[[4]], "2 unique values")
})

test_that("column_overview_frame labels computed columns", {
  df <- preview_df()
  df$t_stat <- df$effect / df$se
  frame <- column_overview_frame(df)
  expect_equal(frame$role[frame$column == "t_stat"], "computed")
})

test_that("study_overview_frame sorts by estimate count and averages per study", {
  df <- preview_df()
  df$study_id <- c("s1", "s1", "s1", "s2")

  frame <- study_overview_frame(df)

  expect_equal(frame$study, c("s1", "s2"))
  expect_equal(frame$estimates, c(3L, 1L))
  expect_equal(frame$mean_effect, c(0.5, -0.2))
  expect_equal(frame$mean_se, c(signif(mean(c(0.1, 0.15, 0.12)), 4), 0.4))
})

test_that("render_study_summary caps the table and says so", {
  df <- data.frame(
    study_id = rep(sprintf("study_%02d", 1:12), each = 2),
    effect = rep(0.1, 24),
    se = rep(0.05, 24),
    stringsAsFactors = FALSE
  )

  messages <- testthat::capture_messages(render_study_summary(df, width = 100))
  text <- paste(messages, collapse = "\n")

  expect_true(grepl("12 studies; estimates per study: min 2, median 2, max 2", text))
  expect_true(grepl("Showing the 10 largest of 12 studies", text))
})

test_that("render_study_summary explains an unresolved study column", {
  messages <- testthat::capture_messages(
    render_study_summary(data.frame(effect = 1:3))
  )
  expect_true(any(grepl("study column is not resolved", messages)))
})

test_that("preview_menu_items offers the viewer only when available", {
  values <- function(items) vapply(items, function(item) item$value, character(1))

  expect_equal(
    values(preview_menu_items(TRUE)),
    c("overview", "columns", "studies", "view", "back")
  )
  expect_equal(
    values(preview_menu_items(FALSE)),
    c("overview", "columns", "studies", "back")
  )
})

test_that("run_preview_menu dispatches the submenu items", {
  viewed <- 0L

  messages <- testthat::capture_messages(run_preview_menu(
    preview_df(),
    view_data = function(frame) viewed <<- viewed + 1L,
    select_fn = make_select_fn(c("Columns", "Studies", "Open spreadsheet viewer", "Back")),
    width = 100,
    viewer_available = function() TRUE
  ))
  text <- paste(messages, collapse = "\n")

  # The overview renders on entry, before any submenu pick.
  expect_true(grepl("4 rows, 4 columns", text))
  # The Columns screen: header plus one line per column.
  expect_true(grepl("column", text))
  expect_true(grepl("moderator", text))
  # The Studies screen.
  expect_true(grepl("2 studies; estimates per study", text))
  expect_equal(viewed, 1L)
})

test_that("run_preview_menu hides the viewer and survives a cancelled menu", {
  messages <- testthat::capture_messages(run_preview_menu(
    preview_df(),
    view_data = function(frame) stop("view_data must not be called"),
    select_fn = make_select_fn(NA),
    width = 100,
    viewer_available = function() FALSE
  ))

  expect_true(any(grepl("4 rows, 4 columns", messages)))
})
