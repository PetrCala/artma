box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_match,
    expect_no_match,
    expect_true,
    local_edition,
    test_that
  ],
  withr[local_options, local_tempdir]
)

box::use(
  artma / report / render[
    build_report_html,
    render_report,
    resolve_method_pngs,
    sanitize_anchor
  ]
)

# Build a small synthetic results list mirroring what artma() returns: one
# new_method_result per method, plus the skip/fail attributes.
make_results <- function() {
  results <- list(
    effect_summary_stats = list(
      tables = list(summary = data.frame(
        statistic = c("mean", "median"),
        estimate = c(0.523456, 0.400001),
        mark = c("**", ""),
        stringsAsFactors = FALSE
      )),
      plots = list(),
      meta = list()
    ),
    bma = list(
      tables = list(),
      plots = list(),
      meta = list(skipped = "package BMS is not installed")
    )
  )
  attr(results, "skipped_methods") <- c(fma = "missing required column: se")
  attr(results, "failed_methods") <- c(maive = "instrument was too weak")
  results
}

test_that("sanitize_anchor produces stable, safe anchors", {
  expect_equal(sanitize_anchor("effect_summary_stats"), "section-effect-summary-stats")
  expect_equal(sanitize_anchor("Best Practice!"), "section-best-practice")
})

test_that("build_report_html renders the metadata header", {
  html <- build_report_html(
    make_results(),
    report_meta = list(
      package_version = "9.9.9",
      data_source = "/data/meta.csv",
      options_file = "my.yaml",
      autonomy_level = "balanced"
    ),
    round_to = 3
  )

  expect_match(html, "9.9.9")
  expect_match(html, "/data/meta.csv", fixed = TRUE)
  expect_match(html, "my.yaml", fixed = TRUE)
  expect_match(html, "balanced")
})

test_that("build_report_html lists every method in the TOC with anchors", {
  html <- build_report_html(make_results(), round_to = 3)

  expect_match(html, "href=\"#section-effect-summary-stats\"", fixed = TRUE)
  expect_match(html, "id=\"section-effect-summary-stats\"", fixed = TRUE)
  expect_match(html, "href=\"#section-bma\"", fixed = TRUE)
  expect_match(html, "href=\"#section-fma\"", fixed = TRUE)
  expect_match(html, "href=\"#section-maive\"", fixed = TRUE)
})

test_that("build_report_html formats numeric cells to the configured decimals", {
  html <- build_report_html(make_results(), round_to = 3)
  expect_match(html, ">0.523<", fixed = TRUE)
  expect_match(html, ">0.400<", fixed = TRUE)

  html2 <- build_report_html(make_results(), round_to = 1)
  expect_match(html2, ">0.5<", fixed = TRUE)
  expect_no_match(html2, ">0.523<", fixed = TRUE)
})

test_that("build_report_html honors the number_of_decimals option by default", {
  local_options(artma.output.number_of_decimals = 2)
  html <- build_report_html(make_results())
  expect_match(html, ">0.52<", fixed = TRUE)
})

test_that("build_report_html passes significance marks through untouched", {
  html <- build_report_html(make_results(), round_to = 3)
  expect_match(html, ">**<", fixed = TRUE)
})

test_that("build_report_html renders skip-reason sections, not silent drops", {
  html <- build_report_html(make_results(), round_to = 3)
  # Self-skipped method (meta$skipped)
  expect_match(html, "package BMS is not installed", fixed = TRUE)
  # Gate-skipped method (skipped_methods attribute)
  expect_match(html, "missing required column: se", fixed = TRUE)
  # Failed method (failed_methods attribute)
  expect_match(html, "instrument was too weak", fixed = TRUE)
})

test_that("resolve_method_pngs matches by method name and alias", {
  gdir <- local_tempdir()
  file.create(file.path(gdir, "funnel_plot_effect_precision.png"))
  file.create(file.path(gdir, "prima_facie_1.png"))
  file.create(file.path(gdir, "unrelated.png"))

  funnel <- resolve_method_pngs("funnel_plot", gdir)
  expect_equal(basename(funnel), "funnel_plot_effect_precision.png")

  prima <- resolve_method_pngs("prima_facie_graphs", gdir)
  expect_equal(basename(prima), "prima_facie_1.png")

  expect_equal(length(resolve_method_pngs("funnel_plot", NULL)), 0L)
})

# A tiny valid PNG (1x1 transparent) written to disk for embedding tests.
write_tiny_png <- function(path) {
  bytes <- as.raw(c(
    0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x0d,
    0x49, 0x48, 0x44, 0x52, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
    0x08, 0x06, 0x00, 0x00, 0x00, 0x1f, 0x15, 0xc4, 0x89, 0x00, 0x00, 0x00,
    0x0a, 0x49, 0x44, 0x41, 0x54, 0x78, 0x9c, 0x63, 0x00, 0x01, 0x00, 0x00,
    0x05, 0x00, 0x01, 0x0d, 0x0a, 0x2d, 0xb4, 0x00, 0x00, 0x00, 0x00, 0x49,
    0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82
  ))
  writeBin(bytes, path)
}

test_that("build_report_html embeds plots as base64 data URIs", {
  gdir <- local_tempdir()
  write_tiny_png(file.path(gdir, "funnel_plot_effect_precision.png"))

  results <- list(
    funnel_plot = list(
      tables = list(),
      plots = list(funnel_plot = "a-plot-object"),
      meta = list()
    )
  )
  html <- build_report_html(results, graphics_dir = gdir)

  expect_match(html, "src=\"data:image/png;base64,", fixed = TRUE)
})

test_that("build_report_html notes absent plot files instead of erroring", {
  results <- list(
    funnel_plot = list(
      tables = list(),
      plots = list(funnel_plot = "a-plot-object"),
      meta = list()
    )
  )
  html <- build_report_html(results, graphics_dir = local_tempdir())
  expect_match(html, "were not found", fixed = TRUE)
})

test_that("the rendered report is fully self-contained", {
  gdir <- local_tempdir()
  write_tiny_png(file.path(gdir, "funnel_plot_effect_precision.png"))

  results <- list(
    funnel_plot = list(
      tables = list(summary = data.frame(x = 1.5)),
      plots = list(funnel_plot = "a-plot-object"),
      meta = list()
    )
  )
  html <- build_report_html(results, graphics_dir = gdir)

  # No external network references of any kind.
  expect_no_match(html, "http://", fixed = TRUE)
  expect_no_match(html, "https://", fixed = TRUE)

  # No src= or href= pointing at a file path; every img src is a data: URI,
  # and every link is an in-page anchor.
  src_values <- regmatches(html, gregexpr("src=\"[^\"]*\"", html))[[1]]
  for (src in src_values) {
    expect_match(src, "src=\"data:", fixed = TRUE)
  }
  href_values <- regmatches(html, gregexpr("href=\"[^\"]*\"", html))[[1]]
  for (href in href_values) {
    expect_match(href, "href=\"#", fixed = TRUE)
  }
})

test_that("the generated report contains no en dashes or em dashes", {
  html <- build_report_html(make_results(), round_to = 3)
  expect_false(grepl("–", html))
  expect_false(grepl("—", html))
})

test_that("render_report writes the HTML file to disk", {
  out_dir <- local_tempdir()
  out_file <- file.path(out_dir, "report.html")

  path <- render_report(
    make_results(),
    output_file = out_file,
    report_meta = list(package_version = "1.0.0"),
    open = FALSE
  )

  expect_equal(path, out_file)
  expect_true(file.exists(out_file))
  contents <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
  expect_match(contents, "<!DOCTYPE html>", fixed = TRUE)
  expect_true(nchar(contents) > 1000)
})
