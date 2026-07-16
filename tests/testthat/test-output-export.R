box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_true,
    test_that
  ],
  withr[local_options, local_tempdir],
  artma / output / export[
    resolve_output_dir,
    resolve_graphics_dir,
    ensure_output_dirs,
    export_results
  ]
)

# resolve_output_dir --------------------------------------------------------

test_that("resolve_output_dir falls back to a session temp dir when auto", {
  local_options(artma.output.dir = "auto")
  expect_equal(resolve_output_dir(), file.path(tempdir(), "artma-results"))
})

test_that("resolve_output_dir returns an explicit path unchanged", {
  local_options(artma.output.dir = "/some/explicit/path")
  expect_equal(resolve_output_dir(), "/some/explicit/path")
})

# resolve_graphics_dir ------------------------------------------------------

test_that("resolve_graphics_dir joins the export subdirectory", {
  local_options(artma.visualization.export_path = "graphics")
  expect_equal(resolve_graphics_dir("/base"), file.path("/base", "graphics"))
})

# ensure_output_dirs --------------------------------------------------------

test_that("ensure_output_dirs creates the tables and graphics subdirectories", {
  dir <- local_tempdir()
  local_options(
    artma.output.dir = dir,
    artma.visualization.export_path = "graphics"
  )

  ensure_output_dirs(dir)

  expect_true(dir.exists(file.path(dir, "tables")))
  expect_true(dir.exists(file.path(dir, "graphics")))
})

# export_results ------------------------------------------------------------

setup_output_dir <- function() {
  dir <- local_tempdir(.local_envir = parent.frame())
  local_options(
    artma.output.dir = dir,
    artma.visualization.export_path = "graphics",
    artma.verbose = 1,
    .local_envir = parent.frame()
  )
  ensure_output_dirs(dir)
  dir
}

test_that("export_results writes a single summary table as <method>.csv", {
  dir <- setup_output_dir()
  df <- data.frame(term = c("a", "b"), estimate = c(1.5, 2.5))

  export_results(list(bma = list(tables = list(summary = df))), dir)

  path <- file.path(dir, "tables", "bma.csv")
  expect_true(file.exists(path))
  written <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(written$term, df$term)
  expect_equal(written$estimate, df$estimate)
})

test_that("export_results names sub-tables as <method>_<key>.csv", {
  dir <- setup_output_dir()
  caliper <- data.frame(x = 1)
  elliott <- data.frame(y = 2)
  maive <- data.frame(z = 3)

  export_results(
    list(p_hacking_tests = list(tables = list(caliper = caliper, elliott = elliott, maive = maive))),
    dir
  )

  expect_true(file.exists(file.path(dir, "tables", "p_hacking_tests_caliper.csv")))
  expect_true(file.exists(file.path(dir, "tables", "p_hacking_tests_elliott.csv")))
  expect_true(file.exists(file.path(dir, "tables", "p_hacking_tests_maive.csv")))
})

test_that("export_results treats generic and method-name keys as <method>.csv", {
  dir <- setup_output_dir()

  export_results(
    list(
      m1 = list(tables = list(coefficients = data.frame(a = 1))),
      m2 = list(tables = list(m2 = data.frame(b = 2)))
    ),
    dir
  )

  expect_true(file.exists(file.path(dir, "tables", "m1.csv")))
  expect_true(file.exists(file.path(dir, "tables", "m2.csv")))
})

test_that("export_results ignores plots and meta and skips NULL results", {
  dir <- setup_output_dir()

  export_results(
    list(
      with_plot = list(
        tables = list(summary = data.frame(a = 1)),
        plots = list(p = "not a table"),
        meta = list(model = 1:10)
      ),
      empty = NULL
    ),
    dir
  )

  files <- list.files(file.path(dir, "tables"))
  expect_true("with_plot.csv" %in% files)
  # No files leak out of the plots/meta slots.
  expect_equal(length(files), 1L)
})

test_that("export_results skips non-data-frame entries in the tables slot", {
  dir <- setup_output_dir()

  export_results(
    list(m = list(tables = list(summary = data.frame(a = 1), junk = 1:5))),
    dir
  )

  expect_true(file.exists(file.path(dir, "tables", "m.csv")))
  expect_false(file.exists(file.path(dir, "tables", "m_junk.csv")))
})
