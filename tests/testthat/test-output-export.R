box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_match,
    expect_true,
    test_that
  ],
  withr[local_envvar, local_options, local_tempdir],
  artma / output / export[
    resolve_output_dir,
    resolve_graphics_dir,
    ensure_output_dirs,
    export_results
  ]
)

# resolve_output_dir --------------------------------------------------------

# Redirect R_user_dir so auto resolution never touches the real user data dir.
local_user_data_dir <- function(.local_envir = parent.frame()) {
  dir <- local_tempdir(.local_envir = .local_envir)
  local_envvar(R_USER_DATA_DIR = dir, .local_envir = .local_envir)
  dir
}

test_that("resolve_output_dir maps auto to a per-options-file user data dir", {
  base <- local_user_data_dir()
  local_options(
    artma.output.dir = "auto",
    artma.temp.file_name = "my_config.yaml"
  )
  expect_equal(
    resolve_output_dir(),
    file.path(tools::R_user_dir("artma", "data"), "results", "my_config")
  )
  expect_true(startsWith(resolve_output_dir(), base))
})

test_that("resolve_output_dir gives distinct auto dirs for distinct options files", {
  local_user_data_dir()
  local_options(artma.output.dir = "auto", artma.temp.file_name = "a.yaml")
  dir_a <- resolve_output_dir()
  local_options(artma.temp.file_name = "b.yaml")
  dir_b <- resolve_output_dir()

  expect_false(identical(dir_a, dir_b))
  expect_equal(basename(dir_a), "a")
  expect_equal(basename(dir_b), "b")
})

test_that("resolve_output_dir uses a default auto subdir without an options file", {
  local_user_data_dir()
  local_options(artma.output.dir = "auto", artma.temp.file_name = NULL)
  expect_equal(basename(resolve_output_dir()), "default")
})

test_that("resolve_output_dir sanitizes the options file stem", {
  local_user_data_dir()
  local_options(
    artma.output.dir = "auto",
    artma.temp.file_name = "My Config (v2).yaml"
  )
  expect_match(basename(resolve_output_dir()), "^[A-Za-z0-9._-]+$")
  expect_equal(basename(resolve_output_dir()), "My_Config_v2")
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

test_that("ensure_output_dirs leaves the auto option untouched", {
  local_user_data_dir()
  local_options(
    artma.output.dir = "auto",
    artma.temp.file_name = "cfg.yaml",
    artma.visualization.export_path = "graphics"
  )

  ensure_output_dirs(resolve_output_dir())

  # The resolved path must never be written back over "auto", neither in the
  # session options nor in the options file (issue #321, bug 3).
  expect_equal(getOption("artma.output.dir"), "auto")
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

# table formats -------------------------------------------------------------

test_that("export_results writes CSV only by default", {
  dir <- setup_output_dir()

  export_results(list(bma = list(tables = list(summary = data.frame(a = 1)))), dir)

  expect_equal(list.files(file.path(dir, "tables")), "bma.csv")
})

test_that("export_results writes LaTeX alongside CSV when both formats are set", {
  dir <- setup_output_dir()
  local_options(artma.output.table_formats = c("csv", "tex"))

  export_results(list(bma = list(tables = list(summary = data.frame(term = "a", est = 1)))), dir)

  tex_path <- file.path(dir, "tables", "bma.tex")
  expect_true(file.exists(file.path(dir, "tables", "bma.csv")))
  expect_true(file.exists(tex_path))

  contents <- readLines(tex_path)
  expect_true(any(grepl("\\begin{tabular}", contents, fixed = TRUE)))
  expect_true("\\label{tab:bma}" %in% contents)
})

test_that("export_results writes LaTeX only when tex is the sole format", {
  dir <- setup_output_dir()
  local_options(artma.output.table_formats = "tex")

  export_results(list(bma = list(tables = list(summary = data.frame(a = 1)))), dir)

  expect_equal(list.files(file.path(dir, "tables")), "bma.tex")
})

test_that("export_results falls back to CSV for unrecognised formats", {
  dir <- setup_output_dir()
  local_options(artma.output.table_formats = "docx")

  export_results(list(bma = list(tables = list(summary = data.frame(a = 1)))), dir)

  expect_equal(list.files(file.path(dir, "tables")), "bma.csv")
})

test_that("export_results applies the sub-table naming rule to LaTeX files", {
  dir <- setup_output_dir()
  local_options(artma.output.table_formats = "tex")

  export_results(
    list(p_hacking_tests = list(tables = list(caliper = data.frame(x = 1), elliott = data.frame(y = 2)))),
    dir
  )

  expect_true(file.exists(file.path(dir, "tables", "p_hacking_tests_caliper.tex")))
  expect_true(file.exists(file.path(dir, "tables", "p_hacking_tests_elliott.tex")))
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
