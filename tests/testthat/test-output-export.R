box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_match,
    expect_null,
    expect_true,
    test_that
  ],
  withr[local_envvar, local_options, local_tempdir],
  artma / output / export[
    begin_run_output_dir,
    clear_run_output_dir,
    latest_run_output_dir,
    resolve_base_output_dir,
    resolve_output_dir,
    resolve_graphics_dir,
    ensure_output_dirs,
    export_results
  ]
)

# Path separators differ between what file.path() builds from a Windows
# tempdir and what dirname() hands back, so compare directories normalized.
same_dir <- function(path) normalizePath(path, winslash = "/", mustWork = FALSE)

# Keep the run directory a test starts out of the options of the next one.
local_no_active_run <- function(.local_envir = parent.frame()) {
  local_options(artma.temp.run_output_dir = NULL, .local_envir = .local_envir)
}

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

# run subdirectories --------------------------------------------------------

test_that("begin_run_output_dir writes into the output dir itself by default", {
  base <- local_tempdir()
  local_no_active_run()
  local_options(artma.output.dir = base)

  expect_equal(begin_run_output_dir(base), base)
  expect_equal(resolve_output_dir(), base)
  expect_false(dir.exists(file.path(base, "runs")))
})

test_that("begin_run_output_dir picks a timestamped subdirectory when enabled", {
  base <- local_tempdir()
  local_no_active_run()
  local_options(artma.output.dir = base, artma.output.run_subdirectories = TRUE)

  run_dir <- begin_run_output_dir(base, time = as.POSIXct("2026-08-25 14:30:12", tz = "UTC"))

  expect_equal(same_dir(dirname(run_dir)), same_dir(file.path(base, "runs")))
  expect_match(basename(run_dir), "^\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}$")
  # The base output directory is untouched until the run writes into its own.
  expect_false(dir.exists(file.path(base, "tables")))
})

test_that("begin_run_output_dir never hands two runs the same directory", {
  base <- local_tempdir()
  local_no_active_run()
  local_options(artma.output.dir = base, artma.output.run_subdirectories = TRUE)
  stamp <- as.POSIXct("2026-08-25 14:30:12", tz = "UTC")

  first <- begin_run_output_dir(base, time = stamp)
  ensure_output_dirs(first)
  second <- begin_run_output_dir(base, time = stamp)

  expect_false(identical(first, second))
  expect_equal(basename(second), paste0(basename(first), "-2"))
})

test_that("resolve_output_dir returns the run directory while a run executes", {
  base <- local_tempdir()
  local_no_active_run()
  local_options(artma.output.dir = base, artma.output.run_subdirectories = TRUE)

  run_dir <- begin_run_output_dir(base)

  expect_equal(resolve_output_dir(), run_dir)
  # Plots therefore land with the rest of the run's output.
  expect_equal(resolve_graphics_dir(resolve_output_dir()), file.path(run_dir, "graphics"))
  expect_equal(resolve_base_output_dir(), base)

  clear_run_output_dir()
  expect_equal(resolve_output_dir(), base)
})

test_that("resolve_output_dir falls back to the latest run once no run is active", {
  base <- local_tempdir()
  local_no_active_run()
  local_options(artma.output.dir = base, artma.output.run_subdirectories = TRUE)

  older <- begin_run_output_dir(base, time = as.POSIXct("2026-08-25 14:30:12", tz = "UTC"))
  ensure_output_dirs(older)
  newer <- begin_run_output_dir(base, time = as.POSIXct("2026-08-25 15:02:00", tz = "UTC"))
  ensure_output_dirs(newer)
  clear_run_output_dir()

  expect_equal(latest_run_output_dir(base), newer)
  expect_equal(resolve_output_dir(), newer)
})

test_that("latest_run_output_dir is NULL when no run subdirectory exists", {
  base <- local_tempdir()

  expect_null(latest_run_output_dir(base))
  expect_null(latest_run_output_dir(file.path(base, "missing")))
})

test_that("resolve_output_dir ignores run subdirectories left by an earlier opt-in", {
  base <- local_tempdir()
  local_no_active_run()
  local_options(artma.output.dir = base, artma.output.run_subdirectories = TRUE)
  ensure_output_dirs(begin_run_output_dir(base))
  clear_run_output_dir()

  local_options(artma.output.run_subdirectories = FALSE)
  expect_equal(resolve_output_dir(), base)
})

# resolve_graphics_dir ------------------------------------------------------

test_that("resolve_graphics_dir joins a relative export subdirectory", {
  local_options(artma.visualization.export_path = "graphics")
  expect_equal(resolve_graphics_dir("/base"), file.path("/base", "graphics"))
})

test_that("resolve_graphics_dir returns an absolute export path as-is", {
  local_options(artma.visualization.export_path = "/tmp/artma-graphics")
  expect_equal(resolve_graphics_dir("/base"), "/tmp/artma-graphics")
})

test_that("resolve_graphics_dir expands a tilde before testing for absoluteness", {
  local_options(artma.visualization.export_path = "~/artma-graphics")
  expect_equal(resolve_graphics_dir("/base"), path.expand("~/artma-graphics"))
})

test_that("resolve_graphics_dir accepts an explicit export path over the option", {
  local_options(artma.visualization.export_path = "graphics")
  expect_equal(resolve_graphics_dir("/base", "figs"), file.path("/base", "figs"))
  expect_equal(resolve_graphics_dir("/base", "/tmp/figs"), "/tmp/figs")
})

# The writers read their target directory from get_visualization_options(),
# not from resolve_graphics_dir() directly. When those two disagreed, an
# absolute export path was created in one place and written to in another.
test_that("get_visualization_options resolves the same directory as ensure_output_dirs", {
  base <- local_tempdir()
  graphics <- file.path(local_tempdir(), "elsewhere")
  local_options(
    artma.output.dir = base,
    artma.output.save_results = TRUE,
    artma.visualization.export_path = graphics
  )

  box::use(artma / visualization / options[get_visualization_options])

  expect_equal(get_visualization_options()$export_path, resolve_graphics_dir(base))
  expect_equal(get_visualization_options()$export_path, graphics)
})

test_that("get_visualization_options joins a relative export path to the output dir", {
  base <- local_tempdir()
  local_options(
    artma.output.dir = base,
    artma.output.save_results = TRUE,
    artma.visualization.export_path = "graphics"
  )

  box::use(artma / visualization / options[get_visualization_options])

  expect_equal(get_visualization_options()$export_path, file.path(base, "graphics"))
})

test_that("ensure_output_dirs honours an absolute export path", {
  base <- local_tempdir()
  graphics <- file.path(local_tempdir(), "elsewhere")
  local_options(
    artma.output.dir = base,
    artma.visualization.export_path = graphics
  )

  ensure_output_dirs(base)

  expect_true(dir.exists(graphics))
  expect_false(dir.exists(file.path(base, graphics)))
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

# estimates slot ------------------------------------------------------------

make_estimates <- function() {
  box::use(artma / modules / runtime_methods[new_estimates])
  new_estimates(data.frame(
    method = "linear_tests",
    model = c("ols", "fixed_effects"),
    term = "publication_bias",
    estimate = c(-0.1294827361, -0.1281193744),
    std_error = c(0.0451927364, 0.0483112947),
    p_value = c(0.0042719384, 0.0081264739),
    stringsAsFactors = FALSE
  ))
}

test_that("export_results writes estimates as <method>.csv and moves the display table", {
  dir <- setup_output_dir()
  display <- data.frame(Metric = "Publication Bias", OLS = "-0.129***")

  export_results(
    list(linear_tests = list(tables = list(summary = display), estimates = make_estimates())),
    dir
  )

  expect_true(file.exists(file.path(dir, "tables", "linear_tests.csv")))
  expect_true(file.exists(file.path(dir, "tables", "linear_tests_display.csv")))

  written_display <- utils::read.csv(file.path(dir, "tables", "linear_tests_display.csv"), stringsAsFactors = FALSE)
  expect_equal(written_display$OLS, "-0.129***")
})

test_that("exported estimates round-trip as unrounded numerics", {
  dir <- setup_output_dir()
  local_options(artma.output.number_of_decimals = 3)
  estimates <- make_estimates()

  export_results(list(linear_tests = list(tables = list(summary = data.frame(a = 1)), estimates = estimates)), dir)

  written <- utils::read.csv(file.path(dir, "tables", "linear_tests.csv"), stringsAsFactors = FALSE)
  expect_true(is.numeric(written$estimate))
  expect_true(is.numeric(written$std_error))
  expect_true(is.numeric(written$p_value))
  expect_equal(written$estimate, estimates$estimate)
  # Display precision must not leak into the machine-readable artifact.
  expect_false(any(written$estimate == round(written$estimate, 3)))
})

test_that("export_results keeps sub-table names when estimates are present", {
  dir <- setup_output_dir()

  export_results(
    list(p_hacking_tests = list(
      tables = list(summary = data.frame(a = 1), caliper = data.frame(x = 1)),
      estimates = make_estimates()
    )),
    dir
  )

  files <- list.files(file.path(dir, "tables"))
  expect_true(all(c(
    "p_hacking_tests.csv", "p_hacking_tests_display.csv", "p_hacking_tests_caliper.csv"
  ) %in% files))
})

test_that("export_results leaves display names alone without an estimates slot", {
  dir <- setup_output_dir()

  export_results(list(bma = list(tables = list(summary = data.frame(a = 1)))), dir)

  expect_equal(list.files(file.path(dir, "tables")), "bma.csv")
})

test_that("an empty estimates frame writes no header-only CSV", {
  box::use(artma / modules / runtime_methods[new_estimates])
  dir <- setup_output_dir()

  export_results(
    list(funnel_plot = list(tables = list(), estimates = new_estimates())),
    dir
  )

  expect_equal(list.files(file.path(dir, "tables")), character())

  # An empty frame also leaves the display table under its own name.
  export_results(
    list(bma = list(tables = list(summary = data.frame(a = 1)), estimates = new_estimates())),
    dir
  )

  expect_equal(list.files(file.path(dir, "tables")), "bma.csv")
})

test_that("estimates are never written as LaTeX and the display table keeps <method>.tex", {
  dir <- setup_output_dir()
  local_options(artma.output.table_formats = c("csv", "tex"))

  export_results(
    list(linear_tests = list(
      tables = list(summary = data.frame(Metric = "Publication Bias", OLS = "-0.129***")),
      estimates = make_estimates()
    )),
    dir
  )

  contents <- readLines(file.path(dir, "tables", "linear_tests.tex"))
  expect_true(any(grepl("-0.129", contents, fixed = TRUE)))
  expect_false(file.exists(file.path(dir, "tables", "linear_tests_display.tex")))
})
