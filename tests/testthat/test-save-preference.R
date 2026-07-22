box::use(
  testthat[
    expect_equal,
    expect_true,
    test_that
  ]
)

box::use(
  artma / interactive / save_preference[save_to_options_file],
  artma / options / files[options_file_path, read_options_file, write_options_file]
)

# save_to_options_file writes a single nested option path into an existing
# file. It must create every missing intermediate level, at any depth, rather
# than silently writing nothing and still returning TRUE.

setup_file <- function() {
  tmp_dir <- withr::local_tempdir(.local_envir = parent.frame())
  file_name <- "prefs.yaml"
  write_options_file(options_file_path(tmp_dir, file_name), list(general = list(name = "x")))
  withr::local_options(
    list(
      "artma.verbose" = 1,
      "artma.temp.file_name" = file_name,
      "artma.temp.dir_name" = tmp_dir
    ),
    .local_envir = parent.frame()
  )
  options_file_path(tmp_dir, file_name)
}

test_that("save_to_options_file writes shallow paths", {
  path <- setup_file()

  expect_true(save_to_options_file("data.na_handling", "drop"))
  expect_equal(read_options_file(path)$data$na_handling, "drop")
})

test_that("save_to_options_file writes a three-level path", {
  path <- setup_file()

  expect_true(save_to_options_file("methods.box_plot.max", 60L))
  expect_equal(read_options_file(path)$methods$box_plot$max, 60L)
})

test_that("save_to_options_file writes a four-level path instead of no-op", {
  path <- setup_file()

  expect_true(save_to_options_file("a.b.c.d", "deep"))
  expect_equal(read_options_file(path)$a$b$c$d, "deep")
})
