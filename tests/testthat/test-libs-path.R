box::use(
  testthat[
    test_that,
    expect_error,
    expect_equal,
    expect_identical,
    expect_null,
    expect_length,
    expect_true
  ]
)

test_that("turn_path_into_box_importable validates input", {
  box::use(artma / libs / path[turn_path_into_box_importable])

  expect_error(
    turn_path_into_box_importable(1),
    class = "rlang_error",
    regexp = "Invalid path"
  )

  expect_error(
    turn_path_into_box_importable(character()),
    class = "rlang_error",
    regexp = "Invalid path"
  )

  expect_error(
    turn_path_into_box_importable("tests/testthat/nope.R"),
    class = "rlang_error",
    regexp = "does not exist"
  )
})

test_that("turn_path_into_box_importable returns NULL when package root is absent", {
  box::use(artma / libs / path[turn_path_into_box_importable])

  tmp_file <- withr::local_tempfile(fileext = ".R")
  writeLines("cat('demo')\n", tmp_file)

  expect_null(turn_path_into_box_importable(tmp_file))
})

test_that("turn_path_into_box_importable returns a package scoped import path", {
  box::use(artma / libs / path[turn_path_into_box_importable])

  pkg_path <- file.path("inst", "artma", "libs", "path.R")
  expected <- "artma/libs/path"

  expect_equal(turn_path_into_box_importable(pkg_path), expected)
})

test_that("turn_path_into_box_import returns an unevaluated box import expression", {
  box::use(artma / libs / path[turn_path_into_box_import])

  pkg_path <- file.path("inst", "artma", "libs", "path.R")
  expr <- turn_path_into_box_import(pkg_path)

  expect_true(is.expression(expr))
  expect_length(expr, 1L)
  expect_identical(
    expr,
    parse(text = "box::use(path=artma/libs/path)")
  )
})

test_that("turn_path_into_box_import errors when it cannot determine an import path", {
  box::use(artma / libs / path[turn_path_into_box_import])

  tmp_file <- withr::local_tempfile(fileext = ".R")
  writeLines("cat('demo')\n", tmp_file)

  expect_error(
    turn_path_into_box_import(tmp_file),
    class = "rlang_error",
    regexp = "Failed to determine a path"
  )
})
