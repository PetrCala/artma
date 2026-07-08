box::use(
  testthat[
    test_that,
    expect_identical,
    expect_null
  ]
)

# .find_package_root is internal to the module, so reach it through the box namespace
.get_find_package_root <- function() {
  box::use(artma / paths)
  attr(paths, "namespace")$.find_package_root
}

test_that(".find_package_root finds the root even when the directory is not named after the package", {
  find_package_root <- .get_find_package_root()

  root <- withr::local_tempdir(pattern = "artma-worktree-")
  writeLines("Package: artma", file.path(root, "DESCRIPTION"))
  nested <- file.path(root, "tests", "testthat")
  dir.create(nested, recursive = TRUE)

  expect_identical(
    find_package_root("artma", start = nested),
    tools::file_path_as_absolute(root)
  )
})

test_that(".find_package_root returns NULL when no matching DESCRIPTION exists", {
  find_package_root <- .get_find_package_root()

  root <- withr::local_tempdir(pattern = "artma-other-")
  writeLines("Package: notartma", file.path(root, "DESCRIPTION"))

  expect_null(find_package_root("artma", start = root))
})

test_that("get_pkg_path falls back to the working directory before find.package", {
  box::use(artma / paths[get_pkg_path])

  root <- withr::local_tempdir(pattern = "artma-clone-")
  writeLines("Package: artma", file.path(root, "DESCRIPTION"))
  dir.create(file.path(root, "inst", "artma"), recursive = TRUE)

  withr::local_options(list(box.path = NULL))
  withr::local_dir(root)

  expect_identical(
    get_pkg_path(),
    file.path(tools::file_path_as_absolute(root), "inst")
  )
})
