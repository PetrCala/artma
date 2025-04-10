artma_box_path <- function() {
  pkg_box_path <- find.package("artma")
  dev_box_path <- file.path(pkg_box_path, "inst") # For local development

  unique(c(pkg_box_path, dev_box_path))
}

.onLoad <- function(libname, pkgname) {
  op <- options()

  current_box_path <- getOption("box.path", character(0))


  if (!any(grepl("artma$", current_box_path))) { # should end with 'artma'
    op.artma <- list(box.path = artma_box_path())
  } else {
    op.artma <- list()
  }

  # op.artma <- list(
  #   box.path = artma_box_path
  #   # artma.abc = xyz
  # )
  toset <- !(names(op.artma) %in% names(op))
  if (any(toset)) options(op.artma[toset])

  invisible()
}

# .onUnload() <- function(libname, pkgname) {
#   box::use(
#     artma / const[CONST],
#     artma / options / utils[remove_options_with_prefix]
#   )

#   remove_options_with_prefix(prefix = CONST$PACKAGE_NAME)
# }
