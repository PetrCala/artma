#' Check if the package is being loaded via devtools::load_all()
#'
#' @return TRUE if loaded via devtools, FALSE otherwise
#' @keywords internal
is_devtools_load <- function() { # nolint: unused_declared_object_linter.
  identical(Sys.getenv("DEVTOOLS_LOAD"), "true")
}

#' @title Get valid box path
#' @description
#' Construct a box path that will allow box imports for the current package. This is done by adding the package path to the box path option if it is not already there.
#' @param libname The path to the library.
#' @param pkgname The name of the package.
#' @keywords internal
get_valid_boxpath <- function(libname, pkgname) {
  current_box_path <- getOption("box.path", character(0))

  pkg_path <- file.path(libname, pkgname)
  dev_path <- file.path(pkg_path, "inst")

  boxpath_defined <- function(path) {
    if (length(current_box_path) == 0) {
      return(FALSE)
    }
    grepl(path, current_box_path)
  }

  if (all(c(
    boxpath_defined(pkg_path),
    boxpath_defined(dev_path)
  ))) {
    return(current_box_path)
  } # Already valid

  unique(c(current_box_path, pkg_path, dev_path))
}

#' @title .onLoad hook for package initialization
#' @description Called when the package is loaded.
#' @param libname The path to the library.
#' @param pkgname The name of the package.
#' @return `NULL` Sets up the package on load
#' @keywords internal
.onLoad <- function(libname, pkgname) { # nolint: unused_declared_object_linter.
  op <- options()
  op.artma <- list(
    # artma.abc = xyz
  )

  # Optional set
  toset <- !(names(op.artma) %in% names(op))
  if (any(toset)) options(op.artma[toset])

  # Mandatory set
  options(box.path = get_valid_boxpath(libname, pkgname))

  # Box package support
  if (requireNamespace("box", quietly = TRUE)) {
    box_ns <- getNamespace("box")
    fn_name <- "mod_export_names.box$pkg_info"
    if (exists(fn_name, envir = box_ns, inherits = FALSE)) {
      patched <- function(info, mod_ns) {
        lazydata <- tryCatch(getNamespaceInfo(mod_ns, "lazydata"), error = function(e) NULL)
        lazy_names <- if (is.environment(lazydata)) {
          suppressWarnings(base::ls(envir = lazydata))
        } else {
          character()
        }
        c(getNamespaceExports(mod_ns), lazy_names)
      }
      was_locked <- bindingIsLocked(fn_name, box_ns)
      if (was_locked) {
        unlockBinding(fn_name, box_ns)
      }
      assign(fn_name, patched, envir = box_ns)
      if (was_locked) {
        lockBinding(fn_name, box_ns)
      }
    }
  }

  invisible()
}

#' @title .onUnload hook for package detachment
#' @description Called when the package is detached.
#' @note The box imports no longer work after the package is detached.
#' @return `NULL` Cleans up the package on unload
#' @keywords internal
.onUnload <- function(libpath) { # nolint: unused_declared_object_linter.
  # Remove options with the "artma" prefix
  opts_to_remove <- names(options())[startsWith(names(options()), "artma")]
  options(stats::setNames(rep(list(NULL), length(opts_to_remove)), opts_to_remove))

  invisible()
}
