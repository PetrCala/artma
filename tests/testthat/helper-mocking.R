# Test helpers for simulating absent optional packages.
#
# Several estimators guard their calls to Suggests packages with
# `requireNamespace(pkg)` and take a skip / abort path when the package is
# missing. Those packages are installed in CI, so the only way to exercise the
# skip path is to make `requireNamespace` report them as absent. The guards live
# inside `box` module namespaces (locked, and resolving `requireNamespace` from
# `base`), so the mock is installed on the `base` binding itself and restored
# when the calling frame exits.

#' Pretend a set of packages is not installed for the duration of the caller.
#'
#' @param pkgs *[character]* Package names that `requireNamespace` should report
#'   as absent. Every other package resolves normally.
#' @param .local_envir *[environment]* Frame whose exit restores the original
#'   binding. Defaults to the calling frame.
#' @return Invisibly `NULL`. Registers a `withr::defer` cleanup.
local_pretend_packages_absent <- function(pkgs, .local_envir = parent.frame()) {
  original <- base::requireNamespace
  unlockBinding("requireNamespace", baseenv())
  assign(
    "requireNamespace", # nolint: object_name_linter.
    function(package, ...) {
      if (package %in% pkgs) {
        return(FALSE)
      }
      original(package, ...)
    },
    envir = baseenv()
  )
  withr::defer(
    {
      assign("requireNamespace", original, envir = baseenv()) # nolint: object_name_linter.
      lockBinding("requireNamespace", baseenv())
    },
    envir = .local_envir
  )
  invisible(NULL)
}
