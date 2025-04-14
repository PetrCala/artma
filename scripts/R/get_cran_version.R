#!/usr/bin/env Rscript

# Returns the version of the package on CRAN

cran_packages <- available.packages()

package_name <- "artma"
if (!package_name %in% rownames(cran_packages)) {
  stop(sprintf("Package %s not found on CRAN", package_name))
}

cran_version <- cran_packages[package_name, "Version"]
cat(cran_version)
