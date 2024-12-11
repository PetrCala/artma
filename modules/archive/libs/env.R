# Source as few modules as possible to avoid initial setup errors
source("base/PACKAGES.R") # can not use 'box' here, as it is not loaded yet

#' Set the CRAN mirror
set_mirror <- function(mirror = NULL) {
  if (is.null(mirror)) {
    # Set the CRAN mirror to the first available mirror
    available_mirrors <- getCRANmirrors()
    mirror <- available_mirrors$URL[1]
  }
  options(repos = mirror)
}

#' Quietly execute an expression
#'
#' This function suppresses package startup messages, warnings, and messages
#' while executing an expression. It is useful for keeping the console output
#' clean when loading or installing packages.
#'
#' @param expr [expression] The expression to be executed quietly.
#'
#' @return The result of the executed expression without any messages, warnings, or package startup messages.
quiet_packages <- function(expr) {
  suppressPackageStartupMessages(suppressWarnings(suppressMessages(expr)))
}

#' Function to install a package if it is not a part of the library yet
install_and_check <- function(pkg, version = NA, verbose = TRUE) {
  if (verbose) {
    version_info <- ifelse(is.na(version), "", paste0(" (", version, ")"))
    message <- paste0("Processing package: ", pkg, version_info)
    cat(sprintf("%-100s", message)) # Add enough whitespace to make sure the whole line is cleared
    flush.console()
  }
  if (!pkg %in% rownames(installed.packages()) || (!is.na(version) && packageVersion(pkg) != version)) {
    tryCatch(
      {
        # Install specific version if provided, else install the latest version
        if (!is.na(version)) {
          remotes::install_version(pkg, version = version)
        } else {
          install.packages(pkg)
        }
      },
      error = function(e) {
        stop("\nPackage installation failed for ", pkg, ": ", e$message)
      }
    )
  }

  # Load the package - this is disabled, as no packages need to be sourced
  # if (!pkg %in% PACKAGES$NON_ATTACHED) {
  #     suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  # }

  # Reset the cursor to the start of the line for the progress bar
  cat("\r")
}


#' Load and install a list of R packages
#'
#' This function checks if the specified packages are installed, installs any missing packages, and then loads all of them. If an error occurs during the installation or loading process, the function stops execution and displays an error message.
#'
#' Include a progress bar to track the loading process.
#'
#' @param package_list [character] A character vector of package names.
#' @param msg [character] An optional message to display before loading the packages.
#' @param native [logical] A flag indicating whether to install the packages in an R-native way. This approach is less transparent, but requires no external packages.
#' @return A message indicating that all packages were loaded successfully or an error message if the process fails.
load_packages <- function(package_list, msg = NULL, native = FALSE) {
  verbose <- !is.null(msg)

  # Convert package_list to a named list with NULL versions if necessary
  if (!is.list(package_list) || is.null(names(package_list))) {
    package_list <- setNames(as.list(rep(NA, length(package_list))), package_list)
  }

  # Loading packages
  if (verbose) {
    cat(paste0(msg, "\n"))
  }

  if (native) {
    # Native installation that does not require any external packages
    install_initial_pkg <- function(pkg) {
      if (!pkg %in% rownames(installed.packages())) install.packages(pkg)
    }
    sapply(names(package_list), install_initial_pkg)
  } else {
    # Applying the function to each package with a progress bar
    pbapply::pblapply(names(package_list), function(pkg) install_and_check(pkg, package_list[[pkg]], verbose))
  }

  if (verbose) {
    cat("\rAll packages loaded successfully\n") # Clear the progress bar
  }
}

#' Setup the R environment
#' @export
setup_env <- function() {
  tryCatch(
    {
      set_mirror()
    },
    error = function(e) {
      message("Error setting the CRAN mirror:")
      message(e)
    }
  )

  tryCatch(
    {
      load_packages(PACKAGES$INITIAL, msg = "Loading initial packages...", native = TRUE)
      load_packages(PACKAGES$CORE, msg = "Loading core packages...")
      load_packages(PACKAGES$DEV, msg = "Loading development packages...")
    },
    error = function(e) {
      message("Error loading the packages:")
      message(e)
    }
  )
}
