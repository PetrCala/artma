box::use(
  artma / options / utils[get_option_group]
)

#' @title Build cache signature for data-dependent workflows
#' @description
#' Construct a list that captures the most relevant inputs influencing
#' downstream data computations. The signature combines the configured data
#' source path (and its modification time when available), hashes of the
#' current data configuration and project options, and the installed package
#' version. It is suitable for reuse across modules that rely on the prepared
#' dataset to ensure cached artefacts are refreshed when any of these inputs
#' change.
#' @return *\[list\]* A deterministic signature list suitable for
#'   forwarding to `cache_cli()` wrappers as a `cache_signature` argument.
build_data_cache_signature <- function() {
  source_path <- getOption("artma.data.source_path")
  normalized_path <- NULL
  source_mtime <- NA_real_

  if (!is.null(source_path)) {
    normalized_path <- tryCatch(
      normalizePath(source_path, mustWork = FALSE),
      error = function(err) source_path
    )

    file_info <- tryCatch(file.info(normalized_path), error = function(err) NULL)
    if (!is.null(file_info) && nrow(file_info) == 1) {
      mtime <- file_info$mtime
      if (!is.na(mtime)) {
        source_mtime <- unclass(mtime)
      }
    }
  }

  box::use(artma / libs / infrastructure / polyfills[digest])

  overrides <- getOption("artma.data.config")
  if (!is.list(overrides)) {
    overrides <- list()
  }

  config_hash <- digest(overrides, algo = "xxhash64")
  artma_options_hash <- digest(get_option_group("artma"), algo = "xxhash64")

  list(
    source_path = normalized_path,
    source_mtime = source_mtime,
    config_hash = config_hash,
    artma_options_hash = artma_options_hash,
    package_version = as.character(utils::packageVersion("artma"))
  )
}

box::export(build_data_cache_signature)
