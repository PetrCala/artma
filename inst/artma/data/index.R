box::use(artma / libs / cache[cache_cli])

build_prepare_data_cache_key <- function() {
  box::use(
    artma / data_config / read[get_data_config],
    artma / options / utils[get_option_group]
  )

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

  config_hash <- digest::digest(get_data_config(), algo = "xxhash64")
  artma_options_hash <- digest::digest(get_option_group("artma"), algo = "xxhash64")

  list(
    source_path = normalized_path,
    source_mtime = source_mtime,
    config_hash = config_hash,
    artma_options_hash = artma_options_hash,
    package_version = as.character(utils::packageVersion("artma"))
  )
}

prepare_data_impl <- function(cache_signature = NULL) {
  force(cache_signature)

  box::use(artma / libs / utils[get_verbosity])

  if (get_verbosity() >= 4) {
    cli::cli_inform("Preparing data for analysis.")
  }

  box::use(
    artma / data / read[read_data],
    artma / data / preprocess[preprocess_data],
    artma / data / compute[compute_optional_columns]
  )

  df <- read_data()
  df <- preprocess_data(df)
  df <- compute_optional_columns(df)

  df
}

prepare_data_cached <- cache_cli(
  prepare_data_impl,
  extra_keys = list(stage = "prepare_data")
)

#' @title Prepare data
#' @description Prepare data for analysis. This includes reading, preprocessing, cleaning, and validating the data.
#' @return *[data.frame]* The prepared data frame.
prepare_data <- function() {
  cache_signature <- build_prepare_data_cache_key()
  prepare_data_cached(cache_signature = cache_signature)
}

box::export(
  prepare_data
)
