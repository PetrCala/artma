devtools::load_all()

box::use(artma / paths[PATHS])

cache_dir <- PATHS$DIR_USR_CACHE

cli::cli_alert_info("Clearing cache in {.file {cache_dir}}...")

if (base::dir.exists(cache_dir)) {
  base::unlink(cache_dir, recursive = TRUE, force = TRUE)
}

cli::cli_alert_success("Cache cleared!")
