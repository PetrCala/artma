box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_null,
    expect_true,
    test_that
  ]
)

# A legacy options file exactly as the pre-unification package wrote it: the
# name mapping under data.colnames and per-variable config under data.config.
write_legacy_options_file <- function(dir, file_name = "legacy.yaml") {
  legacy <- list(
    data = list(
      source_path = "data.csv",
      colnames = list(
        study_id = "study_name",
        effect = "effect_size",
        se = "se",
        n_obs = "sample_size",
        t_stat = NA
      ),
      config = list(
        effect = list(bma = TRUE),
        gdp_growth = list(
          var_name_verbose = "GDP Growth",
          bma = TRUE,
          effect_sum_stats = TRUE
        )
      ),
      na_handling = "remove"
    ),
    verbose = 3L
  )
  yaml::write_yaml(legacy, file.path(dir, file_name))
  file_name
}

test_that("is_legacy_options_format detects only the legacy dual-store format", {
  box::use(artma / options / migrate[is_legacy_options_format])

  expect_true(is_legacy_options_format(list(data = list(colnames = list(effect = "b")))))
  expect_true(is_legacy_options_format(list(data = list(config = list(x = list(bma = TRUE))))))
  expect_false(is_legacy_options_format(list(data = list(columns = list(effect = list())))))
  expect_false(is_legacy_options_format(list(data = list(source_path = "x.csv"))))
  expect_false(is_legacy_options_format(list()))
})

test_that("build_unified_columns merges both legacy stores into one record per column", {
  box::use(artma / options / migrate[build_unified_columns])

  records <- build_unified_columns(
    colnames_map = list(
      study_id = "study_name",
      effect = "effect_size",
      se = "se", # identity mapping: dropped
      t_stat = NA # unmapped optional: dropped
    ),
    config = list(
      effect = list(bma = TRUE),
      gdp_growth = list(bma = TRUE)
    )
  )

  # The name mapping and analysis config land on the same record
  expect_equal(records$effect$source_name, "effect_size")
  expect_true(records$effect$bma)

  # Pure name mappings become role records
  expect_equal(records$study_id$source_name, "study_name")

  # Moderator config entries carry over unchanged
  expect_true(records$gdp_growth$bma)

  # Identity and NA mappings do not produce records
  expect_null(records$se)
  expect_null(records$t_stat)
})

test_that("a legacy options file migrates automatically with clear messaging", {
  box::use(artma / options / migrate[migrate_legacy_options])

  tmp_dir <- withr::local_tempdir()
  file_name <- write_legacy_options_file(tmp_dir)

  withr::local_options(list("artma.verbose" = 3L))
  logs <- testthat::capture_messages(
    migrated <- migrate_legacy_options(
      options_file_name = file_name,
      options_dir = tmp_dir
    )
  )

  expect_true(migrated)
  expect_true(any(grepl("legacy column format", logs)))
  expect_true(any(grepl("Migrated", logs)))

  # The rewritten file holds one unified store and no legacy keys
  converted <- yaml::read_yaml(file.path(tmp_dir, file_name))
  expect_null(converted$data$colnames)
  expect_null(converted$data$config)
  expect_equal(converted$data$columns$effect$source_name, "effect_size")
  expect_true(converted$data$columns$effect$bma)
  expect_equal(converted$data$columns$study_id$source_name, "study_name")
  expect_equal(converted$data$columns$n_obs$source_name, "sample_size")
  expect_true(converted$data$columns$gdp_growth$bma)

  # Untouched settings survive the migration
  expect_equal(converted$data$na_handling, "remove")
  expect_equal(converted$verbose, 3L)
})

test_that("migration is a one-shot: a second pass is a no-op", {
  box::use(artma / options / migrate[migrate_legacy_options])

  tmp_dir <- withr::local_tempdir()
  file_name <- write_legacy_options_file(tmp_dir)

  withr::local_options(list("artma.verbose" = 1L))
  expect_true(migrate_legacy_options(file_name, options_dir = tmp_dir))

  path <- file.path(tmp_dir, file_name)
  first_pass <- readLines(path)

  expect_false(migrate_legacy_options(file_name, options_dir = tmp_dir))
  expect_identical(readLines(path), first_pass)
})

test_that("a migrated legacy file loads and round-trips through options.load", {
  box::use(
    artma[options.load],
    artma / options / migrate[migrate_legacy_options],
    testing / fixtures / index[FIXTURES]
  )

  FIXTURES$local_cli_silence()

  # Pin the template to this checkout so the test does not depend on the
  # template shipped with whichever artma build happens to be installed
  template_path <- file.path(
    testthat::test_path("..", ".."),
    "inst", "artma", "options", "templates", "options_template.yaml"
  )
  testthat::skip_if_not(file.exists(template_path), "package sources are not available")

  tmp_dir <- withr::local_tempdir()
  file_name <- write_legacy_options_file(tmp_dir)

  withr::local_options(list("artma.verbose" = 1L))
  migrate_legacy_options(file_name, options_dir = tmp_dir)

  loaded <- options.load(
    options_file_name = file_name,
    options_dir = tmp_dir,
    template_path = template_path,
    should_validate = TRUE,
    should_return = TRUE
  )

  store <- loaded$`artma.data.columns`
  expect_equal(store$effect$source_name, "effect_size")
  expect_true(store$effect$bma)
  expect_equal(store$study_id$source_name, "study_name")

  # Round-trip: the mapping read from the loaded store drives standardization
  withr::with_options(loaded, {
    box::use(artma / data / utils[get_colnames_map])
    map <- get_colnames_map()
    expect_equal(map$effect, "effect_size")
    expect_equal(map$study_id, "study_name")
    expect_equal(map$n_obs, "sample_size")
    expect_null(map$se)
  })
})

test_that("migration does not touch files already in the unified format", {
  box::use(artma / options / migrate[migrate_legacy_options])

  tmp_dir <- withr::local_tempdir()
  current <- list(
    data = list(
      source_path = "data.csv",
      columns = list(effect = list(source_name = "effect_size"))
    )
  )
  yaml::write_yaml(current, file.path(tmp_dir, "current.yaml"))
  before <- readLines(file.path(tmp_dir, "current.yaml"))

  withr::local_options(list("artma.verbose" = 3L))
  expect_false(migrate_legacy_options("current.yaml", options_dir = tmp_dir))
  expect_identical(readLines(file.path(tmp_dir, "current.yaml")), before)
})
