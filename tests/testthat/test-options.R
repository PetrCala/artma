box::use(testthat[test_that, expect_equal, expect_true, expect_false, expect_null, expect_setequal, expect_length, expect_no_error])

test_that("options lifecycle helpers operate on user files", {
  box::use(artma[options.create, options.copy, options.delete, options.fix, options.list, options.load, options.validate])

  tmp_dir <- withr::local_tempdir()
  template_path <- file.path(tmp_dir, "template.yaml")

  template <- list(
    general = list(
      name = list(
        type = "character",
        default = "Default Config",
        help = "Friendly name for the configuration"
      )
    ),
    data = list(
      threshold = list(
        type = "integer",
        default = 10L,
        help = "Numeric threshold"
      ),
      enabled = list(
        type = "logical",
        default = TRUE,
        help = "Whether the feature is enabled"
      )
    )
  )

  yaml::write_yaml(template, template_path)

  valid_options <- list(
    general = list(name = "Primary"),
    data = list(
      threshold = 4L,
      enabled = FALSE
    )
  )
  invalid_options <- valid_options
  invalid_options$data$threshold <- "four"

  yaml::write_yaml(valid_options, file.path(tmp_dir, "valid.yaml"))
  yaml::write_yaml(invalid_options, file.path(tmp_dir, "invalid.yaml"))

  no_errors <- options.validate(
    options_file_name = "valid.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    failure_action = "return_errors_quiet"
  )
  expect_length(no_errors, 0)

  with_errors <- options.validate(
    options_file_name = "invalid.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    failure_action = "return_errors_quiet"
  )
  expect_equal(with_errors[[1]]$type, "type_mismatch")

  options.copy(
    options_file_name_from = "valid.yaml",
    options_file_name_to = "copied.yaml",
    options_dir = tmp_dir,
    should_overwrite = TRUE
  )
  expect_true(file.exists(file.path(tmp_dir, "copied.yaml")))

  expect_setequal(
    options.list(options_dir = tmp_dir),
    c("copied.yaml", "invalid.yaml", "valid.yaml")
  )

  verbose_names <- options.list(
    options_dir = tmp_dir,
    should_return_verbose_names = TRUE
  )
  expect_true("Primary" %in% verbose_names)

  loaded <- options.load(
    options_file_name = "valid.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    should_validate = TRUE,
    should_add_temp_options = TRUE,
    should_return = TRUE
  )
  expect_equal(loaded$`artma.general.name`, "Primary")
  expect_equal(loaded$`artma.temp.file_name`, "valid.yaml")

  # The runtime path applies loaded options via withr, with automatic restoration
  withr::with_options(loaded, {
    expect_equal(getOption("artma.general.name"), "Primary")
    expect_equal(getOption("artma.temp.file_name"), "valid.yaml")
  })
  expect_null(getOption("artma.general.name"))

  options.create(
    options_file_name = "new.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    user_input = list(
      "general.name" = "Secondary",
      "data.threshold" = 8L
    ),
    should_validate = TRUE,
    should_overwrite = TRUE
  )
  created <- yaml::read_yaml(file.path(tmp_dir, "new.yaml"))
  expect_equal(created$general$name, "Secondary")
  expect_equal(created$data$threshold, 8L)
  expect_true(created$data$enabled)

  options.fix(
    options_file_name = "invalid.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    force_default_overwrites = TRUE
  )
  fixed <- yaml::read_yaml(file.path(tmp_dir, "invalid.yaml"))
  expect_equal(fixed$data$threshold, 10L)

  options.delete(
    options_file_name = "copied.yaml",
    options_dir = tmp_dir,
    skip_confirmation = TRUE
  )
  expect_false(file.exists(file.path(tmp_dir, "copied.yaml")))
})

test_that("options.load backfills missing and invalid values with template defaults", {
  box::use(artma[options.load])

  tmp_dir <- withr::local_tempdir()
  template_path <- file.path(tmp_dir, "template.yaml")

  template <- list(
    methods = list(
      enabled = list(
        type = "logical",
        default = TRUE,
        help = "Enable the method"
      )
    ),
    calc = list(
      precision_type = list(
        type = "character",
        default = "1/SE",
        help = "Precision metric"
      ),
      se_zero_handling = list(
        type = "character",
        default = "stop",
        help = "How to handle zero standard errors"
      )
    )
  )
  yaml::write_yaml(template, template_path)

  stale_options <- list(
    methods = list(
      enabled = "yes"
    ),
    calc = list(
      precision_type = 42L
    )
  )
  yaml::write_yaml(stale_options, file.path(tmp_dir, "stale.yaml"))

  loaded <- NULL
  expect_no_error({
    loaded <- options.load(
      options_file_name = "stale.yaml",
      options_dir = tmp_dir,
      template_path = template_path,
      should_validate = TRUE,
      should_return = TRUE
    )
  })

  expect_equal(loaded$`artma.methods.enabled`, TRUE)
  expect_equal(loaded$`artma.calc.precision_type`, "1/SE")
  expect_equal(loaded$`artma.calc.se_zero_handling`, "stop")
})

test_that("options.load never writes to the options file", {
  box::use(artma[options.load])

  tmp_dir <- withr::local_tempdir()
  template_path <- file.path(tmp_dir, "template.yaml")

  template <- list(
    data = list(
      threshold = list(type = "integer", default = 10L, help = "Threshold"),
      enabled = list(type = "logical", default = TRUE, help = "Enabled")
    )
  )
  yaml::write_yaml(template, template_path)

  opts_path <- file.path(tmp_dir, "stale.yaml")
  # Deliberately stale: missing 'enabled' and a wrong-typed 'threshold'.
  yaml::write_yaml(
    list(data = list(threshold = "not-an-int")),
    opts_path
  )

  before_content <- readLines(opts_path)
  before_mtime <- file.mtime(opts_path)

  options.load(
    options_file_name = "stale.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    should_validate = TRUE,
    should_return = TRUE
  )

  expect_identical(readLines(opts_path), before_content)
  expect_equal(file.mtime(opts_path), before_mtime)
})

test_that("options.load aborts when a required option without a default is missing", {
  box::use(artma[options.load])

  tmp_dir <- withr::local_tempdir()
  template_path <- file.path(tmp_dir, "template.yaml")

  template <- list(
    data = list(
      source_path = list(type = "character", help = "Required source path")
    )
  )
  yaml::write_yaml(template, template_path)
  yaml::write_yaml(
    list(data = list(unrelated = "value")),
    file.path(tmp_dir, "opts.yaml")
  )

  expect_error(
    options.load(
      options_file_name = "opts.yaml",
      options_dir = tmp_dir,
      template_path = template_path,
      should_validate = TRUE,
      should_return = TRUE
    ),
    "source_path"
  )
})

test_that("options.load parses the template only once per load", {
  box::use(artma[options.load])

  tmp_dir <- withr::local_tempdir()
  template_path <- file.path(tmp_dir, "template.yaml")

  template <- list(
    data = list(
      threshold = list(type = "integer", default = 10L, help = "Threshold")
    )
  )
  yaml::write_yaml(template, template_path)
  yaml::write_yaml(
    list(data = list(threshold = 3L)),
    file.path(tmp_dir, "opts.yaml")
  )

  real_read_yaml <- yaml::read_yaml
  n_template_reads <- 0L
  testthat::local_mocked_bindings(
    read_yaml = function(file, ...) {
      if (identical(file, template_path)) {
        n_template_reads <<- n_template_reads + 1L
      }
      real_read_yaml(file, ...)
    },
    .package = "yaml"
  )

  options.load(
    options_file_name = "opts.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    should_validate = TRUE,
    should_return = TRUE
  )

  expect_equal(n_template_reads, 1L)
})

test_that("options.load backfills defaults when loading without prefix", {
  box::use(artma[options.load])

  tmp_dir <- withr::local_tempdir()
  template_path <- file.path(tmp_dir, "template.yaml")

  template <- list(
    data = list(
      na_handling = list(
        type = "character",
        default = "stop",
        help = "Missing value handling"
      ),
      winsorization_level = list(
        type = "numeric",
        default = 0,
        help = "Winsorization quantile"
      )
    )
  )
  yaml::write_yaml(template, template_path)

  stale_options <- list(
    data = list(
      na_handling = FALSE
    )
  )
  yaml::write_yaml(stale_options, file.path(tmp_dir, "stale-no-prefix.yaml"))

  loaded <- options.load(
    options_file_name = "stale-no-prefix.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    load_with_prefix = FALSE,
    should_validate = TRUE,
    should_return = TRUE
  )

  expect_equal(loaded$data.na_handling, "stop")
  expect_equal(loaded$data.winsorization_level, 0)
})

test_that("options.list includes files with the .yml suffix", {
  box::use(artma[options.list])

  tmp_dir <- withr::local_tempdir()

  yaml::write_yaml(
    list(general = list(name = "Short Suffix")),
    file.path(tmp_dir, "short.yml")
  )
  yaml::write_yaml(
    list(general = list(name = "Long Suffix")),
    file.path(tmp_dir, "long.yaml")
  )

  expect_setequal(
    options.list(options_dir = tmp_dir),
    c("long.yaml", "short.yml")
  )
})

test_that("options files are stamped with the package version on write", {
  box::use(artma[options.create])

  tmp_dir <- withr::local_tempdir()
  template_path <- file.path(tmp_dir, "template.yaml")

  template <- list(
    data = list(
      threshold = list(
        type = "integer",
        default = 10L,
        help = "Numeric threshold"
      )
    )
  )
  yaml::write_yaml(template, template_path)

  options.create(
    options_file_name = "stamped.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    user_input = list(),
    should_validate = TRUE,
    should_overwrite = TRUE
  )

  written <- yaml::read_yaml(file.path(tmp_dir, "stamped.yaml"))
  expect_equal(
    written$general$artma_version,
    as.character(utils::packageVersion("artma"))
  )
})

test_that("autonomy level from an options file is honored via the withr runtime path", {
  box::use(artma[options.load])
  box::use(artma / libs / core / autonomy[get_autonomy_level])

  tmp_dir <- withr::local_tempdir()
  template_path <- file.path(tmp_dir, "template.yaml")

  template <- list(
    autonomy = list(
      level = list(
        type = "integer",
        default = 4L,
        help = "Autonomy level"
      )
    )
  )
  yaml::write_yaml(template, template_path)
  yaml::write_yaml(
    list(autonomy = list(level = 2L)),
    file.path(tmp_dir, "auto.yaml")
  )

  runtime_options <- options.load(
    options_file_name = "auto.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    should_validate = TRUE,
    should_return = TRUE
  )

  # Mirrors runtime_setup() in R/aaa.R, which applies loaded options via withr
  withr::with_options(runtime_options, {
    expect_equal(get_autonomy_level(), 2L)
  })
})

test_that("a legacy numeric autonomy.level in an options file migrates cleanly on load", {
  box::use(artma[options.load])
  box::use(artma / libs / core / autonomy[get_autonomy_level])

  tmp_dir <- withr::local_tempdir()
  template_path <- file.path(tmp_dir, "template.yaml")

  # Mirrors the post-collapse autonomy.level template node (a 3-value enum).
  template <- list(
    autonomy = list(
      level = list(
        type = "enum: 'ask_more'|'balanced'|'autonomous'",
        default = "autonomous",
        help = "Autonomy level"
      )
    )
  )
  yaml::write_yaml(template, template_path)

  # Simulate an options file written before the collapse, storing the old numeric level.
  yaml::write_yaml(
    list(autonomy = list(level = 4L)),
    file.path(tmp_dir, "legacy-autonomy.yaml")
  )

  loaded <- NULL
  expect_no_error({
    loaded <- options.load(
      options_file_name = "legacy-autonomy.yaml",
      options_dir = tmp_dir,
      template_path = template_path,
      should_validate = TRUE,
      should_return = TRUE
    )
  })

  # The stale numeric value fails enum validation and is backfilled with the new default.
  expect_equal(loaded$`artma.autonomy.level`, "autonomous")

  withr::with_options(loaded, {
    expect_equal(get_autonomy_level(), "autonomous")
  })
})
