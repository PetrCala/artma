box::use(testthat[test_that, expect_equal, expect_true, expect_false, expect_setequal, expect_length, expect_no_error])

test_that("options lifecycle helpers operate on user files", {
  box::use(artma[options.create, options.copy, options.delete, options.fix, options.list, options.load, options.remove, options.validate])

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

  withr::local_options()
  loaded <- options.load(
    options_file_name = "valid.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    should_validate = TRUE,
    should_set_to_namespace = TRUE,
    should_add_temp_options = TRUE,
    should_return = TRUE
  )
  expect_equal(loaded$`artma.general.name`, "Primary")
  expect_equal(getOption("artma.general.name"), "Primary")
  expect_equal(getOption("artma.temp.file_name"), "valid.yaml")

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

  # Test that options.remove is an alias for options.delete
  options.create(
    options_file_name = "to_remove.yaml",
    options_dir = tmp_dir,
    template_path = template_path,
    user_input = list(),
    should_validate = TRUE,
    should_overwrite = TRUE
  )
  expect_true(file.exists(file.path(tmp_dir, "to_remove.yaml")))
  options.remove(
    options_file_name = "to_remove.yaml",
    options_dir = tmp_dir,
    skip_confirmation = TRUE
  )
  expect_false(file.exists(file.path(tmp_dir, "to_remove.yaml")))
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
      should_set_to_namespace = FALSE,
      should_return = TRUE
    )
  })

  expect_equal(loaded$`artma.methods.enabled`, TRUE)
  expect_equal(loaded$`artma.calc.precision_type`, "1/SE")
  expect_equal(loaded$`artma.calc.se_zero_handling`, "stop")
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
    should_set_to_namespace = FALSE,
    should_return = TRUE
  )

  expect_equal(loaded$data.na_handling, "stop")
  expect_equal(loaded$data.winsorization_level, 0)
})
