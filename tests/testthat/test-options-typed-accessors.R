box::use(
  testthat[expect_equal, expect_true, expect_null, test_that],
  artma / options / typed_accessors[
    option_template_default,
    get_precision_type,
    get_winsorization_level,
    get_na_handling,
    get_se_zero_handling
  ]
)

# option_template_default reads the real packaged template, so these pin the
# accessors to whatever the template declares. If a template default changes,
# the runtime fallback follows automatically instead of drifting.

test_that("option_template_default sources concrete defaults from the template", {
  expect_equal(option_template_default("artma.calc.precision_type"), "1/SE")
  expect_equal(option_template_default("artma.data.winsorization_level"), 0.01)
})

test_that("option_template_default returns NA for allow_na sentinel defaults", {
  expect_true(is.na(option_template_default("artma.data.na_handling")))
  expect_true(is.na(option_template_default("artma.calc.se_zero_handling")))
})

test_that("option_template_default coerces via the type registry", {
  # numeric option arrives coerced to double, not left as a raw YAML scalar.
  expect_true(is.numeric(option_template_default("artma.data.winsorization_level")))
})

test_that("get_precision_type returns the template default when unset", {
  withr::local_options(list("artma.calc.precision_type" = NULL))
  expect_equal(get_precision_type(), "1/SE")
})

test_that("get_precision_type returns the set value when present", {
  withr::local_options(list("artma.calc.precision_type" = "DoF"))
  expect_equal(get_precision_type(), "DoF")
})

test_that("get_winsorization_level returns the disabled default when unset", {
  withr::local_options(list("artma.data.winsorization_level" = NULL))
  expect_equal(get_winsorization_level(), 0)
})

test_that("get_winsorization_level returns the set value when present", {
  withr::local_options(list("artma.data.winsorization_level" = 0.05))
  expect_equal(get_winsorization_level(), 0.05)
})

test_that("get_na_handling returns the documented default when unset", {
  withr::local_options(list("artma.data.na_handling" = NULL))
  expect_equal(get_na_handling(), "stop")
})

test_that("get_na_handling treats the NA sentinel as unset", {
  withr::local_options(list("artma.data.na_handling" = NA))
  expect_equal(get_na_handling(), "stop")
})

test_that("get_na_handling returns the set value when present", {
  withr::local_options(list("artma.data.na_handling" = "remove"))
  expect_equal(get_na_handling(), "remove")
})

test_that("get_se_zero_handling returns the documented default when unset", {
  withr::local_options(list("artma.calc.se_zero_handling" = NULL))
  expect_equal(get_se_zero_handling(), "remove")
})

test_that("get_se_zero_handling treats the NA sentinel as unset", {
  withr::local_options(list("artma.calc.se_zero_handling" = NA))
  expect_equal(get_se_zero_handling(), "remove")
})

test_that("get_se_zero_handling returns the set value when present", {
  withr::local_options(list("artma.calc.se_zero_handling" = "ignore"))
  expect_equal(get_se_zero_handling(), "ignore")
})

test_that("option_template_default aborts for an unknown option", {
  testthat::expect_error(
    option_template_default("artma.does.not.exist"),
    "No template definition"
  )
})
