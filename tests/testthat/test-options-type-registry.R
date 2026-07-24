box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_identical,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(
  artma / options / template[coerce_option_value],
  artma / options / utils[validate_option_value],
  artma / options / type_registry[get_option_type_spec, resolve_option_type_key]
)

# Build a minimal option definition for the coerce path.
opt_def <- function(type, allow_na = FALSE, name = "test.option", ...) {
  list(name = name, type = type, allow_na = allow_na, ...)
}

test_that("resolve_option_type_key collapses enum and passes others through", {
  expect_equal(resolve_option_type_key("enum: a|b|c"), "enum")
  expect_equal(resolve_option_type_key("character"), "character")
  expect_equal(resolve_option_type_key("mystery"), "mystery")
})

test_that("get_option_type_spec returns a spec for known types and pass-through otherwise", {
  for (type in c("character", "integer", "logical", "numeric", "list", "enum: a|b")) {
    spec <- get_option_type_spec(type)
    expect_true(is.function(spec$coerce))
    expect_true(is.function(spec$validate))
    expect_true(isTRUE(spec$allows_na))
  }
  passthrough <- get_option_type_spec("unknown_type")
  expect_equal(passthrough$coerce(42, opt_def("unknown_type")), 42)
  expect_null(passthrough$validate(42, "test.option", "unknown_type"))
})

# ---- character ----

test_that("character type: valid / invalid / NA / NULL through both paths", {
  # valid
  expect_equal(coerce_option_value("hello", opt_def("character")), "hello")
  expect_null(validate_option_value("hello", "character", "test.option"))
  # coerce is permissive (casts), validate is strict about the incoming type
  expect_equal(coerce_option_value(5, opt_def("character")), "5")
  expect_false(is.null(validate_option_value(5, "character", "test.option")))
  # NA
  expect_error(coerce_option_value(NA, opt_def("character")), "does not allow NA")
  expect_equal(coerce_option_value(NA, opt_def("character", allow_na = TRUE)), NA)
  expect_false(is.null(validate_option_value(NA, "character", "test.option")))
  expect_null(validate_option_value(NA, "character", "test.option", allow_na = TRUE))
  # NULL
  expect_null(coerce_option_value(NULL, opt_def("character")))
  expect_false(is.null(validate_option_value(NULL, "character", "test.option")))
  expect_null(validate_option_value(NULL, "character", "test.option", allow_na = TRUE))
})

# ---- integer ----

test_that("integer type: valid / invalid / NA / NULL through both paths", {
  expect_identical(coerce_option_value(3, opt_def("integer")), 3L)
  expect_null(validate_option_value(3L, "integer", "test.option"))
  expect_null(validate_option_value(3, "integer", "test.option"))
  # fractional is invalid on both paths
  expect_error(coerce_option_value(3.7, opt_def("integer")), "integer")
  expect_false(is.null(validate_option_value(3.7, "integer", "test.option")))
  # non-numeric is invalid on both paths
  expect_error(coerce_option_value("x", opt_def("integer")), "integer")
  expect_false(is.null(validate_option_value("x", "integer", "test.option")))
  # NA
  expect_error(coerce_option_value(NA, opt_def("integer")), "does not allow NA")
  expect_equal(coerce_option_value(NA, opt_def("integer", allow_na = TRUE)), NA)
  expect_false(is.null(validate_option_value(NA, "integer", "test.option")))
  expect_null(validate_option_value(NA, "integer", "test.option", allow_na = TRUE))
  # NULL
  expect_null(coerce_option_value(NULL, opt_def("integer")))
  expect_false(is.null(validate_option_value(NULL, "integer", "test.option")))
})

# ---- logical ----

test_that("logical type: valid / invalid / NA / NULL through both paths", {
  expect_true(coerce_option_value(TRUE, opt_def("logical")))
  expect_null(validate_option_value(TRUE, "logical", "test.option"))
  # coerce casts a string via as.logical; validate demands an actual logical
  expect_true(coerce_option_value("TRUE", opt_def("logical")))
  expect_false(is.null(validate_option_value("TRUE", "logical", "test.option")))
  # a string that as.logical cannot parse fails the coerce path (manufactured NA)
  expect_error(coerce_option_value("notlogical", opt_def("logical")), "logical")
  expect_false(is.null(validate_option_value(5, "logical", "test.option")))
  # NA
  expect_error(coerce_option_value(NA, opt_def("logical")), "does not allow NA")
  expect_equal(coerce_option_value(NA, opt_def("logical", allow_na = TRUE)), NA)
  expect_false(is.null(validate_option_value(NA, "logical", "test.option")))
  expect_null(validate_option_value(NA, "logical", "test.option", allow_na = TRUE))
  # NULL
  expect_null(coerce_option_value(NULL, opt_def("logical")))
  expect_false(is.null(validate_option_value(NULL, "logical", "test.option")))
})

# ---- numeric ----

test_that("numeric type: valid / invalid / NA / NULL through both paths", {
  expect_equal(coerce_option_value("3.5", opt_def("numeric")), 3.5)
  expect_equal(coerce_option_value(3.5, opt_def("numeric")), 3.5)
  expect_null(validate_option_value(3.5, "numeric", "test.option"))
  # coerce parses a numeric string; validate rejects the string type
  expect_false(is.null(validate_option_value("3.5", "numeric", "test.option")))
  # non-numeric string fails coerce (manufactured NA)
  expect_error(coerce_option_value("abc", opt_def("numeric")), "numeric")
  expect_false(is.null(validate_option_value("abc", "numeric", "test.option")))
  # NA
  expect_error(coerce_option_value(NA, opt_def("numeric")), "does not allow NA")
  expect_equal(coerce_option_value(NA, opt_def("numeric", allow_na = TRUE)), NA)
  expect_false(is.null(validate_option_value(NA, "numeric", "test.option")))
  expect_null(validate_option_value(NA, "numeric", "test.option", allow_na = TRUE))
  # NULL
  expect_null(coerce_option_value(NULL, opt_def("numeric")))
  expect_false(is.null(validate_option_value(NULL, "numeric", "test.option")))
})

# ---- list ----

test_that("list type: coerce casts, validate always passes non-NA values", {
  expect_equal(coerce_option_value(c(1, 2), opt_def("list")), list(1, 2))
  # validate has no list arm and treats any non-NA value as valid
  expect_null(validate_option_value(list(1, 2), "list", "test.option"))
  expect_null(validate_option_value(5, "list", "test.option"))
  # NA
  expect_error(coerce_option_value(NA, opt_def("list")), "does not allow NA")
  expect_equal(coerce_option_value(NA, opt_def("list", allow_na = TRUE)), NA)
  expect_false(is.null(validate_option_value(NA, "list", "test.option")))
  # NULL
  expect_null(coerce_option_value(NULL, opt_def("list")))
  expect_false(is.null(validate_option_value(NULL, "list", "test.option")))
})

# ---- enum ----

test_that("enum type: valid / invalid / NA / NULL through both paths", {
  enum_type <- "enum: ask|auto|strict"
  expect_equal(coerce_option_value("auto", opt_def(enum_type)), "auto")
  expect_null(validate_option_value("auto", enum_type, "test.option"))
  # invalid membership fails both paths with the "must be one of" message
  expect_error(coerce_option_value("nonsense", opt_def(enum_type)), "must be one of")
  expect_false(is.null(validate_option_value("nonsense", enum_type, "test.option")))
  # NA
  expect_error(coerce_option_value(NA, opt_def(enum_type)), "does not allow NA")
  expect_false(is.null(validate_option_value(NA, enum_type, "test.option")))
  expect_null(validate_option_value(NA, enum_type, "test.option", allow_na = TRUE))
  # NULL
  expect_null(coerce_option_value(NULL, opt_def(enum_type)))
  expect_false(is.null(validate_option_value(NULL, enum_type, "test.option")))
})

# ---- unknown type (pass-through) ----

test_that("unknown type passes through coerce and validate", {
  expect_equal(coerce_option_value("anything", opt_def("date")), "anything")
  expect_null(validate_option_value("anything", "date", "test.option"))
  expect_error(coerce_option_value(NA, opt_def("date")), "does not allow NA")
  expect_null(coerce_option_value(NULL, opt_def("date")))
})

# ---- vector NA handling (guards the #321 / #323 regression) ----

test_that("vector-valued options honor allow_na without the length-1 coercion error", {
  # A vector containing NA is rejected when NA is not allowed.
  expect_error(
    coerce_option_value(c(1, NA), opt_def("numeric")),
    "does not allow NA"
  )
  # When NA is allowed the vector passes through intact (no length-3 coercion crash).
  expect_equal(
    coerce_option_value(c(1, NA), opt_def("numeric", allow_na = TRUE)),
    c(1, NA)
  )
  expect_equal(
    coerce_option_value(c(1L, 2L, 3L), opt_def("integer")),
    c(1L, 2L, 3L)
  )
})
