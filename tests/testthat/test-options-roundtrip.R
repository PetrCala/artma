box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_length,
    expect_named,
    expect_null,
    expect_true,
    test_that
  ]
)

box::use(
  artma / options / template[flatten_user_options, is_descendable],
  artma / options / utils[flat_to_nested]
)

# The flatten/nest pair is the choke point every options rewrite passes
# through (options.create, options.fix, options.modify). A value shape it
# fails to represent is silently dropped from the user's file.

test_that("is_descendable only accepts fully named, non-empty lists", {
  expect_true(is_descendable(list(a = 1, b = 2)))

  expect_false(is_descendable(list()))
  expect_false(is_descendable(list(1, 2)))
  expect_false(is_descendable(list(a = 1, 2)))
  expect_false(is_descendable(NULL))
  expect_false(is_descendable(5))
  expect_false(is_descendable("text"))
})

test_that("flatten keeps value shapes that cannot be descended into", {
  nested <- list(
    data = list(
      specs = list(list(a = 1), list(b = 2)), # YAML sequence of mappings
      empty_map = list(),
      nothing = NULL,
      keep = 5L
    )
  )

  flat <- flatten_user_options(nested, leaf_set = character(0))

  expect_named(
    flat,
    c("data.specs", "data.empty_map", "data.nothing", "data.keep")
  )
  expect_equal(flat[["data.specs"]], list(list(a = 1), list(b = 2)))
  expect_equal(flat[["data.empty_map"]], list())
  expect_null(flat[["data.nothing"]])
  expect_equal(flat[["data.keep"]], 5L)
})

test_that("flatten and nest round trip without dropping keys", {
  nested <- list(
    general = list(name = "run", nothing = NULL),
    data = list(
      specs = list(list(a = 1)),
      empty_map = list(),
      columns = list(effect = list(source_name = "b"))
    )
  )

  restored <- flat_to_nested(
    flatten_user_options(nested, leaf_set = "data.columns")
  )

  expect_equal(restored, nested)
})

test_that("a template leaf is taken whole rather than descended into", {
  nested <- list(data = list(columns = list(
    effect = list(source_name = "b"),
    gdp = list(bma = TRUE)
  )))

  flat <- flatten_user_options(nested, leaf_set = "data.columns")

  expect_length(flat, 1L)
  expect_equal(flat[["data.columns"]], nested$data$columns)
})

test_that("flat_to_nested stores an explicit NULL instead of deleting the key", {
  restored <- flat_to_nested(list("a.b" = NULL, "a.c" = 1L))

  expect_named(restored$a, c("b", "c"))
  expect_null(restored$a$b)
})
