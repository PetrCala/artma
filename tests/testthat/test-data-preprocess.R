box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_true,
    expect_false,
    test_that
  ],
  withr[local_options]
)

test_that <- getFromNamespace("test_that", "testthat")
expect_equal <- getFromNamespace("expect_equal", "testthat")
expect_error <- getFromNamespace("expect_error", "testthat")
expect_true <- getFromNamespace("expect_true", "testthat")
expect_false <- getFromNamespace("expect_false", "testthat")

# Tests for remove_redundant_columns, verify_variable_names, and
# handle_extra_columns_with_data have been removed. Those functions were
# deleted and their responsibilities moved to inst/artma/data/schema_reconcile.R.
# See tests/testthat/test-data-schema-reconcile.R for the replacement tests.
