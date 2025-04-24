test_that("preprocess_data works", {
  withr::local_options(width = 20) # <-- (°_°) look here!

  df <- MOCKS$create_mock_df()

  expect_equal(class(df), "data.frame")
})
