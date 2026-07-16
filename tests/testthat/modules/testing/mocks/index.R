# Aggregated mock helpers for the test suite. `create_mock_df` is production
# code (it powers the interactive "mock" file prompt) and is re-exported here so
# test call sites keep using `MOCKS$create_mock_df`.
box::use(
  artma / data / mock[create_mock_df],
  testing / mocks / mock_options[create_mock_options_colnames]
)

MOCKS <- list(
  create_mock_df = create_mock_df,
  create_mock_options_colnames = create_mock_options_colnames
)

box::export(MOCKS)
