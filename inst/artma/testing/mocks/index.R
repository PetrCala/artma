box::use(
  artma / paths[PATHS],
  artma / testing / load_test_modules[load_test_modules]
)

MOCKS <- load_test_modules(
  dir_path = PATHS$DIR_MOCKS,
  pattern = "^mock_.*\\.R$"
)

box::export(MOCKS)
