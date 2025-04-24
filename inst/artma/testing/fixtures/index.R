box::use(
  artma / paths[PATHS],
  artma / testing / load_test_modules[load_test_modules]
)

FIXTURES <- load_test_modules(
  dir_path = PATHS$DIR_FIXTURES,
  pattern = "^fixture_.*\\.R$"
)

box::export(FIXTURES)
