# Aggregated test fixtures, exposed to test files as `FIXTURES$<name>`.
box::use(
  testing / fixtures / helper_colnames[with_custom_colnames],
  testing / fixtures / helper_test_cache[local_cli_silence, make_fake_modeller]
)

FIXTURES <- list(
  with_custom_colnames = with_custom_colnames,
  local_cli_silence = local_cli_silence,
  make_fake_modeller = make_fake_modeller
)

box::export(FIXTURES)
