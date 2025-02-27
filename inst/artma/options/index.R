box::use(
  artma / options / template[parse_options_from_template],
  artma / options / user[load_user_options, create_user_options_file]
)

box::export(
  create_user_options_file,
  load_user_options,
  parse_options_from_template
)
