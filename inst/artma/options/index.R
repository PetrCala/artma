box::use(
  artma / options / template[parse_options_from_template],
  artma / options / user[load_user_options_file, create_user_options_file]
)

box::export(
  parse_options_from_template,
  load_user_options_file,
  create_user_options_file
)
