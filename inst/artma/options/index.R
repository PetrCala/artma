box::use(
  artma / options / template[parse_options_from_template],
  artma / options / user[
    create_user_options_file,
    load_user_options,
    options.list
  ]
)

box::export(
  create_user_options_file,
  load_user_options,
  options.list,
  parse_options_from_template
)
