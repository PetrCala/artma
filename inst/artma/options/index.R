box::use(
  artma / options / template[parse_options_from_template],
  artma / options / public[
    create_user_options_file,
    list_user_options_files,
    load_user_options
  ]
)

box::export(
  create_user_options_file,
  list_user_options_files,
  load_user_options,
  parse_options_from_template
)
