box::use(
  artma / options / template[parse_options_from_template],
  artma / options / user[
    copy_user_options_file,
    delete_user_options_file,
    create_user_options_file,
    list_user_options_files,
    load_user_options
  ]
)

box::export(
  copy_user_options_file,
  delete_user_options_file,
  create_user_options_file,
  list_user_options_files,
  load_user_options,
  parse_options_from_template
)
