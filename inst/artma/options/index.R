box::use(
  artma / options / utils[
    get_option_group
  ],
  artma / options / template[
    flatten_template_options,
    parse_options_from_template
  ]
)

box::export(
  flatten_template_options,
  get_option_group,
  parse_options_from_template
)
