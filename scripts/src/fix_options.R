devtools::load_all()
default_options_file_path <- file.path("inst", "artma", "options", "templates")

print(glue::glue("Re-creating the default options .yaml file under { default_options_file_path}..."))
artma::options.create(
  options_name = "options_default",
  options_dir = default_options_file_path
)
print("Done.")
