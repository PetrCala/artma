current_box_path <- getOption("box.path", character(0))

new_path <- paste(find.package("artma"), "R", sep = "/") # ../site-library/artma/R/

if (!any(grepl("artma/R$", current_box_path))) {
  # Make the package available to the box
  options(
    box.path = c(current_box_path, new_path)
  )
}
