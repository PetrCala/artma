options(
  # Make the package available to the box
  box.path = c(
    getOption("box.path"),
    paste(find.package("artma"), "R", sep = "/") # ../site-library/artma/R/
  )
)
