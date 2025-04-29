box::use(
  artma / testing / mocks / index[MOCKS],
  artma / testing / fixtures / index[FIXTURES]
)


# For creating a temporary file
# path <- withr::local_tempfile(
#   pattern = "Universit\u00e0-",
#   lines = c("#' @include foo.R", NULL)
# )

# For setting options
# withr::local_options(width = 20) # <-- (°_°) look here!

# To create a useful thing
# local_useful_thing <- function(..., env = parent.frame()) {
#   # your fiddly code to create a useful_thing goes here
#   withr::defer(
#     # your fiddly code to clean up after a useful_thing goes here
#     envir = env
#   )
# }
