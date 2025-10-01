% Generated manually
\name{options.create}
\alias{options.create}
\title{Create a user options file}
\usage{
options.create(
  options_file_name = NULL,
  options_dir = NULL,
  template_path = NULL,
  user_input = list(),
  should_validate = TRUE,
  should_overwrite = FALSE,
  action_name = "creating"
)
}
\arguments{
\item{options_file_name}{Optional file name (including suffix) for the new options file.}
\item{options_dir}{Optional directory in which to place the options file.}
\item{template_path}{Optional path to the options template YAML file.}
\item{user_input}{Optional named list of option overrides supplied programmatically.}
\item{should_validate}{Logical flag indicating whether the generated file should be validated against the template.}
\item{should_overwrite}{Logical flag controlling whether an existing file is overwritten automatically.}
\item{action_name}{Label describing the operation, used only for user-facing messages.}
}
\value{Invisibly returns the name of the created options file.}
\description{
Create or overwrite an options file by merging template defaults with user-supplied values and, optionally, validating the result.
}
