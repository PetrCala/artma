% Generated manually
\name{options.modify}
\alias{options.modify}
\title{Modify a user options file}
\usage{
options.modify(
  options_file_name = NULL,
  options_dir = NULL,
  template_path = NULL,
  user_input = list(),
  should_validate = TRUE
)
}
\arguments{
\item{options_file_name}{Optional name of the options file to modify.}
\item{options_dir}{Optional directory that stores user options files.}
\item{template_path}{Optional path to the options template YAML file.}
\item{user_input}{Optional named list of option values to merge into the file.}
\item{should_validate}{Logical flag indicating whether the updated file should be validated.}
}
\value{Invisibly returns \code{NULL}.}
\description{
Load an options file, merge user-supplied changes, and save the result, prompting when interaction is required.
}
