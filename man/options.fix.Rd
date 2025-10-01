% Generated manually
\name{options.fix}
\alias{options.fix}
\title{Fix a user options file}
\usage{
options.fix(
  options_file_name = NULL,
  options_dir = NULL,
  template_path = NULL,
  force_default_overwrites = TRUE
)
}
\arguments{
\item{options_file_name}{Optional name of the options file to repair.}
\item{options_dir}{Optional directory that stores user options files.}
\item{template_path}{Optional path to the template YAML file.}
\item{force_default_overwrites}{Logical flag controlling whether missing values are overwritten with defaults without prompting.}
}
\value{Invisibly returns \code{NULL}.}
\description{
Validate an options file and attempt to repair issues by filling in template defaults and prompting for required values.
}
