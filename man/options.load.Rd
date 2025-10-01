% Generated manually
\name{options.load}
\alias{options.load}
\title{Load user options}
\usage{
options.load(
  options_file_name = NULL,
  options_dir = NULL,
  create_options_if_null = TRUE,
  load_with_prefix = TRUE,
  template_path = NULL,
  should_validate = TRUE,
  should_set_to_namespace = FALSE,
  should_add_temp_options = FALSE,
  should_return = TRUE
)
}
\arguments{
\item{options_file_name}{Optional name of the options file (including suffix) to load.}
\item{options_dir}{Optional directory containing user options files.}
\item{create_options_if_null}{Logical flag; if \code{TRUE}, a new options file may be created when none exist.}
\item{load_with_prefix}{Logical flag indicating whether loaded options should be prefixed with the package name.}
\item{template_path}{Optional path to the options template YAML file.}
\item{should_validate}{Logical flag controlling whether the loaded options are validated.}
\item{should_set_to_namespace}{Logical flag controlling whether the options are set via \code{options()}.}
\item{should_add_temp_options}{Logical flag controlling whether temporary helper entries are attached to the result.}
\item{should_return}{Logical flag; if \code{TRUE}, the flattened options are returned.}
}
\value{A named list of options when \code{should_return} is \code{TRUE}, otherwise invisibly \code{NULL}.}
\description{
Load, optionally validate, and optionally register a user options file, prompting for input when necessary.
}
