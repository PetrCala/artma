% Generated manually
\name{options.validate}
\alias{options.validate}
\title{Validate a user options file}
\usage{
options.validate(
  options_file_name = NULL,
  options_dir = NULL,
  should_flag_redundant = FALSE,
  template_path = NULL,
  failure_action = "abort_verbose"
)
}
\arguments{
\item{options_file_name}{Optional name of the options file to validate.}
\item{options_dir}{Optional directory containing user options files.}
\item{should_flag_redundant}{Logical flag controlling whether unknown options are reported.}
\item{template_path}{Optional path to the options template YAML file.}
\item{failure_action}{Strategy to use when validation fails. Accepted values mirror the function documentation.}
}
\value{Invisibly returns a list of validation errors.}
\description{
Validate a user options file against the package template, checking for missing entries and type mismatches.
}
