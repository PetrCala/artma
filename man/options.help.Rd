% Generated manually
\name{options.help}
\alias{options.help}
\title{Display help for user options}
\usage{
options.help(options = NULL, template_path = NULL)
}
\arguments{
\item{options}{Optional character vector of option names to explain. When \code{NULL}, information about every option is printed.}
\item{template_path}{Optional path to the options template YAML file.}
}
\value{Invisibly returns \code{NULL} after printing help text to the console.}
\description{
Print descriptions, default values, and expected types for selected options based on the options template.
}
