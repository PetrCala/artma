% Generated manually to satisfy documentation requirements
\name{config.fix}
\alias{config.fix}
\title{Fix the data config}
\usage{
config.fix(options_file_name = NULL, options_dir = NULL)
}
\arguments{
\item{options_file_name}{Optional name of the options file to read the data configuration from. When \code{NULL}, the value of the \code{artma.data.config} option is used.}
\item{options_dir}{Optional directory that contains user options. If \code{NULL}, the current working directory is used.}
}
\value{A list containing the updated data configuration.}
\description{
Run the packaged fix-up routine that synchronises the stored data configuration with the current template defaults.
}
\seealso{\code{runtime_setup()}}
\keyword{internal}
