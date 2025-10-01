% Generated manually
\name{run}
\alias{run}
\title{Run artma runtime methods}
\usage{
run(methods = NULL, options_file_name = NULL, options_dir = NULL)
}
\arguments{
\item{methods}{Optional character vector naming runtime methods to invoke. When \code{NULL}, the user is prompted to choose.}
\item{options_file_name}{Optional name of the options file to load before running.}
\item{options_dir}{Optional directory containing user options files.}
}
\value{A named list containing the results produced by each runtime method.}
\description{
Load the requested options, prepare the dataset, and invoke the selected runtime methods in sequence.
}
