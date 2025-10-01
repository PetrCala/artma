% Generated manually
\name{main}
\alias{main}
\title{Run the artma main entry point}
\usage{
main(options = NULL, options_dir = NULL, FUN = NULL)
}
\arguments{
\item{options}{Optional name of the user options file to load before running the main routine.}
\item{options_dir}{Optional directory that stores user options files.}
\item{FUN}{Optional function to execute after the runtime setup completes. When omitted, a default informational function is used.}
}
\value{The return value of the \code{FUN} callback.}
\description{
Initialise the runtime environment (including options loading) and invoke the supplied callback.
}
