% Generated manually
\name{options.copy}
\alias{options.copy}
\title{Copy user options}
\usage{
options.copy(
  options_file_name_from = NULL,
  options_file_name_to = NULL,
  options_dir = NULL,
  should_overwrite = NULL
)
}
\arguments{
\item{options_file_name_from}{Optional name of the options file to copy from. When omitted, an interactive prompt is shown.}
\item{options_file_name_to}{Optional destination file name. When omitted, the user is prompted for a name.}
\item{options_dir}{Optional directory that stores user options files.}
\item{should_overwrite}{Optional logical flag controlling whether existing files are overwritten without prompting.}
}
\value{Invisibly returns \code{NULL}.}
\description{
Copy an existing user options file to a new location or name, prompting where required.
}
