% Generated manually
\name{options.delete}
\alias{options.delete}
\title{Delete user options files}
\usage{
options.delete(
  options_file_name = NULL,
  options_dir = NULL,
  skip_confirmation = FALSE
)
}
\arguments{
\item{options_file_name}{Optional name of one or more options files to delete. When omitted, an interactive prompt is shown.}
\item{options_dir}{Optional directory containing user options files.}
\item{skip_confirmation}{Logical flag indicating whether the deletion prompt should be skipped.}
}
\value{Invisibly returns \code{NULL}.}
\description{
Delete one or more user options files after optionally confirming the action with the user.
}
