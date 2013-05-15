\name{system_dir}
\alias{system_dir}
\title{Safely execute system from different directory}
\usage{
  system_dir(indir = NULL, command, ...)
}
\arguments{
  \item{indir}{directory to execute from}

  \item{command}{comand to send to system}

  \item{...}{other arguments passed to system}
}
\description{
  Safely execute system from different directory
}
\details{
  see \code{\link[base]{system2}} and
  \code{\link[base]{system}}
}

