\name{system_dir}
\alias{system_dir}
\title{Safely execute system call from different directory}
\usage{
  system_dir(indir = NULL, command, arg = "", ...)
}
\arguments{
  \item{indir}{directory to execute from}

  \item{command}{to send to system see
  (\code{\link[base]{system2}})}

  \item{arg}{command arguments}

  \item{...}{other arguments passed to system}
}
\description{
  Safely execute system call from different directory
}
\details{
  see \code{\link[base]{system2}} and
  \code{\link[base]{system}}
}
\seealso{
  \code{\link[base]{system2}}
}

