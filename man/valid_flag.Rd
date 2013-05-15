\name{valid_flag}
\alias{valid_flag}
\alias{vf}
\title{Validate option flag}
\usage{
  valid_flag(flag, mark = "-")
}
\arguments{
  \item{flag}{character}

  \item{mark}{flag marker}
}
\description{
  Validate option flag
}
\examples{
valid_flag("op")
valid_flag("-op")
valid_flag(" -op ")
valid_flag(" op   ")
valid_flag("-op   ")
# long options
valid_flag("h", mark = "--")
}

