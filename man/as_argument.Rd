\name{as_argument}
\alias{as_argument}
\title{Transform arguments as a pairs of flag and values}
\usage{
  as_argument(flag = NA, arg = NA, expect = NA,
    spacer = " ", parrot = is.list(arg), mark = "-",
    mandatory = NA)
}
\arguments{
  \item{flag}{option parameter}

  \item{arg}{option arguments, provide empty string for no
  arguments}

  \item{spacer}{for parameters}

  \item{expect}{number of parameters to expect if given any
  (for required number of parameters use `mandatory`)}

  \item{mandatory}{number of mandatory parameters (for
  optional parameters use `expect`)}

  \item{parrot}{repeat flag for each list element ?}

  \item{mark}{flag marker}
}
\description{
  Transform arguments as a pairs of flag and values
}
\examples{
as_argument("merged", TRUE) #== "-merged"
as_argument("merged", FALSE) #== ""
as_argument("op", NA) #== ""
as_argument("op", "123")  #== "-op 123"
as_argument("op", c(1, 2, 3)) #== "-op 1 2 3"
as_argument("op", list(c(1, 2, 3), c(4, 5, 6)), parrot = TRUE) #== "-op 1 2 3 -op 4 5 6"
}

