\name{DupRm}
\alias{DupRm}
\title{
Removes successive duplicates in strings
}
\description{
DupRm removes successive duplicated characters in each string in a group.
}
\usage{
DupRm(strings.vec)
}
\arguments{
  \item{strings.vec}{
String Vector.
}
}
\value{
Returns a string vector with successive duplicates been removed.

    That is, each string in the export vector is "collapsed".
}

\examples{
# Simple example
dup1 <- "000<<<<<DDDFFF333333qqqqqKKKKK33FFF"
dup3 <- "aaBB111^^~~~555667777000000!!!###$$$$$$&&&(((((***)))))@@@@@>>>>99"
dup13 <- c(dup1, dup3)
DupRm(dup13)

}

\keyword{programming}
