\name{TransMx}
\alias{TransMx}
\title{
Transition matrices in one group of strings
}
\description{
TransMx discovers transition matrix of a string vector and the related information.

    A transition is defined as a substring (in the forward order) with length of 2 characters.
It can be considered as a special common pattern (length of 2).
}
\usage{
TransMx(strings.vec)
}
\arguments{
  \item{strings.vec}{
String Vector.

    If a string has fewer than 2 characters, that string will be ignored.
}
}

\value{
The function returns a list, which contains the transition matrix, the normalized matrix, and the sorted numbers of transitions.

}
\note{
Strings with less than 2 characters are not included due to the definition of transition.
}
\seealso{
\code{\link{TransInfo}}
}
\examples{
# simple strings
strs.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
TransMx(strs.vec)
}

\keyword{programming}
