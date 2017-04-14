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
TransMx(strings.vec, indiv = FALSE)
}
\arguments{
  \item{strings.vec}{
String Vector.

    If a string has fewer than 2 characters, that string will be ignored.
}
  \item{indiv}{
Whether exports transition matrix for each string into the current directory. Default value is FALSE.
}
}

\value{
The function returns a list, which contains the transition matrix, the normalized matrix, and the sorted numbers of transitions.

    If indiv = TRUE, a set of mx.txt files are exported into the current directory that contain transition matrix for each string. The names of these files are the name of strings.vec appended with the orders of the strings in the string vector. If a string has fewer than 2 characters, the corresponding mx.txt file will be skipped.
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

# simple strings, export an individual transition matrix for each string
strs.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
TransMx(strs.vec, indiv = TRUE)
}

\keyword{programming}
