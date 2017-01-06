\name{PatternInfo}
\alias{PatternInfo}
\title{
Discover pattern information in one group of strings
}
\description{
PatternInfo discovers the start position of each pattern that occurs first or last
as well as the number of patterns in each string.
}
\usage{
PatternInfo(patterns, strings, rev = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{patterns}{
Pattern vector.
}
  \item{strings}{
String vector.
}
  \item{rev}{
Determine whether returning the start positions of patterns that occur first or last.
Default is first.
}
}
\value{
Returns a data frame, which contains the length of each string, and the starting position
 of each pattern in each string.
}

\seealso{
UniPatterns
}
\examples{
# simple strings and patterns
strs.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
patts <- c("ABC", "123")
PatternInfo(patts, strs.vec)

# simple strings and patterns, starting position of last pattern
strs.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
patts <- c("ABC", "123")
PatternInfo(patts, strs.vec, rev = TRUE)
}

\keyword{programming}
