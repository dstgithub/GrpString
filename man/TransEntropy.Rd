\name{TransEntropy}
\alias{TransEntropy}
\title{
Transition entropy of each string in a group
}
\description{
TransEntropy computes the transition entropy of each of the strings in a group.

}
\usage{
TransEntropy(strings.vec)
}
\arguments{
  \item{strings.vec}{
String Vector.
}
}

\value{
Returns a number vector.
}
\details{
Entropy is calculated using the Shannon entropy formula: -sum(freqs * log2(freqs)). Here, freqs are transition frequencies, which are the values in the normalized transition matrix exported by function TransMx in this package. The formula is equivalent to the function entropy.empirical in the entropy package when unit is set to log2.
}
\note{
Strings with less than 2 characters are not included for computation of entropy.
}
\references{
I. Hooge; G. Camps. (2013) Scan path entropy and arrow plots: capturing scanning behavior of multiple observers. Frontiers in Psychology.
}
\seealso{
\code{\link{TransEntro}}, 
\code{\link{TransMx}}
}
\examples{
# default values
stra.vec <- c("ABCDdefABCDa", "def123DC", "A", "123aABCD", "ACD13", "AC1ABC", "3123fe")
TransEntropy(stra.vec)

}

\keyword{programming}
