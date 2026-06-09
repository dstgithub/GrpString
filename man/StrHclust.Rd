\name{StrHclust}
\alias{StrHclust}
\title{
Hierarchical cluster of a group of strings
}
\description{
StrHclust discovers clusters of the strings in a group.

}
\usage{
StrHclust(strings.vec, nclust = 2)
}
\arguments{
  \item{strings.vec}{
String Vector.
}
  \item{nclust}{
Number of clusters. Default is 2.
}
}

\value{
Returns a data frame with the specific cluster assigned to each string.

    A Hierarchical dendrogram is also exported.
}

\seealso{
\code{\link{StrKclust}}
}
\examples{
# Simple strings
strs3.vec <- c("ABCDdefABCDa", "AC3aABCD", "ACD1AB3", "xYZfgAxZY", "gf56xZYx", "AkfxzYZg")
StrHclust(strs3.vec)

}

\keyword{programming}
