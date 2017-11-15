\name{StrKclust}
\alias{StrKclust}
\title{
K-means clustering of a group of strings
}
\description{
StrKclust discovers clusters of the strings in a group.

}
\usage{
StrKclust(strings.vec, nclust = 2, nstart = 1, shade = FALSE)
}
\arguments{
  \item{strings.vec}{
String Vector.
}
  \item{nclust}{
Number of clusters. Default is 2.
}
  \item{nstart}{
Number of random data sets chosen to start. Default is 1.
}
  \item{shade}{
Whether shade the ellipses in the cluster plot or not. Default is false.
}
}

\value{
Returns a data frame with the specific cluster assigned to each string.

    A cluster plot is also exported.
}

\seealso{
\code{\link{StrHclust}}
}
\examples{
# Simple strings
strs3.vec <- c("ABCDdefABCDa", "AC3aABCD", "ACD1AB3", "xYZfgAxZY", "gf56xZYx", "AkfxzYZg")
StrKclust(strs3.vec)

}

\keyword{programming}
