\name{UniPatterns}
\alias{UniPatterns}
\title{
Discovers unique patterns in two groups of strings
}
\description{
UniPatterns discovers "unique" patterns that are in one group of strings but not the other.
}
\usage{
UniPatterns(grp1_pattern, grp2_pattern, grp1_string, grp2_string)
}
\arguments{
  \item{grp1_pattern}{
Patterns shared by a certain percent of strings in string group 1.
}
  \item{grp2_pattern}{
Patterns shared by a certain percent of strings in string group 2.
}
  \item{grp1_string}{
String group 1.
}
  \item{grp2_string}{
String group 2.
}
}
\details{
A (common) pattern is defined as a substring with the minimum length of three
that occurs at least twice among a group of strings.

    A unique pattern is a pattern that appears in only one of the two groups of strings.
}
\value{
The function exports a data frame that lists unique patterns: column 1 for string group 1; column 2 for string group 2.
}

\seealso{
\code{\link{PatternInfo}},
\code{\link{CommonPatt}},
\code{\link{CommonPattern}}
}
\examples{
data(str1)
data(str2)
data(p1_20up)
data(p2_25up)
UniPatterns(p1_20up, p2_25up, str1, str2)
}

\keyword{programming}
