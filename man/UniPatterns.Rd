\name{UniPatterns}
\alias{UniPatterns}
\title{
Discover unique patterns in two groups of strings
}
\description{
UniPatterns discovers "unique" patterns that are in one group of strings but not the other.
}
\usage{
UniPatterns(grp1_pattern, grp2_pattern, grp1_string, grp2_string)
}
\arguments{
  \item{grp1_pattern}{
Patterns shared by a centain percent of strings in string group 1.
}
  \item{grp2_pattern}{
Patterns shared by a centain percent of strings in string group 2
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

    A strict definition of unique pattern is that a unique patternis is a pattern that appears in only one of the two groups of strings.
However, in practice, a pattern usually is not shared by all the strings in a group.
Thus, unique patterns may be obtained from two pattern vectors, each of which contains patterns that are shared by a centain percent of strings in a group.
As a result, "unique patterns" can possibly appear in both groups of strings.
}
\value{
Exports five text files:

    File that lists unique patterns: column 1 for string group 1; colum 2 for string group 2.
    
    Four files that contain information about each group of patterns in each group of strings.
    
    The information includes the number of each of the patterns in each string and the starting
    
positions of the first occurring patterns, as well as the lengths of original strings.

    If a pattern does not appear in a string, -1 is returned.
    
    In the above four files: the first column contains original strings;
                the second column contains the length of strings;
                the third column contains the number of unique patterns each string has
                each of the column from the fourth is the starting position of a pattern that first appears in a string.
                
    In addition, messages are printed out for the four situations of each pattern group in each string group.
The messages include the number and the ratio of strings that have at least one unique pattern.
}

\seealso{
PatternInfo, CommonPatt, CommonPattern
}
\examples{
data(str1)
data(str2)
data(p1_20up)
data(p2_25up)
UniPatterns(p1_20up, p2_25up, str1, str2)
}

\keyword{programming}
