\name{CommonPattern}
\alias{CommonPattern}
\title{
Discover common patterns in a group of strings - full version
}
\description{
CommonPattern finds common patterns shared by a group of strings.

    It is the extended version of CommonPatt and the ursers have more options.
    
    A common pattern is defined as a substring with the minimum length of three
that occurs at least twice among a group of strings.
}
\usage{
CommonPattern(strings.vec, low = 5, high = 25, interval = 5, eveChar.df)
}
\arguments{
  \item{strings.vec}{
String vector.
}
  \item{low}{
The lowest cutoff, which is the minimum percentage of the occurance of patterns that the user specifies.
The default value is 5.
}
  \item{high}{
The highest cutoff, which is the maxmum percentage of the occurance of patterns that the user specifies.
The default value is 25.
}
  \item{interval}{
The increment percentage from the lowest to the highest cutoff.
The default value is 5.
}
  \item{eveChar.df}{
Data frame that stores the event name-character conversion key (optional).
}
}
\details{
The arguments 'low', 'high' and 'interval' range from 0 to 100 in percentage.
}
\value{
Exports in the current directory a set of text files containing patterns, lengths, percentages of patterns,
and converted event names if eveChar.fle is included. The names of these files are the name of strings.vec appended with
the percentages. In addtion, a file with all patterns that occurred at least 2 times as well as a file with all possible substrings (including the original strings themselves) are exported. 
}
\references{
1. H. Tang; E. Day; L. Kendhammer; J. N. Moore; S. A. Brown; N. J. Pienta. (2016). Eye movement patterns in solving science ordering problems. Journal of eye movement research, 9(3), 1-13.

    2. J. J. Topczewski; A. M. Topczewski; H. Tang; L. Kendhammer; N. J. Pienta.(2016). NMR Spectra through the eyes of a student: eye tracking applied to NMR items. Journal of chemical education.
    
    3. J. M. West; A. H. Haake; E. P. Rozanksi; K. S. Karn. (2006). EyePatterns: Software for identifying patterns and similarities across fixation sequences. In Proceedings of the Symposium on Eye-tracking Research & Applications, ACM Press, New York, 149-154.
}
\note{
    The time to run this function can be relativle long (from seconds to mimutes depending on the number and lengths of strings as well as the performance of computers).
}
\seealso{
CommonPatt
}
\examples{
# simple strings
strs.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
CommonPattern(strs.vec, low = 30, high = 50, interval = 20)

# None-default cutoff values, with conversion back
data(eventChar.df)
data(str1)
s0 <- str1[5:15]
CommonPattern(s0, low = 20, high = 30, interval = 10, eveChar.df = eventChar.df)
}

\keyword{programming}
