\name{CommonPattern}
\alias{CommonPattern}
\title{
Discovers common patterns in a group of strings - full version
}
\description{
CommonPattern finds common patterns shared by a group of strings.

    It is the extended version of CommonPatt and the users have more options.
    
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
The lowest cutoff. It is the minimum percentage of the occurrence of patterns that the user specifies.
The default value is 5.
}
  \item{high}{
The highest cutoff, which is the maximum percentage of the occurrence of patterns that the user specifies.
The default value is 25.
}
  \item{interval}{
The increment percentage from the lowest to the highest cutoff.
The default value is 5.
}
  \item{eveChar.df}{
Data frame that stores the event name - character conversion key (optional).
}
}
\details{
The arguments 'low', 'high' and 'interval' range from 0 to 100 in percentage.
}
\value{
A set of .txt files are exported into the current directory that contain patterns, lengths, percents of patterns,
and converted event names if eveChar.fle is included. The names of these files are the name of strings.vec appended with
the percentages. In addition, a file with all patterns that occurred at least 2 times is exported. 

    row name - The initial order of substrings, which can be ignored.
    
    Column 1 - Pattern: common pattern.
    
    Column 2 - Freq_total: the overall frequency (times of occurrence) of each pattern.
    
    Column 3 - Percent_total: the ratio of Freq_total to the number of original strings, in percent. 
    
    Column 4 - Length: the length (i.e., number of characters) of pattern.
    
    Column 5 - Freq_str: similar to Freq_total; but each pattern is counted only once in a string even if the string contains that pattern multiple times.
    
    Column 6 - Percent_str: similar to Percent; but each pattern is counted only once in a string if this string contains the pattern.

    Column 7 - Event_name (optional): sequence of event names converted back from pattern string 
    
    Data is sorted by Length, then Freq_total, in decreasing order.
}
\note{
The time to run this function can be relatively long (from seconds to minutes depending on the number and lengths of strings as well as the performance of computers).
}
\references{
1. H. Tang; E. Day; L. Kendhammer; J. N. Moore; S. A. Brown; N. J. Pienta. (2016). Eye movement patterns in solving science ordering problems. Journal of eye movement research, 9(3), 1-13.

    2. J. J. Topczewski; A. M. Topczewski; H. Tang; L. Kendhammer; N. J. Pienta.(2017). NMR Spectra through the eyes of a student: eye tracking applied to NMR items. Journal of chemical education, 94(1), 29-37.
    
    3. J. M. West; A. H. Haake; E. P. Rozanksi; K. S. Karn. (2006). EyePatterns: Software for identifying patterns and similarities across fixation sequences. In Proceedings of the Symposium on Eye-tracking Research & Applications, ACM Press, New York, 149-154.
}
\seealso{
\code{\link{CommonPatt}}
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
