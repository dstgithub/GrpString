\name{CommonPatt}
\alias{CommonPatt}
\title{
Discovers common patterns in a group of strings - simplified version
}
\description{
CommonPatt finds common patterns shared by a group of strings.

    A common pattern is defined as a substring with the minimum length of three
that occurs at least twice among a group of strings.
}
\usage{
CommonPatt(strings.vec, low = 10)
}
\arguments{
  \item{strings.vec}{
String Vector.
}
  \item{low}{
Cutoff. It is the minimum percentage of the occurrence of patterns that the user specifies.
The default value is 10.
}
}
\details{
The argument 'low' ranges from 0 to 100 in percentage.
}
\value{
The function returns a data frame containing patterns, lengths and percentages of patterns.

    row name - The initial order of substrings, which can be ignored.
    
    Column 1 - Pattern: common pattern.
    
    Column 2 - Freq_total: the overall frequency (times of occurrence) of each pattern.
    
    Column 3 - Percent_total: the ratio of Freq_total to the number of original strings, in percent. 
    
    Column 4 - Length: the length (i.e., number of characters) of pattern.
    
    Column 5 - Freq_str: similar to Freq_total; but each pattern is counted only once in a string even if the string contains that pattern multiple times.
    
    Column 6 - Percent_str: similar to Percent; but each pattern is counted only once in a string if this string contains the pattern.
    
    Data is sorted by Length, then Freq_total, in decreasing order.
}
\references{
1. H. Tang; E. Day; L. Kendhammer; J. N. Moore; S. A. Brown; N. J. Pienta. (2016). Eye movement patterns in solving science ordering problems. Journal of eye movement research, 9(3), 1-13.

    2. J. J. Topczewski; A. M. Topczewski; H. Tang; L. Kendhammer; N. J. Pienta.(2017). NMR Spectra through the eyes of a student: eye tracking applied to NMR items. Journal of chemical education, 94(1), 29-37.
    
    3. J. M. West; A. H. Haake; E. P. Rozanksi; K. S. Karn. (2006). EyePatterns: Software for identifying patterns and similarities across fixation sequences. In Proceedings of the Symposium on Eye-tracking Research & Applications, ACM Press, New York, 149-154.
}

\seealso{
\code{\link{CommonPattern}}
}
\examples{
# Simple strings, non-default cutoff
strs.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
CommonPatt(strs.vec, low = 30)
}

\keyword{programming}
