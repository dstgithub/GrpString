\name{TransInfo}
\alias{TransInfo}
\title{
Transitions in one group of strings
}
\description{
TransInfo discovers transitions of two adjacent characters in strings.

    A transition is defined as a substring (in the forward order) with length of 2 characters.
It can be considered as a special common pattern (length of 2).
}
\usage{
TransInfo(strings.vec, type1 = "letters", type2 = "digits")
}
\arguments{
  \item{strings.vec}{
String Vector.
}
  \item{type1}{
The first type of transition. Default value is letter.
}
  \item{type2}{
The second type of transition. Default value is digit.
}
}

\value{
The function returns a data frame, which contains the numbers of type1 transition, type2 transition, and transitions
belonging to neither type1 nor type2.
}
\note{
Strings with less than 2 characters are not included due to the definition of transition.
}
\references{
1. H. Tang; E. Day; L. Kendhammer; J. N. Moore; S. A. Brown; N. J. Pienta. (2016) Eye movement patterns in solving science ordering problems. Journal of eye movement research, 9(3), 1-13.  

    2. J. J. Topczewski; A. M. Topczewski; H. Tang; L. Kendhammer; N. J. Pienta.(2017) NMR Spectra through the eyes of a student: eye tracking applied to NMR items. Journal of chemical education, 94(1), 29-37.
}
\seealso{
\code{\link{TransMx}}
}
\examples{
# default values
strs.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
TransInfo(strs.vec)

# non-default values
str1.vec <- c("ABCABEF", "CDCDAB")
TransInfo(str1.vec, type1 = "AB", type2 = "CD")
}

\keyword{programming}
