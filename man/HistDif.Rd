\name{HistDif}
\alias{HistDif}
\title{
Customize the positions of legend and p value in a histogram
}
\description{
The positions of legend and p value in the histogram generated from function StrDif may not be ideal
for different (permutations on differences of normalized Levenstein distances) situations.
HistDif customizes the positions of legend and p value in the histogram of the statistical difference
of two groups of strings.
}
\usage{
HistDif(dif.vec, obsDif, pvalue, o.x = 0.01, o.y = 0, p.x = 0.015, p.y = 0)
}

\arguments{
  \item{dif.vec}{
Vector containing differences of normalized Levenstein differences (LD) from the permutation test.
}
  \item{obsDif}{
The "observed" or original difference between between-group and within-group normalized LD.
}
  \item{pvalue}{
p value of the permutation test.
}
  \item{o.x}{
x coordinate of the legend in the histogram, default is 0.01.
}
  \item{o.y}{
y coordinate of the legend in the histogram, default is 0.
}
  \item{p.x}{
x coordinate of the p value in the histogram, default is 0.015.
}
  \item{p.y}{
y coordinate of the p value in the histogram, default is 0.
}
}
\details{
The default values of o.y and p.y are 0. They are actually related to num_perm:  o.y is above 0.2 * num_perm, 
and p.y is below 0.2 * num_perm. If non-default values are used, the values become absolute y coordinates.
}

\seealso{
StrDif
}
\examples{
# simple example, use the vectors of ld difference values obtained from StrDifStat
strs1.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
strs2.vec <- c("xYZdkfAxDa", "ef1563xy", "BC9Dzy35X", "AkeC1fxz", "65CyAdC", "Dfy3f69k")
ld.dif.vec <- StrDif(strs1.vec, strs2.vec, num_perm = 500, p.x = 0.025)
HistDif(dif.vec = ld.dif.vec, obsDif = 0.00751, pvalue = 0.35600, 
        o.x = 0.025, p.x = 0.040, p.y = 75)
}

\keyword{programming}
