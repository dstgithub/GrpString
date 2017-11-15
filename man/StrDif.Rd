\name{StrDif}
\alias{StrDif}
\title{
Statistically compares the difference between two groups of strings
}
\description{
StrDif tests whether the difference between two groups of strings is statistically significant or not.
The difference is based on normalized Levenshtein distances (LDs) between strings. A permutation test is used 
as the statistical method.
}
\usage{
StrDif(grp1_string, grp2_string, num_perm = 1000,
       o.x = 0.01, o.y = 0, p.x = 0.015, p.y = 0)
}

\arguments{
  \item{grp1_string}{
String group (vector) 1.
}
  \item{grp2_string}{
String group  (vector) 2.
}
  \item{num_perm}{
Number of permutations. The default is 1000.
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
y coordinate of the legend in the histogram, default is 0.
}
}
\details{
The default values of o.y and p.y are 0. They are actually related to num_perm:  o.y is above 0.2 * num_perm, and p.y is below 0.2 * num_perm. If non-default values are used, the values become absolute y coordinates.
}
\value{
The function generates a histogram that demonstrates the distribution of the differences of LDs, the original difference, and the p value.

    The function also returns a vector containing differences of normalized LDs. 
The total number of differences is num_perm (number of permutations).

    Differences are calculated by subtracting within-group LD from between-group LD. They range from -1 to 1.
    The "observed" difference is the difference from the original data set. 
}
\references{
1. H. Tang; J. J. Topczewski; A. M. Topczewski; N. J. Pienta.
Permutation Test for Groups of Scanpaths Using Normalized
Levenshtein Distances and Application in NMR Questions. In
Proceedings of the Symposium on Eye Tracking Research and Applications,
Santa Barbara, CA, March 28-30, 2012; ACM Press: New York; pp 169-172.

    2. M. Feusner; B. Lukoff. (2008). Testing for statistically significant differences between groups of scan patterns. 
In Proceedings of the Symposium on Eye-tracking Research & Applications, ACM Press, New York, 43-46.
}
\note{
1. Because the number of permutations is usually large (default is 1000), and so
is the number of elements in the vector returned from the function,
it's better for the user to use a vector to store the returned results,
instead of printing out directly. See the examples. 

    2. The positions of legend and p value in the histogram generated from function StrDif may not be ideal
for different (permutations on differences of normalized Levenshtein distances) situations.
Thus, this package includes another function, HistDif, to customize the positions of legend and p value in the histogram.

    3. The time to run this function can be relatively long (from seconds to minutes depending on the number and lengths of strings as well as the computer performance).

    4. Acknowledgement: The first version of this function was developed with significant help from Dr. Rhonda DeCook 
in the Department of Statistics and Actuarial Science at the University of Iowa.
}

\seealso{
\code{\link{HistDif}}
}
\examples{
# simple stings, non-default permutation number and p-value position
strs1.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
strs2.vec <- c("xYZdkfAxDa", "ef1563xy", "BC9Dzy35X", "AkeC1fxz", "65CyAdC", "Dfy3f69k")
ld.dif.vec <- StrDif(strs1.vec, strs2.vec, num_perm = 500, p.x = 0.025)

# longer strings
data(str1)
data(str2)
s1 <- str1[1:6]
s2 <- str2[1:6]
ld.dif12.vec <- StrDif(s1, s2, num_perm = 500)
}

\keyword{programming}
