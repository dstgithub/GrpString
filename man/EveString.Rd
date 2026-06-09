\name{EveString}
\alias{EveString}

\title{
Converts sequences of event names to strings - generalized
}
\description{
EveString converts event names in a data frame to a string vector.

    In the data frame, each row, which can have different number of 
event names, is converted to a string based on the conversion key. As
a result, in the vector, converted strings may have different lengths.
}
\usage{
EveString(eveName.file, eveName.vec, char.vec)
}
\arguments{
  \item{eveName.file}{
File that stores event names to be converted.
}
  \item{eveName.vec}{
Vector of event names in a conversion key.
}
  \item{char.vec}{
Characters vector in a conversion key.
}
}
\details{
In general, it is not convenient to deal with data frames where different rows have different
numbers of elements. Thus, it is easier to use a text file than to use a data frame when storing
different numbers of event names in rows. As a result, this function utilizes a .txt or .csv file 
(for eveName.file) and handles such task to save users' effort.
}
\value{
The function returns a vector containing converted strings that generally have different lengths.

If not all event names are converted to characters, a warning message will be printed out.
}

\note{
eveName.file is the name of a file. Thus quote signs are needed
when a file name (and its directory) is directly used in the function. 

If the example is used, the eveName.file will be eve1d.txt, which is located in your R library.
The users may copy eve1d.txt to a directory that can be easily found.
}

\seealso{
\code{\link{EveStr}}
}
\examples{
data(eventChar.df)
event1d <- paste(path.package("GrpString"), "/extdata/eve1d.txt", sep = "")
EveString(event1d, eventChar.df$event, eventChar.df$char)
}

\keyword{programming}
