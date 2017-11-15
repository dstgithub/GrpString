\name{EveS}
\alias{EveS}
\title{
Converts sequence of event names to a string
}
\description{
EveS converts event names in a vector to a single string based on the conversion key.
}
\usage{
EveS(eves.vec, eveName.vec, char.vec)
}
\arguments{
  \item{eves.vec}{
Vector that stores event names to be converted.
}
  \item{eveName.vec}{
Event name vector in a conversion key.
}
  \item{char.vec}{
Character vector in a conversion key.
}
}
\details{
The lengths of eveName.vec and char.vec are the same.

    Each element (event name) in eveName.vec corresponds to an element (character) in char.vec.
    
    An element in char.vec can be a letter, digit, or a special character.
}
\value{
The function returns a string.
}

\seealso{
\code{\link{EveStr}}, 
\code{\link{EveString}}
}
\examples{
event.vec <- c("aoi_1", "aoi_2", "aoi_3", "aoi_2", "aoi_1")
eve.names <- c("aoi_1", "aoi_2", "aoi_3")
labels <- c("a", "b", "c")
EveS(event.vec, eve.names, labels)

}

\keyword{programming}
