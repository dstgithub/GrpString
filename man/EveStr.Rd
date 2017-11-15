\name{EveStr}
\alias{EveStr}
\title{
Converts sequences of event names to strings - same length
}
\description{
EveStr converts event names in a data frame to a string vector.
In the data frame, each row, which has the same number of event names,
is converted to a string based on the conversion key. A string vector is exported.
As a result, in the vector, each converted string has the same length.
}
\usage{
EveStr(eveName.df, eveName.vec, char.vec)
}
\arguments{
  \item{eveName.df}{
Data frame that stores event names to be converted.
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
The function returns a string vector.
}

\seealso{
\code{\link{EveS}}, 
\code{\link{EveString}}
}
\examples{
# small number of event names
event.df <- data.frame(c("aoi_1", "aoi_2"),
                     c("aoi_1", "aoi_3"),
                     c("aoi_3", "aoi_5"))
event.name.vec <- c("aoi_1", "aoi_2", "aoi_3", "aoi_4", "aoi_5")
label.vec <- c("a", "b", "c", "d", "e")
EveStr(event.df, event.name.vec, label.vec)

# more event names
data(event1s.df) 
data(eventChar.df)
EveStr(event1s.df, eventChar.df$event, eventChar.df$char)
}

\keyword{programming}
