\name{ANT2}
\alias{ANT2}
\docType{data}
\title{Messy ANT data}
\description{
A \dQuote{messy} version of the ANT data set (see \code{\link{ANT}}). In this version of the data, subnum #7 is missing data from the last half of the experiment, subnum #14 made all errors in the incongruent cells, and subnum #12 mistakenly reversed their responses.
}
\usage{data(ANT2)}
\author{
Michael A. Lawrence \email{Mike.Lawrence@dal.ca}\cr
Visit the \code{ez} development site at \url{http://github.com/mike-lawrence/ez}\cr
for the bug/issue tracker and the link to the mailing list.
}
\examples{
data(ANT2)
head(ANT2)
ezPrecis(ANT2)
}
\keyword{datasets}
