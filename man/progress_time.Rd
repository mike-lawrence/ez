\name{progress_time}
\Rdversion{1.1}
\alias{progress_time}
\title{
A progress bar with time remaining estimation
}
\description{
This function is intended to be used with the various plyr functions. See examples below.
}
\usage{
progress_time()
}
\author{
Michael A. Lawrence \email{Mike.Lawrence@dal.ca}
To report bugs or request features, please visit: \url{https://github.com/mike-lawrence/ez/issues}
To keep up to date on developments related to this package, join the discussion group at: \url{http://groups.google.com/group/ez4r}
}
\seealso{
    \code{\link{ANT}}, \code{\link{ANT2}}, \code{\link{ezANOVA}}, \code{\link{ezBoot}}, \code{\link{ezBootPlot}}, \code{\link{ezCor}}, \code{\link{ezDesign}}, \code{\link{ezMixed}}, \code{link{ezMixedRel}}, \code{\link{ezPerm}}, \code{\link{ezPlot}}, \code{\link{ezPrecis}}, \code{\link{ezPredict}}, \code{\link{ezResample}}, \code{\link{ezStats}}, \code{\link{progress_time}}, \code{\link{progress_timeCI}}
}
\examples{

\dontrun{
l_ply(
	.data = 1:1e3
	, .fun = function(x){
		Sys.sleep(1)
	}
	, .progress = 'time'
)
}

}
