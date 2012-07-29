\name{progress_time}
\alias{progress_time}
\title{Progress bar with time remaining estimation}
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
